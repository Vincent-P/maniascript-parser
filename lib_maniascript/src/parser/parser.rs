use std::{collections::VecDeque, fmt};

use cbitset::BitSet256;
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language, SmolStr};

use rowan::TextRange;

use crate::parser::{
    language::{MsLanguage, SyntaxElement, SyntaxNode, SyntaxToken},
    typed_node::{Root, TypedNode},
    SyntaxKind::{self, *},
};

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    Missing(TextRange, Box<[SyntaxKind]>),
    UnknownToken(SyntaxToken),
    UnexpectedEOFWanted(Box<[SyntaxKind]>),
    UnexpectedEOF,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedEOF => write!(f, "unexpected eof"),
            ParseError::UnexpectedEOFWanted(kinds) => {
                write!(f, "unexpected eof, wanted any of {:?}", kinds)
            }
            ParseError::Missing(_, kinds) => write!(f, "missing token, wanted any of {:?}", kinds),
            ParseError::UnknownToken(token) => write!(f, "unkown token {}", token.text()),
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Clone)]
pub struct AST {
    node: GreenNode,
    errors: Vec<ParseError>,
}

impl AST {
    /// Return the root node
    pub fn node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }

    /// Return a borrowed typed root node
    pub fn root(&self) -> Root {
        Root::cast(self.node()).unwrap()
    }

    /// Return all the errors that occured while parsing - NOT
    /// including invalid stuff in the AST
    pub fn root_errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Return all errors in the tree, if any
    pub fn errors(&self) -> Vec<ParseError> {
        let mut errors = self.errors.clone();
        errors.extend(self.root().errors().into_iter().map(|error| match error {
            SyntaxElement::Node(node) => {
                let error_kinds = node
                    .children_with_tokens()
                    .map(|c| c.kind())
                    .collect::<Vec<SyntaxKind>>()
                    .into_boxed_slice();

                ParseError::Missing(node.text_range(), error_kinds)
            }
            SyntaxElement::Token(token) => ParseError::UnknownToken(token),
        }));

        errors
    }

    /*
    /// Either return the first error in the tree, or if there are none return self
    pub fn as_result(self) -> Result<Self, ParseError> {
        if let Some(err) = self.errors.first() {
            return Err(err.clone());
        }
        if let Some(node) = self.root().errors().first() {
            return Err(ParseError::Unexpected(node.text_range()));
        }
        Ok(self)
    }
    */
}

struct Parser<I>
where
    I: Iterator<Item = (SyntaxKind, SmolStr)>,
{
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,

    trivia_buffer: Vec<I::Item>,
    buffer: VecDeque<I::Item>,
    iter: I,
}

impl<I> Parser<I>
where
    I: Iterator<Item = (SyntaxKind, SmolStr)>,
{
    fn new(iter: I) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),

            trivia_buffer: Vec::with_capacity(1),
            buffer: VecDeque::with_capacity(1),
            iter,
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.eat_trivia();
        self.builder.start_node(MsLanguage::kind_to_raw(kind));
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.eat_trivia();
        self.builder.checkpoint()
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, MsLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    // return the first token of buffer and pull from lexer if empty
    fn peek_raw(&mut self) -> Option<&(SyntaxKind, SmolStr)> {
        if self.buffer.is_empty() {
            if let Some(token) = self.iter.next() {
                self.buffer.push_back(token);
            }
        }
        self.buffer.front()
    }

    // Eat the next token and make a token node of it
    fn bump(&mut self) {
        let next = self.buffer.pop_front().or_else(|| self.iter.next());
        match next {
            Some((token, s)) => {
                if token.is_trivia() {
                    self.trivia_buffer.push((token, s))
                } else {
                    self.trivia_buffer.drain(..).for_each({
                        let builder = &mut self.builder;
                        move |(t, s)| builder.token(MsLanguage::kind_to_raw(t), s)
                    });
                    self.builder.token(MsLanguage::kind_to_raw(token), s)
                }
            }
            None => self.errors.push(ParseError::UnexpectedEOF),
        }
    }

    // return a reference of the first non-trivia token with its str, eat all trivia and buffer them
    fn peek_data(&mut self) -> Option<&(SyntaxKind, SmolStr)> {
        while self
            .peek_raw()
            .map(|&(t, _)| t.is_trivia())
            .unwrap_or(false)
        {
            self.bump();
        }
        self.peek_raw()
    }

    // return the first non-trivia token kind, eat all trivia and buffer them
    fn peek(&mut self) -> Option<SyntaxKind> {
        self.peek_data().map(|&(t, _)| t)
    }

    // eat all trivia / buffer them and drain the trivia buffer
    fn eat_trivia(&mut self) {
        self.peek();
        self.trivia_buffer.drain(..).for_each({
            let builder = &mut self.builder;
            move |(t, s)| builder.token(MsLanguage::kind_to_raw(t), s)
        });
    }

    // return the expected kind, make error node if next token doesnt match any
    fn expect_peek_any(&mut self, allowed_slice: &[SyntaxKind]) -> Option<SyntaxKind> {
        let allowed: BitSet256 = allowed_slice.iter().map(|&k| k as u16).collect();

        match self.peek() {
            Some(kind) if allowed.contains(kind as usize) => Some(kind),

            Some(_) => {
                self.start_node(NODE_ERROR);
                for expected in allowed_slice {
                    self.builder
                        .token(MsLanguage::kind_to_raw(*expected), SmolStr::new(""));
                }
                self.finish_node();
                None
            }

            None => {
                self.errors.push(ParseError::UnexpectedEOFWanted(
                    allowed_slice.to_vec().into_boxed_slice(),
                ));

                None
            }
        }
    }

    fn peek_any(&mut self, allowed_slice: &[SyntaxKind]) -> Option<SyntaxKind> {
        let allowed: BitSet256 = allowed_slice.iter().map(|&k| k as u16).collect();

        match self.peek() {
            Some(kind) if allowed.contains(kind as usize) => Some(kind),
            _ => None,
        }
    }

    // eat next token if matches expected kind
    fn expect(&mut self, expected: SyntaxKind) {
        if self.expect_peek_any(&[expected]).is_some() {
            self.bump();
        }
    }

    // make ident node
    fn expect_ident(&mut self) {
        if self.expect_peek_any(&[TOKEN_IDENT]).is_some() {
            self.start_node(NODE_IDENTIFIER);
            self.bump();
            self.finish_node()
        }
    }

    ///
    /// Parse functions
    ///

    fn parse_root(&mut self) {
        loop {
            match self.peek() {
                Some(kind) if kind.is_directive() => self.parse_directive(),
                Some(TOKEN_DECLARE) => self.parse_var_decl(),
                Some(TOKEN_LABEL_STAR) => self.parse_label_decl(),
                Some(TOKEN_IDENT) => self.parse_func_decl(),
                _ => break,
            }
        }
    }

    fn parse_directive(&mut self) {
        match self.peek().unwrap() {
            // #Include "TextLib" as TL
            TOKEN_INCLUDE => {
                self.start_node(NODE_INCLUDE);
                self.bump();
                self.parse_string();
                self.expect(TOKEN_AS);
                self.expect_ident();
                self.finish_node();
            }

            // #Const C_Name LiteralValue
            TOKEN_CONST => {
                self.start_node(NODE_CONST);
                self.bump();
                self.expect_ident();
                self.parse_expr();
                self.finish_node();
            }

            // #Setting S_Name LiteralValue [as Description]
            TOKEN_SETTING => {
                self.start_node(NODE_SETTING);
                self.bump();
                self.expect_ident();
                self.parse_expr();
                if let Some(TOKEN_AS) = self.peek() {
                    self.bump();
                    self.parse_expr();
                }
                self.finish_node();
            }

            // #RequireContext ContextName
            TOKEN_REQUIRE_CONTEXT => {
                self.start_node(NODE_REQUIRE_CONTEXT);
                self.bump();
                self.expect_ident();
                self.finish_node();
            }

            //#Extends "path"
            TOKEN_EXTENDS => {
                self.start_node(NODE_EXTENDS);
                self.bump();
                self.parse_string();
                self.finish_node();
            }

            // #Struct MyStruct { Type1 Field1; Type2 Field2; }
            TOKEN_STRUCT => {
                self.start_node(NODE_STRUCT);
                self.bump();
                self.expect_ident();
                self.expect(TOKEN_OPEN_BRACE);
                while let Some(TOKEN_IDENT) = self.peek() {
                    self.start_node(NODE_STRUCT_FIELD);
                    self.parse_type(None);
                    self.expect_ident();
                    self.expect(TOKEN_SEMICOLON);
                    self.finish_node();
                }
                self.expect(TOKEN_CLOSE_BRACE);
                self.finish_node();
            }
            _ => (),
        }
    }

    // main() { statements.. }
    // Void RegularFunction(Integer A, Real B) { statements.. }
    fn parse_func_decl(&mut self) {
        self.start_node(NODE_FUNC_DECL);

        let mut need_name = true;
        if let Some((TOKEN_IDENT, s)) = self.peek_data() {
            if s == "main" {
                self.start_node(NODE_IDENTIFIER);
                self.bump();
                self.finish_node();
                need_name = false;
            } else {
                self.parse_type(None);
            }
        }

        if need_name {
            self.expect_ident();
        }

        self.expect(TOKEN_OPEN_PAREN);

        loop {
            match self.peek() {
                Some(TOKEN_CLOSE_PAREN) | None => break,
                _ => (),
            }

            self.start_node(NODE_FORMAL_ARG);
            self.parse_type(None);
            self.expect_ident();

            let is_last = if let Some(TOKEN_COMMA) = self.peek() {
                false
            } else {
                true
            };

            if !is_last {
                self.bump();
            }

            self.finish_node();

            if is_last {
                break;
            }
        }

        self.expect(TOKEN_CLOSE_PAREN);
        self.parse_block();
        self.finish_node();
    }

    // ***
    // LabelName
    // ***
    // ***
    // statements..
    // ***
    fn parse_label_decl(&mut self) {
        self.start_node(NODE_LABEL_DECL);
        self.expect(TOKEN_LABEL_STAR);
        self.expect_ident();
        self.expect(TOKEN_LABEL_STAR);
        self.expect(TOKEN_LABEL_STAR);

        loop {
            match self.peek() {
                Some(TOKEN_LABEL_STAR) | None => break,
                _ => (),
            }
            self.parse_statement();
        }

        self.expect(TOKEN_LABEL_STAR);
        self.finish_node();
    }

    // declare [type] name [as alias] [for something] [= initialization_if_no_type];
    fn parse_var_decl(&mut self) {
        self.start_node(NODE_VAR_DECL);
        self.expect(TOKEN_DECLARE);

        while let Some(_) = self.peek_any(&[
            TOKEN_NETREAD,
            TOKEN_NETWRITE,
            TOKEN_PERSISTENT,
            TOKEN_METADATA,
        ]) {
            self.bump();
        }

        let maybe_type_checkpoint = self.checkpoint();

        // Is it the type or the name?
        self.expect_ident();

        // there is name after the type
        // or a [ indicating an array type
        // the previous ident was surely a type!
        if self.peek_any(&[TOKEN_IDENT, TOKEN_OPEN_SQUARE]).is_some() {
            self.parse_type(Some(maybe_type_checkpoint));

            // this one should be the name
            self.expect_ident();
        }

        if let Some(TOKEN_AS) = self.peek() {
            self.bump();
            self.expect_ident();
        }

        if let Some(TOKEN_FOR) = self.peek() {
            self.bump();
            self.parse_expr();
        }

        match self.peek() {
            Some(t) if t.is_assignment_operator() => {
                self.bump();
                self.parse_expr();
            }
            _ => (),
        }

        self.expect(TOKEN_SEMICOLON);
        self.finish_node();
    }

    // a = b
    fn parse_assignment(&mut self, checkpoint: Option<Checkpoint>) {
        match checkpoint {
            Some(cp) => {
                self.start_node_at(cp, NODE_ASSIGNMENT);
            }
            None => {
                self.start_node(NODE_ASSIGNMENT);
                self.parse_expr();
            }
        }
        if self
            .expect_peek_any(SyntaxKind::ASSIGNMENT_OPERATORS)
            .is_some()
        {
            self.bump();
        }
        self.parse_expr();
        self.finish_node();
    }

    // { statements.. }
    fn parse_block(&mut self) {
        self.start_node(NODE_BLOCK);
        self.expect(TOKEN_OPEN_BRACE);
        loop {
            match self.peek() {
                Some(TOKEN_CLOSE_BRACE) | None => break,
                _ => (),
            }
            self.parse_statement();
        }
        self.expect(TOKEN_CLOSE_BRACE);
        self.finish_node();
    }

    fn parse_statement(&mut self) {
        let peeked = self.peek();
        match peeked {
            Some(TOKEN_OPEN_BRACE) => return self.parse_block(),
            Some(TOKEN_DECLARE) => return self.parse_var_decl(),
            Some(TOKEN_IF) => return self.parse_if(),
            Some(TOKEN_SWITCHTYPE) | Some(TOKEN_SWITCH) => return self.parse_switch(),
            Some(TOKEN_FOR) => return self.parse_for(),
            Some(TOKEN_FOREACH) => return self.parse_foreach(),
            Some(TOKEN_WHILE) => return self.parse_while(),

            // +++LabelCall+++
            Some(TOKEN_LABEL_PLUS) => {
                self.start_node(NODE_LABEL_CALL);
                self.bump();
                self.expect_ident();
                self.expect(TOKEN_LABEL_PLUS);
                self.finish_node();
                return;
            }

            // ***LabelCall***
            Some(TOKEN_LABEL_STAR) => {
                self.start_node(NODE_LABEL_CALL);
                self.bump();
                self.expect_ident();
                self.expect(TOKEN_LABEL_STAR);
                self.finish_node();
                return;
            }
            _ => (),
        }

        self.start_node(NODE_STATEMENT);

        match peeked {
            Some(TOKEN_RETURN) => {
                self.start_node(NODE_RETURN);
                self.bump();
                match self.peek() {
                    Some(TOKEN_SEMICOLON) => (),
                    _ => self.parse_expr(),
                };
                self.finish_node();
            }
            Some(TOKEN_CONTINUE) => {
                self.start_node(NODE_CONTINUE);
                self.bump();
                self.finish_node();
            }
            Some(TOKEN_BREAK) => {
                self.start_node(NODE_BREAK);
                self.bump();
                self.finish_node();
            }
            Some(TOKEN_YIELD) => {
                self.start_node(NODE_YIELD);
                self.bump();
                self.finish_node();
            }
            _ => {
                let maybe_assignment_cp = self.checkpoint();

                self.parse_expr();

                match self.peek() {
                    // expr1 = expr2;
                    Some(t) if t.is_assignment_operator() => {
                        self.parse_assignment(Some(maybe_assignment_cp));
                    }

                    // expr1();
                    _ => (),
                }
            }
        }

        self.expect(TOKEN_SEMICOLON);

        self.finish_node();
    }

    // if (condition) statements.. [else statements..]
    fn parse_if(&mut self) {
        self.start_node(NODE_IF_ELSE);
        self.expect(TOKEN_IF);
        self.expect(TOKEN_OPEN_PAREN);
        self.parse_expr();
        self.expect(TOKEN_CLOSE_PAREN);
        self.parse_statement();
        if let Some(TOKEN_ELSE) = self.peek() {
            self.bump();
            self.parse_statement();
        }
        self.finish_node();
    }

    // switch[type] (condition) { cases.. }
    fn parse_switch(&mut self) {
        self.start_node(NODE_SWITCH);
        self.bump();
        self.expect(TOKEN_OPEN_PAREN);
        self.parse_expr();
        self.expect(TOKEN_CLOSE_PAREN);
        self.expect(TOKEN_OPEN_BRACE);

        while let Some(peeked) = self.peek() {
            match peeked {
                TOKEN_CASE => self.parse_case(),
                TOKEN_DEFAULT => self.parse_default(),
                _ => break,
            }
        }

        self.expect(TOKEN_CLOSE_BRACE);
        self.finish_node();
    }

    // case 0: statements..
    fn parse_case(&mut self) {
        self.start_node(NODE_CASE);
        self.expect(TOKEN_CASE);
        self.parse_expr();
        self.expect(TOKEN_COLON);
        self.parse_statement();
        self.finish_node();
    }

    // default: statements..
    fn parse_default(&mut self) {
        self.start_node(NODE_DEFAULT);
        self.expect(TOKEN_DEFAULT);
        self.expect(TOKEN_COLON);
        self.parse_statement();
        self.finish_node();
    }

    // for (I, Start, End) { statements.. }
    fn parse_for(&mut self) {
        self.start_node(NODE_FOR);
        self.expect(TOKEN_FOR);
        self.expect(TOKEN_OPEN_PAREN);
        self.expect_ident();
        self.expect(TOKEN_COMMA);
        self.parse_expr();
        self.expect(TOKEN_COMMA);
        self.parse_expr();
        self.expect(TOKEN_CLOSE_PAREN);
        self.parse_statement();
        self.finish_node();
    }

    // foreach ([Key =>] Value in Array) { statements.. }
    fn parse_foreach(&mut self) {
        self.start_node(NODE_FOREACH);
        self.expect(TOKEN_FOREACH);
        self.expect(TOKEN_OPEN_PAREN);

        let maybe_key_value_cp = self.checkpoint();
        self.expect_ident();

        if let Some(TOKEN_ARROW) = self.peek() {
            // TODO: NODE_KEY_VALUE instead of BINARY_OP?
            self.start_node_at(maybe_key_value_cp, NODE_BINARY_OP);
            self.bump();
            self.expect_ident();
            self.finish_node();
        }

        self.expect(TOKEN_IN);
        self.parse_expr();
        self.expect(TOKEN_CLOSE_PAREN);
        self.parse_statement();
        self.finish_node();
    }

    // while (condition) { statements.. }
    fn parse_while(&mut self) {
        self.start_node(NODE_WHILE);
        self.expect(TOKEN_WHILE);
        self.expect(TOKEN_OPEN_PAREN);
        self.parse_expr();
        self.expect(TOKEN_CLOSE_PAREN);
        self.parse_statement();
        self.finish_node();
    }

    fn parse_type(&mut self, checkpoint: Option<Checkpoint>) {
        match checkpoint {
            Some(cp) => {
                self.start_node_at(cp, NODE_TYPE);
                // ident should be expected between the checkpoint and the call to parse_type
            }
            None => {
                self.start_node(NODE_TYPE);
                self.expect_ident();
            }
        }

        while let Some(TOKEN_OPEN_SQUARE) = self.peek() {
            self.bump();
            if let Some(TOKEN_IDENT) = self.peek() {
                self.bump();
            }
            self.expect(TOKEN_CLOSE_SQUARE);
        }

        self.finish_node();
    }

    // Nud is called when the token is the first of an expression
    fn parse_expr_nud(&mut self) {
        let expect = self.peek();
        match expect {
            Some(TOKEN_IDENT) => {
                let start = self.checkpoint();
                self.bump();

                // Struct initialization: MyStruct { member = 1 }
                if let Some(TOKEN_OPEN_BRACE) = self.peek() {
                    self.start_node_at(start, NODE_STRUCT_INIT);
                    self.bump(); // OPEN_BRACE

                    // Empty init
                    if let Some(TOKEN_CLOSE_BRACE) = self.peek() {
                        self.bump();
                        self.finish_node();
                        return;
                    }

                    // Assignments init
                    self.parse_assignment(None);
                    while let Some(TOKEN_COMMA) = self.peek() {
                        self.bump();
                        self.parse_assignment(None);
                    }

                    self.expect(TOKEN_CLOSE_BRACE);
                    self.finish_node();
                    return;
                }

                // Variable: myvariable
                self.start_node_at(start, NODE_IDENTIFIER);
                self.finish_node();
            }

            // unary minus not: -1  !valid
            Some(TOKEN_MINUS) | Some(TOKEN_NOT) => {
                self.start_node(NODE_UNARY_OP);
                self.bump();
                self.parse_expr_until(expect.unwrap().rbp());
                self.finish_node();
            }

            // string "message"
            Some(TOKEN_STRING_START) => {
                self.parse_string();
            }

            // literal: 1.2
            Some(t) if t.is_literal() => {
                self.start_node(NODE_LITERAL);
                self.bump();
                self.finish_node();
            }

            // expression priority: (1 + 2) * 4
            Some(TOKEN_OPEN_PAREN) => {
                self.start_node(NODE_PARENTHESISED);
                self.bump();
                self.parse_expr_until(expect.unwrap().rbp());
                self.expect(TOKEN_CLOSE_PAREN);
                self.finish_node();
            }

            // vector: <1., 2.>
            Some(TOKEN_LESS) => {
                self.start_node(NODE_VECTOR);
                self.bump();
                self.parse_expr_until(expect.unwrap().rbp());
                while let Some(TOKEN_COMMA) = self.peek() {
                    self.bump();
                    self.parse_expr_until(expect.unwrap().rbp());
                }
                self.expect(TOKEN_MORE);
                self.finish_node();
            }

            // array: [1., 2.]
            Some(TOKEN_OPEN_SQUARE) => {
                self.start_node(NODE_ARRAY);
                self.bump();

                // empty case
                if let Some(TOKEN_CLOSE_SQUARE) = self.peek() {
                    self.bump();
                    self.finish_node();
                    return;
                }

                self.parse_expr_until(expect.unwrap().rbp());
                while let Some(TOKEN_COMMA) = self.peek() {
                    self.bump();
                    self.parse_expr_until(expect.unwrap().rbp());
                }
                self.expect(TOKEN_CLOSE_SQUARE);
                self.finish_node();
            }

            _ => {
                self.start_node(NODE_ERROR);
                self.start_node(NODE_EXPRESSION);
                self.finish_node();
                self.finish_node();
            }
        }
    }

    // Led is called when there is a token in the middle of an expression
    fn parse_expr_led(&mut self, start: Checkpoint) {
        let expect = self.peek();

        match expect {
            // binary operation: a + b, a - b, a => b, ...
            Some(t) if t.is_binary_operator() => {
                self.start_node_at(start, NODE_BINARY_OP);
                self.bump();
                self.parse_expr_until(expect.unwrap().lbp());
                self.finish_node();
            }

            // array access: a[b]
            Some(TOKEN_OPEN_SQUARE) => {
                self.start_node_at(start, NODE_ARRAY_ACCESS);
                self.bump();
                self.parse_expr_until(expect.unwrap().lbp());
                self.expect(TOKEN_CLOSE_SQUARE);
                self.finish_node();
            }

            // function call: a(b)
            Some(TOKEN_OPEN_PAREN) => {
                self.start_node_at(start, NODE_FUNCTION_CALL);
                self.bump();

                // empty case
                if let Some(TOKEN_CLOSE_PAREN) = self.peek() {
                    self.bump();
                    self.finish_node();
                    return;
                }

                // args
                self.parse_expr_until(expect.unwrap().rbp());
                while let Some(TOKEN_COMMA) = self.peek() {
                    self.bump();
                    self.parse_expr_until(expect.unwrap().rbp());
                }

                self.expect(TOKEN_CLOSE_PAREN);
                self.finish_node();
            }

            _ => (),
        }
    }

    fn parse_expr_until(&mut self, rbp: i32) -> Checkpoint {
        let checkpoint = self.checkpoint();

        self.parse_expr_nud();

        while let Some(t) = self.peek() {
            if t.lbp() < rbp {
                break;
            }

            self.parse_expr_led(checkpoint);
        }

        checkpoint
    }

    fn parse_expr(&mut self) {
        self.start_node(NODE_EXPRESSION);
        self.parse_expr_until(0);
        self.finish_node();
    }

    fn parse_string(&mut self) {
        self.start_node(NODE_STRING);
        self.expect(TOKEN_STRING_START);

        loop {
            match self.expect_peek_any(&[
                TOKEN_STRING_END,
                TOKEN_STRING_CONTENT,
                TOKEN_INTERPOL_START,
            ]) {
                Some(TOKEN_STRING_CONTENT) => self.bump(),
                Some(TOKEN_INTERPOL_START) => {
                    self.start_node(NODE_STRING_INTERPOL);
                    self.bump();
                    self.parse_expr();
                    self.expect(TOKEN_INTERPOL_END);
                    self.finish_node();
                }
                // handled by expect_peek_any
                _ => break,
            }
        }
        self.expect(TOKEN_STRING_END);

        self.finish_node();
    }
}

/// Parse tokens into an AST
pub fn parse<I>(iter: I) -> AST
where
    I: IntoIterator<Item = (SyntaxKind, SmolStr)>,
{
    let mut parser = Parser::new(iter.into_iter());
    parser
        .builder
        .start_node(MsLanguage::kind_to_raw(NODE_ROOT));
    parser.parse_root();
    parser.eat_trivia();
    if parser.peek().is_some() {
        parser
            .builder
            .start_node(MsLanguage::kind_to_raw(NODE_ERROR));
        while parser.peek().is_some() {
            parser.bump();
        }
        parser.builder.finish_node();
        parser.eat_trivia();
    }
    parser.builder.finish_node();
    AST {
        node: parser.builder.finish(),
        errors: parser.errors,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::{ffi::OsStr, fmt::Write, fs, path::PathBuf};

    fn test_dir(name: &str) {
        let dir: PathBuf = ["test_data", name].iter().collect();

        for entry in dir.read_dir().unwrap() {
            let entry = entry.unwrap();
            let mut path = entry.path();
            if path.extension() != Some(OsStr::new("ms")) {
                continue;
            }
            let mut code = fs::read_to_string(&path).unwrap();

            if code.ends_with('\n') {
                code.truncate(code.len() - 1);
            }

            if code.ends_with('\r') {
                code.truncate(code.len() - 1);
            }

            let ast = crate::parser::parse(&code);
            path.set_extension("expected");
            let expected = fs::read_to_string(&path).unwrap();

            let mut actual = String::new();
            for error in ast.errors() {
                writeln!(actual, "error: {}", error).unwrap();
            }
            write!(actual, "{}", ast.root().dump()).unwrap();

            if actual != expected {
                path.set_extension("ms");
                eprintln!("In {}:", path.display());
                eprintln!("--- Actual ---");
                eprintln!("{}", actual);
                eprintln!("-- Expected ---");
                eprintln!("{}", expected);
                eprintln!("--- End ---");
                panic!("Tests did not match");
            }
        }
    }

    #[rustfmt::skip]
    mod dir_tests {
        use super::test_dir;
        #[test] fn directives() { test_dir("parser/directives"); }
        #[test] fn expressions() { test_dir("parser/expressions"); }
        #[test] fn declarations() { test_dir("parser/declarations"); }
        #[test] fn control_flow() { test_dir("parser/control_flow"); }
    }
}
