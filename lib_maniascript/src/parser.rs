use crate::ast::*;
use crate::lexer::token::Token;
use crate::lexer::token_kind::TokenKind;
use crate::lexer::Lexer;

use std::iter::Peekable;
use std::fmt;
use std::error::Error;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    pub source: &'a str,
    pub tree: Tree,
}

pub enum ParseError {
    ExpectedToken(TokenKind, Option<(usize, usize, TokenKind)>),
    ExpectedExpr(usize, usize, TokenKind),
    ExpectedOperator(usize, usize, TokenKind),
    UnexpectedToken(usize, usize, TokenKind),
    GlobalsNotFirst,
    IncompleteExpr,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::ExpectedToken(e, Some((l, c, g))) => f.write_str(&format!("{}:{} Expected {} but got {}.", l, c, e, g)),
            ParseError::ExpectedToken(e, None) => f.write_str(&format!("Expected {} token.", e)),
            ParseError::ExpectedExpr(l, c, g) => f.write_str(&format!("{}:{} Expected an expression but got {}.", l, c, g)),
            ParseError::ExpectedOperator(l, c, g) => f.write_str(&format!("{}:{} Expected an operator but got {}.", l, c, g)),
            ParseError::UnexpectedToken(l, c, g) => f.write_str(&format!("{}:{} Unexpected token {}.", l, c, g)),
            ParseError::GlobalsNotFirst => f.write_str("Globals need to be declared before functions and labels."),
            ParseError::IncompleteExpr => f.write_str("Incomplete expression."),
        }
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::ExpectedToken(e, Some((l, c, g))) => f.write_str(&format!("{}:{} Expected {} but got {}.", l, c, e, g)),
            ParseError::ExpectedToken(e, None) => f.write_str(&format!("Expected {} token.", e)),
            ParseError::ExpectedExpr(l, c, g) => f.write_str(&format!("{}:{} Expected an expression but got {}.", l, c, g)),
            ParseError::ExpectedOperator(l, c, g) => f.write_str(&format!("{}:{} Expected an operator but got {}.", l, c, g)),
            ParseError::UnexpectedToken(l, c, g) => f.write_str(&format!("{}:{} Unexpected token {}.", l, c, g)),
            ParseError::GlobalsNotFirst => f.write_str("Globals need to be declared before functions and labels."),
            ParseError::IncompleteExpr => f.write_str("Incomplete expression."),
        }
    }
}

impl Error for ParseError {}

impl Token {
    // Nud is called when the token is the first of an expression
    fn nud(&self, parser: &mut Parser) -> ParseResult<NodeId> {
        match self.kind {
            TokenKind::Identifier => {
                parser.tree.start_node();
                parser.tree.add_node(Node::from(self));
                Ok(parser.tree.end_node(NodeKind::Identifier))
            }

            k if k.is_litteral_value() => {
                parser.tree.start_node();
                parser.tree.add_node(Node::from(self));
                Ok(parser.tree.end_node(NodeKind::Literal))
            }

            // If the first token of an expr is - or ! it is an unary expression
            TokenKind::Minus | TokenKind::Not => {
                let mut unary_op = UnOp::new(parser.tree.start_node());
                unary_op.set_operator(parser.tree.add_node(Node::from(self)));
                unary_op.set_operand(parser.expression(self.rbp())?);
                Ok(parser.tree.end_node(NodeKind::UnOp(unary_op)))
            }

            // If the first token of an expr is ( a parenthesised expression ( expr )
            TokenKind::OpenParen => {
                let mut parenthesised = Parenthesised::new(parser.tree.start_node());

                parenthesised.set_lparen(parser.tree.add_node(Node::from(self)));
                parenthesised.set_expr(parser.expression(self.rbp())?);

                if parser.next_token_is(TokenKind::CloseParen) {
                    parenthesised.set_rparen(parser.next_token_node());
                }

                Ok(parser.tree.end_node(NodeKind::Parenthesised(parenthesised)))
            }

            // An expression starting with < is a vector, <1., 2.> or <1., 2., 3.>
            TokenKind::Inf => {
                let mut vector = Vector::new(parser.tree.start_node());
                vector.set_langle(parser.tree.add_node(Node::from(self)));

                let mut expr_id = parser.expression(self.rbp())?;

                while parser.next_token_is(TokenKind::Comma) {
                    let comma = parser.next_token_node();
                    vector.add_value(expr_id, Some(comma));

                    expr_id = parser.expression(self.rbp())?;
                }

                vector.add_value(expr_id, None);

                vector.set_rangle(parser.expect_node(TokenKind::Sup)?);
                Ok(parser.tree.end_node(NodeKind::Vector(vector)))
            }

            // An expression starting with [ is an array, [ expr1, ... ]
            TokenKind::OpenSquare => {
                let mut array = Array::new(parser.tree.start_node());

                array.set_lsquare(parser.tree.add_node(Node::from(self)));

                if parser.next_token_is(TokenKind::CloseSquare) {
                    array.set_rsquare(parser.next_token_node());
                    return Ok(parser.tree.end_node(NodeKind::Array(array)));
                }

                let mut expr_id = parser.expression(self.rbp())?;

                while parser.next_token_is(TokenKind::Comma) {
                    let comma = parser.next_token_node();
                    array.add_value(expr_id, Some(comma));

                    expr_id = parser.expression(self.rbp())?;
                }

                array.add_value(expr_id, None);
                array.set_rsquare(parser.expect_node(TokenKind::CloseSquare)?);

                Ok(parser.tree.end_node(NodeKind::Array(array)))
            }

            k => Err(ParseError::ExpectedExpr(self.line, self.col, k)),
        }
    }

    // Led is called when there is a token in the middle of an expression
    fn led(&self, parser: &mut Parser, lhs: NodeId) -> ParseResult<NodeId> {
        match self.kind {
            k if k.is_binary_op() => {
                let mut binop = BinaryOp::new(parser.tree.start_node());
                binop.set_lhs(lhs);
                parser.tree.link_nodes(binop.syntax(), lhs);
                binop.set_operator(parser.tree.add_node(Node::from(self)));
                binop.set_rhs(parser.expression(self.lbp())?);
                Ok(parser.tree.end_node(NodeKind::BinaryOp(binop)))
            }

            // An expression with a [ in the middle is an array access, arr[expr]
            TokenKind::OpenSquare => {
                let mut array_access = ArrayAccess::new(parser.tree.start_node());

                array_access.set_lhs(lhs);
                parser.tree.link_nodes(array_access.syntax(), lhs);
                array_access.set_lsquare(parser.tree.add_node(Node::from(self)));
                array_access.set_index(parser.expression(self.lbp())?);
                array_access.set_rsquare(parser.expect_node(TokenKind::CloseSquare)?);

                Ok(parser.tree.end_node(NodeKind::ArrayAccess(array_access)))
            }

            // An expression with a ( in the middle is a function call, myfun(expr)
            TokenKind::OpenParen => {
                let mut function_call = FunctionCall::new(parser.tree.start_node());

                function_call.set_lhs(lhs);
                parser.tree.link_nodes(function_call.syntax(), lhs);
                function_call.set_lparen(parser.tree.add_node(Node::from(self)));

                if parser.next_token_is(TokenKind::CloseParen) {
                    function_call.set_rparen(parser.next_token_node());
                    return Ok(parser.tree.end_node(NodeKind::FunctionCall(function_call)));
                }

                let mut expr_id = parser.expression(self.rbp())?;

                while parser.next_token_is(TokenKind::Comma) {
                    let comma = parser.next_token_node();
                    function_call.add_arg(expr_id, Some(comma));

                    expr_id = parser.expression(self.rbp())?;
                }

                function_call.add_arg(expr_id, None);
                function_call.set_rparen(parser.expect_node(TokenKind::CloseParen)?);

                Ok(parser.tree.end_node(NodeKind::FunctionCall(function_call)))
            }

            k => Err(ParseError::ExpectedOperator(self.line, self.col, k))
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let source = lexer.source;
        let tokens = lexer.peekable();
        let tree = Tree::new();
        Parser {
            tokens,
            source,
            tree,
        }
    }

    pub fn parse(mut self) -> ParseResult<Tree> {
        match self.parse_file() {
            Ok(_) => Ok(self.tree),
            Err(e) => Err(e),
        }
    }

    // UTILITY FUNCTIONS
    fn expect(&mut self, token_kind: TokenKind) -> ParseResult<Token> {
        match self.tokens.peek() {
            Some(ref t) if t.kind == token_kind => Ok(self.tokens.next().unwrap()),
            Some(ref t) => Err(ParseError::ExpectedToken(token_kind, Some((t.line, t.col, t.kind)))),
            _ => Err(ParseError::ExpectedToken(token_kind, None)),
        }
    }

    fn expect_node(&mut self, token_kind: TokenKind) -> ParseResult<NodeId> {
        let token = self.expect(token_kind)?;
        Ok(self.tree.add_node(Node::from(token)))
    }

    fn next_token_node(&mut self) -> NodeId {
        self.tree.add_node(Node::from(self.tokens.next().unwrap()))
    }

    fn next_token_is(&mut self, expected: TokenKind) -> bool {
        if let Some(next) = self.tokens.peek() {
            if next.kind == expected {
                return true;
            }
        }
        false
    }

    // EXPRESSIONS PARSING START
    fn parse_nud(&mut self) -> ParseResult<NodeId> {
        match self.tokens.next() {
            Some(t) => t.nud(self),
            _ => Err(ParseError::IncompleteExpr),
        }
    }

    fn parse_led(&mut self, expr: NodeId) -> ParseResult<NodeId> {
        match self.tokens.next() {
            Some(t) => t.led(self, expr),
            _ => Err(ParseError::IncompleteExpr),
        }
    }

    fn expression(&mut self, rbp: u32) -> ParseResult<NodeId> {
        self.tree.start_node();
        let mut left = self.parse_nud()?;

        while let Some(t) = self.tokens.peek() {
            if t.lbp() < rbp {
                break;
            }

            left = self.parse_led(left)?;
        }

        Ok(self.tree.end_node(NodeKind::Expr))
    }

    pub fn parse_expr(&mut self) -> ParseResult<NodeId> {
        self.expression(1)
    }
    // EXPRESSIONS PARSING END

    pub fn parse_file(&mut self) -> ParseResult<NodeId> {
        let mut file = File::new(self.tree.start_node());

        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_hash() {
                break;
            }
            file.add_hash(self.parse_hash()?);
        }

        while self.next_token_is(TokenKind::Declare) {
            file.add_global(self.parse_vardec()?);
        }

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::EOF => {
                    file.set_eof(self.next_token_node());
                    break;
                }

                TokenKind::Declare => {
                    return Err(ParseError::GlobalsNotFirst);
                }

                TokenKind::Identifier => {
                    file.add_function(self.parse_funcdec()?);
                }

                TokenKind::LabelStar => {
                    file.add_label(self.parse_labeldec()?);
                }

                k => {
                    return Err(ParseError::UnexpectedToken(next.line, next.col, k));
                }
            }
        }

        Ok(self.tree.end_node(NodeKind::File(file)))
    }

    pub fn parse_hash(&mut self) -> ParseResult<NodeId> {
        match self.tokens.peek().unwrap().kind {
            TokenKind::Include => {
                let mut include = Include::new(self.tree.start_node());
                include.set_include(self.next_token_node());
                include.set_path(self.expect_node(TokenKind::LineString)?);
                include.set_as_(self.expect_node(TokenKind::As)?);
                include.set_name(self.expect_node(TokenKind::Identifier)?);
                Ok(self.tree.end_node(NodeKind::Include(include)))
            }
            TokenKind::Const => {
                let mut const_ = Const::new(self.tree.start_node());
                const_.set_const_(self.next_token_node());
                const_.set_name(self.expect_node(TokenKind::Identifier)?);
                const_.set_value(self.parse_expr()?);
                Ok(self.tree.end_node(NodeKind::Const(const_)))
            }
            TokenKind::Setting => {
                let mut setting = Setting::new(self.tree.start_node());
                setting.set_setting(self.next_token_node());
                setting.set_name(self.expect_node(TokenKind::Identifier)?);
                setting.set_value(self.parse_expr()?);
                Ok(self.tree.end_node(NodeKind::Setting(setting)))
            }
            TokenKind::RequireContext => {
                let mut require_context = RequireContext::new(self.tree.start_node());
                require_context.set_require_context(self.next_token_node());
                require_context.set_name(self.expect_node(TokenKind::Identifier)?);
                Ok(self
                    .tree
                    .end_node(NodeKind::RequireContext(require_context)))
            }
            TokenKind::Extends => {
                let mut extends = Extends::new(self.tree.start_node());
                extends.set_extends(self.next_token_node());
                extends.set_path(self.expect_node(TokenKind::LineString)?);
                Ok(self.tree.end_node(NodeKind::Extends(extends)))
            }
            _ => unreachable!(),
        }
    }

    pub fn parse_funcdec(&mut self) -> ParseResult<NodeId> {
        let mut funcdec = FuncDec::new(self.tree.start_node());

        // If the parsed type is just an identifier, maybe it's a name instead
        let name_id = self.parse_type()?;

        let mut is_identifier_only = false;
        if self.tree.children[name_id].len() == 1 {
            let child_id = self.tree.children[name_id][0];
            if let NodeKind::Token(t) = &self.tree.nodes[child_id].kind {
                if let TokenKind::Identifier = t.kind {
                    is_identifier_only = true;
                }
            }
        }

        if !is_identifier_only || self.next_token_is(TokenKind::Identifier) {
            funcdec.set_type_(name_id);
            funcdec.set_name(self.expect_node(TokenKind::Identifier)?);
        } else {
            funcdec.set_name(name_id);
        }

        if self.next_token_is(TokenKind::OpenParen) {
            funcdec.set_lparen(self.next_token_node());
        }

        while self.next_token_is(TokenKind::Identifier) {
            let mut arg = FormalArg::new(self.tree.start_node());

            arg.set_type_(self.parse_type()?);

            if self.next_token_is(TokenKind::Identifier) {
                arg.set_name(self.next_token_node());
            }

            let is_last_arg = !self.next_token_is(TokenKind::Comma);

            if !is_last_arg {
                arg.set_comma(self.next_token_node());
            }

            funcdec.add_arg(self.tree.end_node(NodeKind::FormalArg(arg)));

            if is_last_arg {
                break;
            }
        }

        if self.next_token_is(TokenKind::CloseParen) {
            funcdec.set_rparen(self.next_token_node());
        }

        funcdec.set_body(self.parse_block()?);

        Ok(self.tree.end_node(NodeKind::FuncDec(funcdec)))
    }

    pub fn parse_labeldec(&mut self) -> ParseResult<NodeId> {
        let mut label = LabelImpl::new(self.tree.start_node());

        if self.next_token_is(TokenKind::LabelStar) {
            label.set_stars1(self.next_token_node());
        }

        if self.next_token_is(TokenKind::Identifier) {
            label.set_name(self.next_token_node());
        }

        if self.next_token_is(TokenKind::LabelStar) {
            label.set_stars2(self.next_token_node());
        }

        if self.next_token_is(TokenKind::LabelStar) {
            label.set_stars3(self.next_token_node());
        }

        while !self.next_token_is(TokenKind::LabelStar) {
            label.add_statement(self.parse_statement()?);
        }

        if self.next_token_is(TokenKind::LabelStar) {
            label.set_stars4(self.next_token_node());
        }

        Ok(self.tree.end_node(NodeKind::LabelImpl(label)))
    }

    pub fn parse_block(&mut self) -> ParseResult<NodeId> {
        let mut block = Block::new(self.tree.start_node());

        if self.next_token_is(TokenKind::OpenBrace) {
            block.set_lbrace(self.next_token_node());
        }

        while !self.next_token_is(TokenKind::CloseBrace) {
            block.add_statement(self.parse_statement()?);
        }

        if self.next_token_is(TokenKind::CloseBrace) {
            block.set_rbrace(self.next_token_node());
        }

        Ok(self.tree.end_node(NodeKind::Block(block)))
    }

    pub fn parse_vardec(&mut self) -> ParseResult<NodeId> {
        let mut vardec = VarDec::new(self.tree.start_node());

        if self.next_token_is(TokenKind::Declare) {
            vardec.set_declare(self.next_token_node());
        }

        loop {
            if self.next_token_is(TokenKind::Netread) {
                vardec.set_netread(self.next_token_node());
            } else if self.next_token_is(TokenKind::Netwrite) {
                vardec.set_netwrite(self.next_token_node());
            } else if self.next_token_is(TokenKind::Persistent) {
                vardec.set_persistent(self.next_token_node());
            } else if self.next_token_is(TokenKind::Metadata) {
                vardec.set_metadata(self.next_token_node());
            } else {
                break;
            }
        }

        // If the parsed type is just an identifier, maybe it's a name instead
        let name_id = self.parse_type()?;

        let mut is_identifier_only = false;
        if self.tree.children[name_id].len() == 1 {
            let child_id = self.tree.children[name_id][0];
            if let NodeKind::Token(t) = &self.tree.nodes[child_id].kind {
                if let TokenKind::Identifier = t.kind {
                    is_identifier_only = true;
                }
            }
        }

        if !is_identifier_only || self.next_token_is(TokenKind::Identifier) {
            vardec.set_type_(name_id);
            vardec.set_name(self.expect_node(TokenKind::Identifier)?);
        } else {
            vardec.set_name(name_id);
        }

        if self.next_token_is(TokenKind::As) {
            vardec.set_as_(self.next_token_node());
            vardec.set_alias(self.expect_node(TokenKind::Identifier)?);
        }

        if self.next_token_is(TokenKind::For) {
            vardec.set_for_(self.next_token_node());
            vardec.set_target(self.parse_expr()?);
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind.is_assign_op() {
                vardec.set_assignment(self.next_token_node());
                vardec.set_value(self.parse_expr()?);
            }
        }

        let semi = self.expect_node(TokenKind::Semicolon)?;
        self.tree.link_nodes(vardec.syntax(), semi);

        Ok(self.tree.end_node(NodeKind::VarDec(vardec)))
    }

    pub fn parse_else(&mut self) -> ParseResult<NodeId> {
        let mut else_ = Else::new(self.tree.start_node());

        if self.next_token_is(TokenKind::Else) {
            else_.set_else_(self.next_token_node());
        }

        if self.next_token_is(TokenKind::If) {
            else_.set_if_(self.parse_if()?);
        } else {
            else_.set_body(self.parse_statement()?);
        }

        Ok(self.tree.end_node(NodeKind::Else(else_)))
    }

    pub fn parse_if(&mut self) -> ParseResult<NodeId> {
        let mut if_ = If::new(self.tree.start_node());

        if self.next_token_is(TokenKind::If) {
            if_.set_if_(self.next_token_node());
        }

        if self.next_token_is(TokenKind::OpenParen) {
            if_.set_lparen(self.next_token_node());
        }

        if_.set_condition(self.parse_expr()?);

        if self.next_token_is(TokenKind::CloseParen) {
            if_.set_rparen(self.next_token_node());
        }

        if_.set_body(self.parse_statement()?);

        if self.next_token_is(TokenKind::Else) {
            if_.set_else_(self.parse_else()?);
        }

        Ok(self.tree.end_node(NodeKind::If(if_)))
    }

    pub fn parse_statement(&mut self) -> ParseResult<NodeId> {
        let mut statement = Statement::new(self.tree.start_node());

        let tkind = self.tokens.peek().unwrap().kind;
        let substatement = match &tkind {
            TokenKind::OpenBrace => self.parse_block()?,
            TokenKind::Declare => self.parse_vardec()?,
            TokenKind::If => self.parse_if()?,
            TokenKind::Switch | TokenKind::SwitchType => self.parse_switch()?,
            TokenKind::For => self.parse_for()?,
            TokenKind::Foreach => self.parse_foreach()?,
            TokenKind::While => self.parse_while()?,

            TokenKind::LabelPlus => {
                let mut label = LabelCall::new(self.tree.start_node());
                label.set_start(self.next_token_node());
                label.set_name(self.expect_node(TokenKind::Identifier)?);
                label.set_end(self.expect_node(TokenKind::LabelPlus)?);
                self.tree.end_node(NodeKind::LabelCall(label))
            }

            TokenKind::LabelMinus => {
                let mut label = LabelCall::new(self.tree.start_node());
                label.set_start(self.next_token_node());
                label.set_name(self.expect_node(TokenKind::Identifier)?);
                label.set_end(self.expect_node(TokenKind::LabelMinus)?);
                self.tree.end_node(NodeKind::LabelCall(label))
            }

            TokenKind::Return => {
                let mut return_ = Return::new(self.tree.start_node());
                return_.set_return_(self.next_token_node());
                if !self.next_token_is(TokenKind::Semicolon) {
                    return_.set_value(self.parse_expr()?);
                }
                self.tree.end_node(NodeKind::Return(return_))
            }

            TokenKind::Break | TokenKind::Continue | TokenKind::Yield => {
                self.tree.start_node();
                self.next_token_node();
                self.tree.end_node(match &tkind {
                    TokenKind::Break => NodeKind::Break,
                    TokenKind::Continue => NodeKind::Continue,
                    TokenKind::Yield => NodeKind::Yield,
                    _ => unreachable!(),
                })
            }

            // expr = value; OR  expr;
            _ => {
                let expr_id = self.parse_expr()?;

                let mut is_assign = false;
                // If there is no assignment op then it is only an expression
                if let Some(next) = self.tokens.peek() {
                    if next.kind.is_assign_op() {
                        is_assign = true;
                    }
                }

                if is_assign {
                    let mut assignment = Assignment::new(self.tree.start_node());

                    assignment.set_lvalue(expr_id);
                    assignment.set_operator(self.next_token_node());
                    assignment.set_rvalue(self.parse_expr()?);
                    self.tree.end_node(NodeKind::Assignment(assignment))
                } else {
                    expr_id
                }
            }
        };

        statement.set_statement(substatement);

        if self.next_token_is(TokenKind::Semicolon) {
            statement.set_semicolon(self.next_token_node());
        }

        Ok(self.tree.end_node(NodeKind::Statement(statement)))
    }

    pub fn parse_switch(&mut self) -> ParseResult<NodeId> {
        let mut switch = Switch::new(self.tree.start_node());

        switch.set_switch(self.next_token_node());

        if self.next_token_is(TokenKind::OpenParen) {
            switch.set_lparen(self.next_token_node());
        }

        switch.set_value(self.parse_expr()?);

        if self.next_token_is(TokenKind::CloseParen) {
            switch.set_rparen(self.next_token_node());
        }

        if self.next_token_is(TokenKind::OpenBrace) {
            switch.set_lbrace(self.next_token_node());
        }

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => switch.set_default(self.parse_default()?),
                TokenKind::Case => switch.add_case(self.parse_case()?),
                _ => break,
            };
        }

        if self.next_token_is(TokenKind::CloseBrace) {
            switch.set_rbrace(self.next_token_node());
        }

        Ok(self.tree.end_node(NodeKind::Switch(switch)))
    }

    pub fn parse_case(&mut self) -> ParseResult<NodeId> {
        let mut case = Case::new(self.tree.start_node());

        case.set_case(self.next_token_node());
        case.set_value(self.parse_expr()?);
        case.set_colon(self.expect_node(TokenKind::Colon)?);
        case.set_statement(self.parse_statement()?);

        Ok(self.tree.end_node(NodeKind::Case(case)))
    }

    pub fn parse_default(&mut self) -> ParseResult<NodeId> {
        let mut default = DefaultCase::new(self.tree.start_node());

        default.set_default(self.next_token_node());

        if self.next_token_is(TokenKind::Colon) {
            default.set_colon(self.next_token_node());
        }

        default.set_statement(self.parse_statement()?);

        Ok(self.tree.end_node(NodeKind::Default(default)))
    }

    pub fn parse_for(&mut self) -> ParseResult<NodeId> {
        let mut for_ = For::new(self.tree.start_node());

        for_.set_for_(self.expect_node(TokenKind::For)?);

        if self.next_token_is(TokenKind::OpenParen) {
            for_.set_lparen(self.next_token_node());
        }

        if self.next_token_is(TokenKind::Identifier) {
            for_.set_name(self.next_token_node());
        }

        if self.next_token_is(TokenKind::Comma) {
            for_.set_comma1(self.next_token_node());
        }

        for_.set_value_start(self.parse_expr()?);

        if self.next_token_is(TokenKind::Comma) {
            for_.set_comma2(self.next_token_node());
        }

        for_.set_value_end(self.parse_expr()?);

        if self.next_token_is(TokenKind::CloseParen) {
            for_.set_rparen(self.next_token_node());
        }

        for_.set_body(self.parse_statement()?);

        Ok(self.tree.end_node(NodeKind::For(for_)))
    }

    pub fn parse_foreach(&mut self) -> ParseResult<NodeId> {
        let mut foreach = Foreach::new(self.tree.start_node());
        foreach.set_foreach(self.expect_node(TokenKind::Foreach)?);

        if self.next_token_is(TokenKind::OpenParen) {
            foreach.set_lparen(self.next_token_node());
        }

        if self.next_token_is(TokenKind::Identifier) {
            foreach.set_name1(self.next_token_node());
        }

        if self.next_token_is(TokenKind::Arrow) {
            foreach.set_arrow(self.next_token_node());
            foreach.set_name2(self.expect_node(TokenKind::Identifier)?);
        }

        if self.next_token_is(TokenKind::In) {
            foreach.set_in_(self.next_token_node());
        }

        foreach.set_value(self.parse_expr()?);

        if self.next_token_is(TokenKind::CloseParen) {
            foreach.set_rparen(self.next_token_node());
        }

        foreach.set_body(self.parse_statement()?);

        Ok(self.tree.end_node(NodeKind::Foreach(foreach)))
    }

    pub fn parse_while(&mut self) -> ParseResult<NodeId> {
        let mut while_ = While::new(self.tree.start_node());
        while_.set_while_(self.expect_node(TokenKind::While)?);

        if self.next_token_is(TokenKind::OpenParen) {
            while_.set_lparen(self.next_token_node());
        }

        while_.set_condition(self.parse_expr()?);

        if self.next_token_is(TokenKind::CloseParen) {
            while_.set_rparen(self.next_token_node());
        }

        while_.set_body(self.parse_statement()?);

        Ok(self.tree.end_node(NodeKind::While(while_)))
    }

    pub fn parse_type(&mut self) -> ParseResult<NodeId> {
        let mut type_ = Type::new(self.tree.start_node());

        type_.set_basename(self.expect_node(TokenKind::Identifier)?);

        while self.next_token_is(TokenKind::ColonColon) {
            let colon = self.next_token_node();
            let ident = self.expect_node(TokenKind::Identifier)?;
            type_.add_member(colon, ident);
        }

        // Continue to parse while there is an open square bracket ( the type is an array type )
        while self.next_token_is(TokenKind::OpenSquare) {
            let opens = self.next_token_node();
            let array_type = if self.next_token_is(TokenKind::Identifier) {
                Some(self.next_token_node())
            } else {
                None
            };
            let closes = self.expect_node(TokenKind::CloseSquare)?;
            type_.add_array(opens, array_type, closes);
        }

        Ok(self.tree.end_node(NodeKind::Type(type_)))
    }
}
