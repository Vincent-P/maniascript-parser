use crate::ast::Node;
use crate::ast::{ExpressionKind, NodeKind};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token_kind::TokenKind;
use std::iter::Peekable;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    pub source: &'a str,
}

impl Token {
    fn nud(&self, parser: &mut Parser) -> Result<Node, String> {
        match self.kind {
            TokenKind::Identifier => {
                let expr = ExpressionKind::from(self.kind);
                let mut node = Node::new_expr(expr);

                let tok = Node::from(self);

                node.add_child(tok);
                Ok(node)
            }

            k if k.is_litteral_value() => {
                let expr = ExpressionKind::from(self.kind);
                let mut node = Node::new_expr(expr);

                let tok = Node::from(self);

                node.add_child(tok);
                Ok(node)
            }

            TokenKind::Minus | TokenKind::Not => {
                let rhs = parser.expression(self.rbp())?;

                let expr = match self.kind {
                    TokenKind::Minus => ExpressionKind::Negative,
                    _ => ExpressionKind::Not,
                };

                let mut node = Node::new_expr(expr);
                node.add_child(rhs);

                Ok(node)
            }

            TokenKind::OpenParen => {
                let mut rhs = parser.expression(self.rbp())?;
                let end = parser.expect(TokenKind::CloseParen)?;
                rhs.span = (self.position, end.position + end.len);
                Ok(rhs)
            }

            TokenKind::Inf => {
                let expr = ExpressionKind::Vector;
                let mut node = Node::new_expr(expr);

                node.add_child(parser.expression(self.rbp())?);

                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    parser.tokens.next();
                    node.add_child(parser.expression(self.rbp())?);
                }

                let (_, end) = parser.expect(TokenKind::Sup)?.span();
                node.span = (self.position, end);
                Ok(node)
            }

            TokenKind::OpenSquare => {
                let expr = ExpressionKind::Array;
                let mut node = Node::new_expr(expr);

                if let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind == TokenKind::CloseSquare {
                        let (_, end) = parser.tokens.next().unwrap().span();
                        node.span = (self.position, end);
                        return Ok(node);
                    }
                }

                node.add_child(parser.expression(self.rbp())?);

                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    parser.tokens.next();
                    node.add_child(parser.expression(self.rbp())?);
                }

                let (_, end) = parser.expect(TokenKind::CloseSquare)?.span();
                node.span = (self.position, end);
                Ok(node)
            }

            k => Err(format!(
                "{}:{} Expected an expression but got {:?}.",
                self.line, self.col, k
            )),
        }
    }

    fn led(&self, parser: &mut Parser, lhs: Node) -> Result<Node, String> {
        match self.kind {
            k if k.is_binary_op() => {
                let expr = ExpressionKind::from(k);
                let mut node = Node::new_expr(expr);
                let rhs = parser.expression(self.lbp())?;

                node.add_child(lhs);
                node.add_child(rhs);

                Ok(node)
            }

            TokenKind::As => {
                let expr = ExpressionKind::from(self.kind);
                let mut node = Node::new_expr(expr);

                let type_node = parser.parse_type()?;

                node.add_child(lhs);
                node.add_child(type_node);

                Ok(node)
            }

            TokenKind::Is => {
                let expr = ExpressionKind::from(self.kind);
                let mut node = Node::new_expr(expr);

                let type_node = parser.parse_type()?;

                node.add_child(lhs);
                node.add_child(type_node);

                Ok(node)
            }

            TokenKind::OpenSquare => {
                let expr = ExpressionKind::ArrayAccess;
                let mut node = Node::new_expr(expr);

                let rhs = parser.expression(self.lbp())?;
                let close_token = parser.expect(TokenKind::CloseSquare)?;

                node.add_child(lhs);
                node.add_child(rhs);

                let (begin, _) = node.span;
                let (_, end) = close_token.span();
                node.span = (begin, end);

                Ok(node)
            }

            TokenKind::OpenParen => {
                let expr = ExpressionKind::FunctionCall;
                let mut node = Node::new_expr(expr);
                node.add_child(lhs);

                if let Some(next) = parser.tokens.peek() {
                    if next.kind == TokenKind::CloseParen {
                        let close_token = parser.tokens.next().unwrap();
                        let (begin, _) = node.span;
                        let (_, end) = close_token.span();
                        node.span = (begin, end);
                        return Ok(node);
                    }
                }

                node.add_child(parser.expression(self.lbp())?);
                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    parser.tokens.next();
                    node.add_child(parser.expression(self.lbp())?);
                }

                let close_token = parser.expect(TokenKind::CloseParen)?;
                let (begin, _) = node.span;
                let (_, end) = close_token.span();
                node.span = (begin, end);

                Ok(node)
            }

            TokenKind::Arrow => {
                let expr = ExpressionKind::from(self.kind);
                let mut node = Node::new_expr(expr);

                let rhs = parser.expression(self.lbp())?;
                node.add_child(lhs);
                node.add_child(rhs);

                Ok(node)
            }

            k => Err(format!(
                "{}:{} Expected an operator but got {:?}.",
                self.line, self.col, k
            )),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let source = lexer.source;
        let tokens = lexer.peekable();
        Parser { tokens, source }
    }

    fn expect(&mut self, token_kind: TokenKind) -> Result<Token, String> {
        match self.tokens.peek() {
            Some(ref t) if t.kind == token_kind => Ok(self.tokens.next().unwrap()),
            Some(ref t) => Err(format!(
                "{}:{} Expected {:?} token but got {:?}.",
                t.line, t.col, token_kind, t.kind,
            )),
            _ => Err(format!("Expected {:?} token.", token_kind)),
        }
    }

    fn parse_nud(&mut self) -> Result<Node, String> {
        match self.tokens.next() {
            Some(t) => t.nud(self),
            _ => Err("Incomplete expression.".to_string()),
        }
    }

    fn parse_led(&mut self, expr: Node) -> Result<Node, String> {
        match self.tokens.next() {
            Some(t) => t.led(self, expr),
            _ => Err("Incomplete expression.".to_string()),
        }
    }

    fn expression(&mut self, rbp: u32) -> Result<Node, String> {
        let mut left = self.parse_nud()?;

        while let Some(t) = self.tokens.peek() {
            if t.lbp() < rbp {
                break;
            }

            left = self.parse_led(left)?;
        }

        Ok(left)
    }

    pub fn parse_expr(&mut self) -> Result<Node, String> {
        self.expression(1)
    }

    pub fn parse_file(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::File);

        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_hash() {
                break;
            }
            node.add_child(self.parse_hash()?);
        }

        while let Some(next) = self.tokens.peek() {
            if next.kind != TokenKind::Declare {
                break;
            }
            node.add_child(self.parse_vardec()?);
        }

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::EOF => break,
                TokenKind::Declare => {
                    return Err("Globals need to be defined before functions.".to_string())
                }
                TokenKind::Identifier => {
                    node.add_child(self.parse_funcdec()?);
                }
                TokenKind::LabelStar => {
                    node.add_child(self.parse_labeldec()?);
                }
                k => {
                    return Err(format!(
                        "{}:{} Unexpected token {:?}.",
                        next.line, next.col, k
                    ))
                }
            }
        }

        Ok(node)
    }

    pub fn parse_hash(&mut self) -> Result<Node, String> {
        let kind = match self.tokens.peek() {
            Some(t) if t.kind.is_hash() => t.kind,
            _ => return Err("Expected a hash statement.".to_string()),
        };

        match kind {
            TokenKind::Include => {
                let mut node = Node::new(NodeKind::Include);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::LineString)?));
                self.expect(TokenKind::As)?;
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
                Ok(node)
            }
            TokenKind::Const => {
                let mut node = Node::new(NodeKind::Const);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
                node.add_child(self.parse_expr()?);
                Ok(node)
            }
            TokenKind::Setting => {
                let mut node = Node::new(NodeKind::Setting);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
                node.add_child(self.parse_expr()?);

                if let Some(next) = self.tokens.peek() {
                    if next.kind == TokenKind::As {
                        self.tokens.next();
                        node.add_child(self.parse_expr()?);
                    }
                }

                Ok(node)
            }
            TokenKind::RequireContext => {
                let mut node = Node::new(NodeKind::RequireContext);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
                Ok(node)
            }
            TokenKind::Extends => {
                let mut node = Node::new(NodeKind::Extends);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::LineString)?));
                Ok(node)
            }
            _ => Err("Expected Include, Const, Setting, RequireContext or Extends.".to_string()),
        }
    }

    pub fn parse_funcdec(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::FuncDec);

        let mut _type = self.parse_type()?;
        let _name = match self.tokens.peek() {
            // There is an identifier after the type
            Some(t) if t.kind == TokenKind::Identifier => {
                node.add_child(_type);
                Node::from(self.tokens.next().unwrap())
            }
            // There is no type so the previous identifier is in fact the name
            _ => {
                let _new_name = _type.children.pop().unwrap();
                _new_name
            }
        };
        node.add_child(Node::from(_name));

        self.expect(TokenKind::OpenParen)?;

        while let Some(next) = self.tokens.peek() {
            if next.kind != TokenKind::Identifier {
                break;
            }
            let _arg_type = self.parse_type()?;
            let _arg_name = Node::from(self.expect(TokenKind::Identifier)?);
            node.add_child(_arg_type);
            node.add_child(_arg_name);

            match self.tokens.peek() {
                Some(t) if t.kind == TokenKind::Comma => self.tokens.next(),
                _ => break
            };
        }

        self.expect(TokenKind::CloseParen)?;
        node.add_child(self.parse_block()?);
        Ok(node)
    }

    pub fn parse_labeldec(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::LabelDec);

        self.expect(TokenKind::LabelStar)?;
        node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
        self.expect(TokenKind::LabelStar)?;

        self.expect(TokenKind::LabelStar)?;

        node.add_child(self.parse_statement()?);
        while let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::LabelStar {
                break;
            }
            node.add_child(self.parse_statement()?);
        }

        self.expect(TokenKind::LabelStar)?;
        Ok(node)
    }

    pub fn parse_block(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::Block);
        self.expect(TokenKind::OpenBrace)?;

        while let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::CloseBrace {
                break;
            }
            node.add_child(self.parse_statement()?);
        }

        self.expect(TokenKind::CloseBrace)?;
        Ok(node)
    }

    pub fn parse_vardec(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::VarDec);

        self.expect(TokenKind::Declare)?;

        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_decl_metadata() {
                break;
            }
            node.add_child(Node::from(self.tokens.next().unwrap()));
        }

        let mut _type = self.parse_type()?;
        let _name = match self.tokens.peek() {
            // There is an identifier after the type
            Some(t) if t.kind == TokenKind::Identifier => {
                node.add_child(_type);
                Node::from(self.tokens.next().unwrap())
            }
            // There is no type so the previous identifier is in fact the name
            _ => {
                let _new_name = _type.children.pop().unwrap();
                _new_name
            }
        };
        node.add_child(Node::from(_name));

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::As {
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
            }
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::For {
                self.tokens.next();
                node.add_child(self.parse_expr()?);
            }
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind.is_assign_op() {
                let mut assign = Node::new(NodeKind::Assignment);
                assign.add_child(Node::from(self.tokens.next().unwrap()));
                assign.add_child(self.parse_expr()?);
                node.add_child(assign);
            }
        }

        self.expect(TokenKind::Semicolon)?;

        Ok(node)
    }

    pub fn parse_else(&mut self) -> Result<Node, String> {
        self.expect(TokenKind::Else)?;

        match self.tokens.peek() {
            Some(t) if t.kind == TokenKind::If => {
                Ok(self.parse_if()?)
            }
            _ => {
                let mut node = Node::new(NodeKind::Else);
                node.add_child(self.parse_statement()?);
                Ok(node)
            }
        }
    }

    pub fn parse_if(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::If);

        self.expect(TokenKind::If)?;
        self.expect(TokenKind::OpenParen)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::CloseParen)?;
        node.add_child(self.parse_statement()?);

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Else {
                node.add_child(self.parse_else()?);
            }
        }

        Ok(node)
    }

    pub fn parse_statement(&mut self) -> Result<Node, String> {
        let kind = match self.tokens.peek() {
            Some(ref t) => t.kind,
            _ => return Err("Expected a statement.".to_string()),
        };

        match kind {
            TokenKind::OpenBrace => self.parse_block(),
            TokenKind::Declare => self.parse_vardec(),
            TokenKind::If => self.parse_if(),
            TokenKind::Switch => self.parse_switch(),
            TokenKind::SwitchType => self.parse_switchtype(),
            TokenKind::For => self.parse_for(),
            TokenKind::Foreach => self.parse_foreach(),
            TokenKind::While => self.parse_while(),

            TokenKind::LabelPlus => {
                let mut node = Node::new(NodeKind::LabelCall);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
                self.expect(TokenKind::LabelPlus)?;
                Ok(node)
            }

            TokenKind::LabelMinus => {
                let mut node = Node::new(NodeKind::LabelCall);
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
                self.expect(TokenKind::LabelMinus)?;
                Ok(node)
            }

            TokenKind::Break => {
                self.tokens.next();
                let node = Node::new(NodeKind::Break);
                self.expect(TokenKind::Semicolon)?;
                Ok(node)
            }

            TokenKind::Continue => {
                self.tokens.next();
                let node = Node::new(NodeKind::Continue);
                self.expect(TokenKind::Semicolon)?;
                Ok(node)
            }

            TokenKind::Return => {
                let mut node = Node::new(NodeKind::Return);

                self.tokens.next();
                if let Some(next) = self.tokens.peek() {
                    if next.kind == TokenKind::Semicolon {
                        self.tokens.next();
                        return Ok(node);
                    }
                }

                node.add_child(self.parse_expr()?);
                self.expect(TokenKind::Semicolon)?;
                Ok(node)
            }

            _ => {
                let mut node = self.parse_expr()?;
                if let Some(next) = self.tokens.peek() {
                    if next.kind.is_assign_op() {
                        let mut parent_node = Node::new(NodeKind::Assignment);
                        parent_node.add_child(node);
                        node = parent_node;

                        node.add_child(Node::from(self.tokens.next().unwrap()));
                        node.add_child(self.parse_expr()?);
                    }
                }
                self.expect(TokenKind::Semicolon)?;
                Ok(node)
            }
        }
    }

    pub fn parse_switch(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::Switch);

        self.expect(TokenKind::Switch)?;
        self.expect(TokenKind::OpenParen)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::OpenBrace)?;

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => node.add_child(self.parse_default()?),
                TokenKind::Case => node.add_child(self.parse_case()?),
                _ => break,
            };
        }

        self.expect(TokenKind::CloseBrace)?;

        Ok(node)
    }
    pub fn parse_switchtype(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::SwitchType);
        self.expect(TokenKind::SwitchType)?;
        self.expect(TokenKind::OpenParen)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::OpenBrace)?;

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => node.add_child(self.parse_default()?),
                TokenKind::Case => node.add_child(self.parse_case_type()?),
                _ => break,
            };
        }

        self.expect(TokenKind::CloseBrace)?;

        Ok(node)
    }

    pub fn parse_case(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::Case);
        self.expect(TokenKind::Case)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::Colon)?;
        node.add_child(self.parse_statement()?);
        Ok(node)
    }

    pub fn parse_case_type(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::CaseType);
        self.expect(TokenKind::Case)?;
        node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
        self.expect(TokenKind::Colon)?;
        node.add_child(self.parse_statement()?);
        Ok(node)
    }

    pub fn parse_default(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::Default);
        self.expect(TokenKind::Default)?;
        self.expect(TokenKind::Colon)?;
        node.add_child(self.parse_statement()?);
        Ok(node)
    }

    pub fn parse_for(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::For);
        self.expect(TokenKind::For)?;
        self.expect(TokenKind::OpenParen)?;
        node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
        self.expect(TokenKind::Comma)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::Comma)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::CloseParen)?;
        node.add_child(self.parse_statement()?);
        Ok(node)
    }

    pub fn parse_foreach(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::Foreach);
        self.expect(TokenKind::Foreach)?;
        self.expect(TokenKind::OpenParen)?;
        node.add_child(Node::from(self.expect(TokenKind::Identifier)?));

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Arrow {
                self.tokens.next();
                node.add_child(Node::from(self.expect(TokenKind::Identifier)?));
            }
        }

        self.expect(TokenKind::In)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::CloseParen)?;

        node.add_child(self.parse_statement()?);
        Ok(node)
    }

    pub fn parse_while(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::While);
        self.expect(TokenKind::While)?;
        self.expect(TokenKind::OpenParen)?;
        node.add_child(self.parse_expr()?);
        self.expect(TokenKind::CloseParen)?;
        node.add_child(self.parse_statement()?);
        Ok(node)
    }

    pub fn parse_type(&mut self) -> Result<Node, String> {
        let mut node = Node::new(NodeKind::Type);

        let inner_type = Node::from(self.expect(TokenKind::Identifier)?);
        node.add_child(inner_type);

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::OpenSquare => {
                    self.tokens.next();
                    let array = Node::new_expr(ExpressionKind::Array);

                    if let Some(following) = self.tokens.peek() {
                        if following.kind == TokenKind::Identifier {
                            node.add_child(Node::from(self.tokens.next().unwrap()));
                        }
                    }

                    self.expect(TokenKind::CloseSquare)?;
                    node.add_child(array);
                }
                TokenKind::ColonColon => {
                    let mut namespace = Node::new_expr(ExpressionKind::Namespace);

                    self.tokens.next();
                    namespace.add_child(Node::from(self.expect(TokenKind::Identifier)?));

                    node.add_child(namespace);
                }
                _ => break,
            }
        }

        Ok(node)
    }
}
