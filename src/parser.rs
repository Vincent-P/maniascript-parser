use crate::ast::{Tree, Node, NodeId};
use crate::ast::node_kind::{ExpressionKind, NodeKind};
use crate::lexer::Lexer;
use crate::lexer::token::Token;
use crate::lexer::token_kind::TokenKind;

use std::iter::Peekable;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    pub source: &'a str,
    pub tree: Tree
}

impl Token {
    fn nud(&self, parser: &mut Parser) -> Result<NodeId, String> {
        match self.kind {
            TokenKind::Identifier => {
                let node = Node::new(ExpressionKind::Identifier.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            k if k.is_litteral_value() => {
                let node = Node::new(ExpressionKind::Literal.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Minus | TokenKind::Not => {
                let node = Node::new(match self.kind {
                    TokenKind::Minus => ExpressionKind::Negative,
                    _ => ExpressionKind::Not,
                }.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                { let tmp = parser.expression(self.rbp())?; parser.tree.add_child_id(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::OpenParen => {
                let node = Node::new(NodeKind::Parenthesised);
                let node_id = parser.tree.add_node(node);
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                { let tmp = parser.expression(self.rbp())?; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(parser.expect(TokenKind::CloseParen)?); parser.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Inf => {
                let node = Node::new(ExpressionKind::Vector.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                { let tmp = parser.expression(self.rbp())?; parser.tree.add_child_id(node_id, tmp) };

                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    { let tmp = Node::from(parser.tokens.next().unwrap()); parser.tree.add_child(node_id, tmp) };
                    { let tmp = parser.expression(self.rbp())?; parser.tree.add_child_id(node_id, tmp) };
                }

                { let tmp = Node::from(parser.expect(TokenKind::Sup)?); parser.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::OpenSquare => {
                let node = Node::new(ExpressionKind::Array.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };

                if let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind == TokenKind::CloseSquare {
                        { let tmp = Node::from(parser.tokens.next().unwrap()); parser.tree.add_child(node_id, tmp) };
                        return Ok(node_id);
                    }
                }

                { let tmp = parser.expression(self.rbp())?; parser.tree.add_child_id(node_id, tmp) };

                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    { let tmp = Node::from(parser.tokens.next().unwrap()); parser.tree.add_child(node_id, tmp) };
                    { let tmp = parser.expression(self.rbp())?; parser.tree.add_child_id(node_id, tmp) };
                }

                { let tmp = Node::from(parser.expect(TokenKind::Sup)?); parser.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            k => Err(format!(
                "{}:{} Expected an expression but got {:?}.",
                self.line, self.col, k
            ))
        }
    }

    fn led(&self, parser: &mut Parser, lhs: NodeId) -> Result<NodeId, String> {
        match self.kind {
            k if k.is_binary_op() => {
                let node = Node::new(ExpressionKind::BinaryOp.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = lhs; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                // TODO(vincent): parse type
                { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::As => {
                let node = Node::new(ExpressionKind::Cast.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = lhs; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                // TODO(vincent): parse type
                { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Is => {
                let node = Node::new(ExpressionKind::Is.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = lhs; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                // TODO(vincent): parse type
                { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::OpenSquare => {
                let node = Node::new(ExpressionKind::ArrayAccess.into());
                let node_id = parser.tree.add_node(node);

                { let tmp = lhs; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(parser.expect(TokenKind::CloseSquare)?); parser.tree.add_child(node_id, tmp) };

                Ok(node_id)
            }

            TokenKind::OpenParen => {
                let node = Node::new(ExpressionKind::FunctionCall.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = lhs; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };

                if let Some(next) = parser.tokens.peek() {
                    if next.kind == TokenKind::CloseParen {
                        { let tmp = Node::from(parser.tokens.next().unwrap()); parser.tree.add_child(node_id, tmp) };
                        return Ok(node_id);
                    }
                }

                { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    { let tmp = Node::from(parser.tokens.next().unwrap()); parser.tree.add_child(node_id, tmp) };
                    { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                }

                { let tmp = Node::from(parser.expect(TokenKind::CloseParen)?); parser.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Arrow => {
                let node = Node::new(ExpressionKind::MapsTo.into());
                let node_id = parser.tree.add_node(node);
                { let tmp = lhs; parser.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self); parser.tree.add_child(node_id, tmp) };
                { let tmp = parser.expression(self.lbp())?; parser.tree.add_child_id(node_id, tmp) };
                Ok(node_id)
            }

            k => Err(format!(
                "{}:{} Expected an operator but got {:?}.",
                self.line, self.col, k
            ))
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let source = lexer.source;
        let tokens = lexer.peekable();
        let tree = Tree::new();
        Parser { tokens, source, tree }
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

    fn parse_nud(&mut self) -> Result<NodeId, String> {
        match self.tokens.next() {
            Some(t) => t.nud(self),
            _ => Err("Incomplete expression.".to_string()),
        }
    }

    fn parse_led(&mut self, expr: NodeId) -> Result<NodeId, String> {
        match self.tokens.next() {
            Some(t) => t.led(self, expr),
            _ => Err("Incomplete expression.".to_string())
        }
    }

    fn expression(&mut self, rbp: u32) -> Result<NodeId, String> {
        let mut left = self.parse_nud()?;

        while let Some(t) = self.tokens.peek() {
            if t.lbp() < rbp {
                break;
            }

            left = self.parse_led(left)?;
        }

        Ok(left)
    }

    pub fn parse_expr(&mut self) -> Result<NodeId, String> {
        self.expression(1)
    }

    pub fn parse_file(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::File);
        let node_id = self.tree.add_node(node);

        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_hash() {
                break;
            }
            { let tmp = self.parse_hash()?; self.tree.add_child_id(node_id, tmp) };
        }

        while let Some(next) = self.tokens.peek() {
            if next.kind != TokenKind::Declare {
                break;
            }
            { let tmp = self.parse_vardec()?; self.tree.add_child_id(node_id, tmp) };
        }

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::EOF => break,
                TokenKind::Declare => {
                    return Err("Globals need to be defined before functions.".to_string());
                }
                TokenKind::Identifier => {
                    { let tmp = self.parse_funcdec()?; self.tree.add_child_id(node_id, tmp) };
                }
                TokenKind::LabelStar => {
                    { let tmp = self.parse_labeldec()?; self.tree.add_child_id(node_id, tmp) };
                }
                k => {
                    return Err(format!(
                        "{}:{} Unexpected token {:?}.",
                        next.line, next.col, k
                    ));
                }
            }
        }

        Ok(node_id)
    }

    pub fn parse_hash(&mut self) -> Result<NodeId, String> {
        match self.tokens.peek().unwrap().kind {
            TokenKind::Include => {
                let node = Node::new(NodeKind::Include);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::LineString)?); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::As)?); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }
            TokenKind::Const => {
                let node = Node::new(NodeKind::Const);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
                { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
                Ok(node_id)
            }
            TokenKind::Setting => {
                let node = Node::new(NodeKind::Setting);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
                { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };

                if let Some(next) = self.tokens.peek() {
                    if next.kind == TokenKind::As {
                        { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
                    }
                }

                Ok(node_id)
            }
            TokenKind::RequireContext => {
                let node = Node::new(NodeKind::RequireContext);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }
            TokenKind::Extends => {
                let node = Node::new(NodeKind::Extends);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::LineString)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }
            _ => Err("Expected Include, Const, Setting, RequireContext or Extends.".to_string())
        }
    }

    pub fn parse_funcdec(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::FuncDec);
        let node_id = self.tree.add_node(node);

        //TODO(vincent): parse type
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };

        match self.tokens.peek() {
            Some(name) if name.kind == TokenKind::Identifier => {
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
            },
            _ => {}
        }

        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };

        while let Some(next) = self.tokens.peek() {
            if next.kind != TokenKind::Identifier {
                break;
            }
            { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
            { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };

            match self.tokens.peek() {
                Some(t) if t.kind == TokenKind::Comma => { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) },
                _ => break,
            };
        }

        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_block()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_labeldec(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::LabelDec);
        let node_id = self.tree.add_node(node);

        { let tmp = Node::from(self.expect(TokenKind::LabelStar)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::LabelStar)?); self.tree.add_child(node_id, tmp) };

        { let tmp = Node::from(self.expect(TokenKind::LabelStar)?); self.tree.add_child(node_id, tmp) };

        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        while let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::LabelStar {
                break;
            }
            { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        }

        { let tmp = Node::from(self.expect(TokenKind::LabelStar)?); self.tree.add_child(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_block(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::Block);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::OpenBrace)?); self.tree.add_child(node_id, tmp) };

        while let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::CloseBrace {
                break;
            }
            { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        }

        { let tmp = Node::from(self.expect(TokenKind::CloseBrace)?); self.tree.add_child(node_id, tmp) };
        Ok(node_id)
    }

    // TODO(vincent): make different nodes based on the assignment
    pub fn parse_vardec(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::VarDec);
        let node_id = self.tree.add_node(node);

        { let tmp = Node::from(self.expect(TokenKind::Declare)?); self.tree.add_child(node_id, tmp) };

        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_decl_metadata() {
                break;
            }
            { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
        }

        //TODO(vincent): parse type
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };

        match self.tokens.peek() {
            Some(name) if name.kind == TokenKind::Identifier => {
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
            },
            _ => {}
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::As {
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
            }
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::For {
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
            }
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind.is_assign_op() {
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
            }
        }

        { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };

        Ok(node_id)
    }

    pub fn parse_else(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::Else);
        let node_id = self.tree.add_node(node);

        { let tmp = Node::from(self.expect(TokenKind::Else)?); self.tree.add_child(node_id, tmp) };

        match self.tokens.peek() {
            Some(t) if t.kind == TokenKind::If => { let tmp = self.parse_if()?; self.tree.add_child_id(node_id, tmp) },
            _ => { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) }
        };

        Ok(node_id)
    }

    pub fn parse_if(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::If);
        let node_id = self.tree.add_node(node);

        { let tmp = Node::from(self.expect(TokenKind::If)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Else {
                { let tmp = self.parse_else()?; self.tree.add_child_id(node_id, tmp) };
            }
        }

        Ok(node_id)
    }

    pub fn parse_statement(&mut self) -> Result<NodeId, String> {
        match self.tokens.peek().unwrap().kind {
            TokenKind::OpenBrace => self.parse_block(),
            TokenKind::Declare => self.parse_vardec(),
            TokenKind::If => self.parse_if(),
            TokenKind::Switch => self.parse_switch(),
            TokenKind::SwitchType => self.parse_switchtype(),
            TokenKind::For => self.parse_for(),
            TokenKind::Foreach => self.parse_foreach(),
            TokenKind::While => self.parse_while(),

            TokenKind::LabelPlus => {
                let node = Node::new(NodeKind::LabelCall);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::LabelPlus)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::LabelMinus => {
                let node = Node::new(NodeKind::LabelCall);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::LabelMinus)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Break => {
                let node = Node::new(NodeKind::Break);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Continue => {
                let node = Node::new(NodeKind::Continue);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Yield => {
                let node = Node::new(NodeKind::Yield);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            TokenKind::Return => {
                let node = Node::new(NodeKind::Return);
                let node_id = self.tree.add_node(node);
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };

                if let Some(next) = self.tokens.peek() {
                    if next.kind != TokenKind::Semicolon {
                        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
                    }
                }

                { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }

            _ => {
                let expr_id = self.parse_expr()?;

                if let Some(next) = self.tokens.peek() {
                    if next.kind.is_assign_op() {
                        let node = Node::new(NodeKind::Assignment);
                        let node_id = self.tree.add_node(node);
                        { let tmp = expr_id; self.tree.add_child_id(node_id, tmp) };
                        { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
                        { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };
                        return Ok(node_id);
                    }
                }

                let node = Node::new(NodeKind::ExprStatement);
                let node_id = self.tree.add_node(node);
                { let tmp = expr_id; self.tree.add_child_id(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Semicolon)?); self.tree.add_child(node_id, tmp) };
                Ok(node_id)
            }
        }
    }

    pub fn parse_switch(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::Switch);
        let node_id = self.tree.add_node(node);

        { let tmp = Node::from(self.expect(TokenKind::Switch)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenBrace)?); self.tree.add_child(node_id, tmp) };

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => { let tmp = self.parse_default()?; self.tree.add_child_id(node_id, tmp) },
                TokenKind::Case => { let tmp = self.parse_case()?; self.tree.add_child_id(node_id, tmp) },
                _ => break,
            };
        }

        { let tmp = Node::from(self.expect(TokenKind::CloseBrace)?); self.tree.add_child(node_id, tmp) };

        Ok(node_id)
    }
    pub fn parse_switchtype(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::SwitchType);
        let node_id = self.tree.add_node(node);

        { let tmp = Node::from(self.expect(TokenKind::SwitchType)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenBrace)?); self.tree.add_child(node_id, tmp) };

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => { let tmp = self.parse_default()?; self.tree.add_child_id(node_id, tmp) },
                TokenKind::Case => { let tmp = self.parse_case_type()?; self.tree.add_child_id(node_id, tmp) },
                _ => break,
            };
        }

        { let tmp = Node::from(self.expect(TokenKind::CloseBrace)?); self.tree.add_child(node_id, tmp) };

        Ok(node_id)
    }

    pub fn parse_case(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::Case);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::Case)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Colon)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_case_type(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::CaseType);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::Case)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Colon)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_default(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::Default);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::Default)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Colon)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_for(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::For);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::For)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Comma)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Comma)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_foreach(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::Foreach);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::Foreach)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Arrow {
                { let tmp = Node::from(self.tokens.next().unwrap()); self.tree.add_child(node_id, tmp) };
                { let tmp = Node::from(self.expect(TokenKind::Identifier)?); self.tree.add_child(node_id, tmp) };
            }
        }

        { let tmp = Node::from(self.expect(TokenKind::In)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }

    pub fn parse_while(&mut self) -> Result<NodeId, String> {
        let node = Node::new(NodeKind::While);
        let node_id = self.tree.add_node(node);
        { let tmp = Node::from(self.expect(TokenKind::While)?); self.tree.add_child(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::OpenParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_expr()?; self.tree.add_child_id(node_id, tmp) };
        { let tmp = Node::from(self.expect(TokenKind::CloseParen)?); self.tree.add_child(node_id, tmp) };
        { let tmp = self.parse_statement()?; self.tree.add_child_id(node_id, tmp) };
        Ok(node_id)
    }
}
