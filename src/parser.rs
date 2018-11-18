use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::token_kind::TokenKind;
use std::iter::Peekable;

#[derive(Debug)]
pub enum ExpressionKind {
    Identifier,
    Literal,
    Unary(Token, Box<ExpressionKind>),
    Binary(Box<ExpressionKind>, Token, Box<ExpressionKind>),
    Array(Vec<ExpressionKind>),
    Vector(Vec<ExpressionKind>),
    ArrayAccess(Box<ExpressionKind>, Box<ExpressionKind>),
    FunctionCall(Box<ExpressionKind>, Vec<ExpressionKind>),
    Cast(Box<ExpressionKind>, Token),
    Is(Box<ExpressionKind>, Token),
    MapsTo(Box<ExpressionKind>, Box<ExpressionKind>),
}

impl Token {
    fn lbp(&self) -> u32 {
        match self.kind {
            TokenKind::Arrow => 5,

            TokenKind::OpenParen | TokenKind::OpenSquare => 51,

            TokenKind::StrConcat => 100,

            TokenKind::And => 200,
            TokenKind::Or => 300,

            TokenKind::EqualEqual
            | TokenKind::NotEqual
            | TokenKind::Inf
            | TokenKind::InfEq
            | TokenKind::Sup
            | TokenKind::SupEq => 400,

            TokenKind::Plus | TokenKind::Minus => 500,
            TokenKind::Mult | TokenKind::Div | TokenKind::Modulo => 600,

            TokenKind::Is | TokenKind::As => 650,

            TokenKind::Dot | TokenKind::ColonColon => 800,

            _ => 0,
        }
    }

    fn rbp(&self) -> u32 {
        match self.kind {
            TokenKind::OpenParen | TokenKind::OpenSquare => 1,

            TokenKind::Inf => 450,
            TokenKind::Minus | TokenKind::Not => 700,
            _ => 0,
        }
    }

    fn nud(&self, parser: &mut Parser) -> Result<ExpressionKind, String> {
        match self.kind {
            TokenKind::Identifier => Ok(ExpressionKind::Identifier),

            k if k.is_litteral_value() => Ok(ExpressionKind::Literal),

            TokenKind::Minus | TokenKind::Not => {
                let rhs = parser.expression(self.rbp())?;
                Ok(ExpressionKind::Unary(self.clone(), Box::new(rhs)))
            }

            TokenKind::OpenParen => {
                let rhs = parser.expression(self.rbp())?;
                parser.expect(TokenKind::CloseParen)?;
                Ok(rhs)
            }

            TokenKind::Inf => {
                let mut values = vec![];
                if let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind == TokenKind::Sup {
                        parser.tokens.next();
                        return Ok(ExpressionKind::Vector(values));
                    }
                }

                values.push(parser.expression(self.rbp())?);
                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    parser.tokens.next();
                    values.push(parser.expression(self.rbp())?);
                }
                parser.expect(TokenKind::Sup)?;
                Ok(ExpressionKind::Vector(values))
            }

            TokenKind::OpenSquare => {
                let mut values = vec![];
                if let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind == TokenKind::CloseSquare {
                        parser.tokens.next();
                        return Ok(ExpressionKind::Array(values));
                    }
                }

                values.push(parser.expression(self.rbp())?);
                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    parser.tokens.next();
                    values.push(parser.expression(self.rbp())?);
                }
                parser.expect(TokenKind::CloseSquare)?;
                Ok(ExpressionKind::Array(values))
            }

            k => Err(format!(
                "Expected an expression but got {:?}.
            At : {}",
                k,
                &parser.source[self.position..]
            )),
        }
    }

    fn led(&self, parser: &mut Parser, lhs: ExpressionKind) -> Result<ExpressionKind, String> {
        match self.kind {
            k if k.is_binary_op() => {
                let rhs = parser.expression(self.lbp())?;
                Ok(ExpressionKind::Binary(
                    Box::new(lhs),
                    self.clone(),
                    Box::new(rhs),
                ))
            }

            TokenKind::As => {
                let type_token = parser.expect(TokenKind::Identifier)?;
                Ok(ExpressionKind::Cast(Box::new(lhs), type_token.clone()))
            }

            TokenKind::Is => {
                let type_token = parser.expect(TokenKind::Identifier)?;
                Ok(ExpressionKind::Is(Box::new(lhs), type_token.clone()))
            }

            TokenKind::OpenSquare => {
                let rhs = parser.expression(self.lbp())?;
                parser.expect(TokenKind::CloseSquare)?;
                Ok(ExpressionKind::ArrayAccess(Box::new(lhs), Box::new(rhs)))
            }

            TokenKind::OpenParen => {
                let mut args = vec![];
                if let Some(next) = parser.tokens.peek() {
                    if next.kind == TokenKind::CloseParen {
                        parser.tokens.next();
                        return Ok(ExpressionKind::FunctionCall(Box::new(lhs), args));
                    }
                }

                args.push(parser.expression(self.lbp())?);
                while let Some(next_token) = parser.tokens.peek() {
                    if next_token.kind != TokenKind::Comma {
                        break;
                    }
                    parser.tokens.next();
                    args.push(parser.expression(self.lbp())?);
                }
                parser.expect(TokenKind::CloseParen)?;
                Ok(ExpressionKind::FunctionCall(Box::new(lhs), args))
            }

            TokenKind::Arrow => {
                let rhs = parser.expression(self.lbp())?;
                Ok(ExpressionKind::MapsTo(Box::new(lhs), Box::new(rhs)))
            }

            k => Err(format!(
                "Expected an operator but got {:?}.
            At : {}",
                k,
                &parser.source[self.position..]
            )),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    pub source: &'a str,
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
                "Expected {:?} token but got {:?}.
                At : {}",
                token_kind,
                t.kind,
                &self.source[t.position..]
            )),
            _ => Err(format!("Expected {:?} token.", token_kind)),
        }
    }

    fn parse_nud(&mut self) -> Result<ExpressionKind, String> {
        match self.tokens.next() {
            Some(t) => t.nud(self),
            _ => Err("Incomplete expression.".to_string()),
        }
    }

    fn parse_led(&mut self, expr: ExpressionKind) -> Result<ExpressionKind, String> {
        match self.tokens.next() {
            Some(t) => t.led(self, expr),
            _ => Err("Incomplete expression.".to_string()),
        }
    }

    fn expression(&mut self, rbp: u32) -> Result<ExpressionKind, String> {
        let mut left = self.parse_nud()?;

        while let Some(t) = self.tokens.peek() {
            if t.lbp() < rbp {
                break;
            }

            left = self.parse_led(left)?;
        }

        Ok(left)
    }

    pub fn parse_expr(&mut self) -> Result<ExpressionKind, String> {
        self.expression(1)
    }

    pub fn parse_file(&mut self) -> Result<(), String> {
        let mut _hash_statements = vec![];
        let mut _vardecs = vec![];
        let mut _funcdecs = vec![];
        let mut _labeldecs = vec![];

        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_hash() {
                break;
            }
            _hash_statements.push(self.parse_hash()?);
        }

        while let Some(next) = self.tokens.peek() {
            if next.kind != TokenKind::Declare {
                break;
            }
            _vardecs.push(self.parse_vardec()?);
        }

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::EOF => break,
                TokenKind::Declare => {
                    return Err("Globals need to be defined before functions.".to_string())
                }
                TokenKind::Identifier => {
                    _funcdecs.push(self.parse_funcdec()?);
                }
                TokenKind::LabelStar => {
                    _labeldecs.push(self.parse_labeldec()?);
                }
                k => {
                    return Err(format!(
                        "Unexpected token {:?}.
                At: {}",
                        k,
                        &self.source[next.position..]
                    ))
                }
            }
        }

        Ok(())
    }

    pub fn parse_hash(&mut self) -> Result<(), String> {
        let kind = match self.tokens.peek() {
            Some(t) if t.kind.is_hash() => t.kind,
            _ => return Err("Expected a hash statement.".to_string()),
        };
        match kind {
            TokenKind::Include => {
                self.tokens.next();
                let _path = self.expect(TokenKind::LineString)?;
                self.expect(TokenKind::As)?;
                let _identifier = self.expect(TokenKind::Identifier)?;
                Ok(())
            }
            TokenKind::Const => {
                self.tokens.next();
                let _identifier = self.expect(TokenKind::Identifier)?;
                let _value = self.parse_expr()?;
                Ok(())
            }
            TokenKind::Setting => {
                self.tokens.next();
                let _identifier = self.expect(TokenKind::Identifier)?;
                let _value = self.parse_expr()?;
                let mut _alias = None;
                if let Some(next) = self.tokens.peek() {
                    if next.kind == TokenKind::As {
                        self.tokens.next();
                        _alias = self.parse_expr()?.into();
                    }
                }
                Ok(())
            }
            TokenKind::RequireContext => {
                self.tokens.next();
                let _identifier = self.expect(TokenKind::Identifier)?;
                Ok(())
            }
            TokenKind::Extends => {
                self.tokens.next();
                let _path = self.expect(TokenKind::LineString);
                Ok(())
            }
            _ => Err("Expected Include, Const, Setting, RequireContext or Extends.".to_string()),
        }
    }

    pub fn parse_funcdec(&mut self) -> Result<(), String> {
        let mut _type: Option<Token> = None;
        let mut _name = self.expect(TokenKind::Identifier)?;

        if let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Identifier => {
                    _type = Some(_name);
                    _name = self.expect(TokenKind::Identifier)?;
                }
                TokenKind::ColonColon | TokenKind::OpenSquare => {
                    _type = self.parse_type(Some(_name))?.into();
                    _name = self.expect(TokenKind::Identifier)?;
                }
                _ => {}
            }
        }

        let mut _args = vec![];
        self.expect(TokenKind::OpenParen)?;
        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Identifier {
                let mut _arg_type = self.parse_type(None)?;
                let mut _arg_name = self.expect(TokenKind::Identifier)?;
                _args.push((_arg_type, _arg_name));
                while let Some(next) = self.tokens.peek() {
                    if next.kind != TokenKind::Comma {
                        break;
                    }
                    self.tokens.next();
                    _arg_type = self.parse_type(None)?;
                    _arg_name = self.expect(TokenKind::Identifier)?;
                    _args.push((_arg_type, _arg_name));
                }
            }
        }

        self.expect(TokenKind::CloseParen)?;
        let _block = self.parse_block()?;
        Ok(())
    }

    pub fn parse_labeldec(&mut self) -> Result<(), String> {
        self.expect(TokenKind::LabelStar)?;
        let _identifier = self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::LabelStar)?;

        self.expect(TokenKind::LabelStar)?;

        let mut _statements = vec![];
        _statements.push(self.parse_statement()?);
        while let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::LabelStar {
                break;
            }
            _statements.push(self.parse_statement()?);
        }

        self.expect(TokenKind::LabelStar)?;
        Ok(())
    }

    pub fn parse_block(&mut self) -> Result<(), String> {
        let mut _statements = vec![];
        self.expect(TokenKind::OpenBrace)?;

        while let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::CloseBrace {
                break;
            }
            _statements.push(self.parse_statement()?);
        }

        self.expect(TokenKind::CloseBrace)?;
        Ok(())
    }

    pub fn parse_vardec(&mut self) -> Result<(), String> {
        self.expect(TokenKind::Declare)?;
        while let Some(next) = self.tokens.peek() {
            if !next.kind.is_decl_metadata() {
                break;
            }
            let _metadata = self.tokens.next().unwrap();
        }

        let mut _type: Option<Token> = None;
        let mut _name = self.expect(TokenKind::Identifier)?;
        let mut _name_alias: Option<Token> = None;
        let mut _for: Option<ExpressionKind> = None;
        let mut _assign_tok: Option<Token> = None;
        let mut _value: Option<ExpressionKind> = None;

        if let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Identifier => {
                    _type = Some(_name);
                    _name = self.tokens.next().unwrap();
                }
                TokenKind::ColonColon | TokenKind::OpenSquare => {
                    _type = self.parse_type(Some(_name))?.into();
                    _name = self.expect(TokenKind::Identifier)?;
                }
                _ => {}
            }
        }

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::As {
                self.tokens.next();
                _name_alias = self.expect(TokenKind::Identifier)?.into();
            }
        }
        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::For {
                self.tokens.next();
                _for = self.parse_expr()?.into();
            }
        }
        if let Some(next) = self.tokens.peek() {
            if next.kind.is_assign_op() {
                _assign_tok = self.tokens.next().unwrap().into();
                _value = self.parse_expr()?.into();
            }
        }
        self.expect(TokenKind::Semicolon)?;

        if _type.is_none() && _value.is_none() {
            Err("The variable declaration needs a type or a default value.".to_string())
        } else {
            Ok(())
        }
    }

    pub fn parse_if(&mut self) -> Result<(), String> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::OpenParen)?;
        let _condition = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;
        let _body = self.parse_statement()?;

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Else {
                self.tokens.next();
                let mut _else_body: Option<_> = None;

                if let Some(next) = self.tokens.peek() {
                    if next.kind == TokenKind::If {
                        _else_body = self.parse_if()?.into();
                    }
                }
                if _else_body.is_none() {
                    _else_body = self.parse_statement()?.into();
                }
            }
        }

        Ok(())
    }

    pub fn parse_statement(&mut self) -> Result<(), String> {
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
                self.tokens.next();
                let _identfier = self.expect(TokenKind::Identifier)?;
                self.expect(TokenKind::LabelPlus)?;
                Ok(())
            }

            TokenKind::LabelMinus => {
                self.tokens.next();
                let _identfier = self.expect(TokenKind::Identifier)?;
                self.expect(TokenKind::LabelMinus)?;
                Ok(())
            }

            TokenKind::Break => {
                self.tokens.next();
                self.expect(TokenKind::Semicolon)?;
                Ok(())
            }

            TokenKind::Continue => {
                self.tokens.next();
                self.expect(TokenKind::Semicolon)?;
                Ok(())
            }

            TokenKind::Return => {
                self.tokens.next();
                if let Some(next) = self.tokens.peek() {
                    if next.kind == TokenKind::Semicolon {
                        self.tokens.next();
                        return Ok(());
                    }
                }
                let _expr = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(())
            }

            _ => {
                let _expr = self.parse_expr()?;
                if let Some(next) = self.tokens.peek() {
                    if next.kind.is_assign_op() {
                        let _assign_op = self.tokens.next().unwrap();
                        let _value = self.parse_expr()?;
                    }
                }
                self.expect(TokenKind::Semicolon)?;
                Ok(())
            }
        }
    }

    pub fn parse_switch(&mut self) -> Result<(), String> {
        self.expect(TokenKind::Switch)?;
        self.expect(TokenKind::OpenParen)?;
        let _value = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::OpenBrace)?;

        let mut _cases = vec![];
        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => _cases.push(self.parse_default()?),
                TokenKind::Case => _cases.push(self.parse_case()?),
                _ => break,
            };
        }

        self.expect(TokenKind::CloseBrace)?;

        Ok(())
    }
    pub fn parse_switchtype(&mut self) -> Result<(), String> {
        self.expect(TokenKind::SwitchType)?;
        self.expect(TokenKind::OpenParen)?;
        let _value = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::OpenBrace)?;

        let mut _cases = vec![];
        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::Default => _cases.push(self.parse_default()?),
                TokenKind::Case => _cases.push(self.parse_case_type()?),
                _ => break,
            };
        }

        self.expect(TokenKind::CloseBrace)?;

        Ok(())
    }

    pub fn parse_case(&mut self) -> Result<(), String> {
        self.expect(TokenKind::Case)?;
        let _condition = self.parse_expr()?;
        self.expect(TokenKind::Colon)?;
        let _body = self.parse_statement()?;
        Ok(())
    }

    pub fn parse_case_type(&mut self) -> Result<(), String> {
        self.expect(TokenKind::Case)?;
        let _type = self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Colon)?;
        let _body = self.parse_statement()?;
        Ok(())
    }

    pub fn parse_default(&mut self) -> Result<(), String> {
        self.expect(TokenKind::Default)?;
        self.expect(TokenKind::Colon)?;
        let _body = self.parse_statement()?;
        Ok(())
    }

    pub fn parse_for(&mut self) -> Result<(), String> {
        self.expect(TokenKind::For)?;
        self.expect(TokenKind::OpenParen)?;
        let _identifier = self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Comma)?;
        let _start_expr = self.parse_expr()?;
        self.expect(TokenKind::Comma)?;
        let _end_expr = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;
        let _body = self.parse_statement()?;
        Ok(())
    }

    pub fn parse_foreach(&mut self) -> Result<(), String> {
        self.expect(TokenKind::Foreach)?;
        self.expect(TokenKind::OpenParen)?;
        let _key = self.expect(TokenKind::Identifier)?;
        let mut _value: Option<Token> = None;

        if let Some(next) = self.tokens.peek() {
            if next.kind == TokenKind::Arrow {
                self.tokens.next();
                _value = self.expect(TokenKind::Identifier)?.into();
            }
        }

        self.expect(TokenKind::In)?;
        let _array = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;

        let _body = self.parse_statement()?;
        Ok(())
    }

    pub fn parse_while(&mut self) -> Result<(), String> {
        self.expect(TokenKind::While)?;
        self.expect(TokenKind::OpenParen)?;
        let _condition = self.parse_expr()?;
        self.expect(TokenKind::CloseParen)?;
        let _body = self.parse_statement()?;
        Ok(())
    }

    pub fn parse_type(&mut self, identifier: Option<Token>) -> Result<Token, String> {
        let mut _type = match identifier {
            Some(t) => t,
            None => self.expect(TokenKind::Identifier)?.into(),
        };

        while let Some(next) = self.tokens.peek() {
            match next.kind {
                TokenKind::OpenSquare => {
                    self.tokens.next();
                    if let Some(following) = self.tokens.peek() {
                        if following.kind == TokenKind::Identifier {
                            _type = self.tokens.next().unwrap().into();
                        }
                    }
                    self.expect(TokenKind::CloseSquare)?;
                }
                TokenKind::ColonColon => {
                    self.tokens.next();
                    _type = self.expect(TokenKind::Identifier)?;
                }
                _ => break,
            }
        }

        Ok(_type)
    }
}
