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
}

impl ExpressionKind {
    pub fn display_rec(self, parser: &Parser, level: usize) {
        for _ in 0..level {
            print!("--");
        }
        match self {
            ExpressionKind::Unary(tok, exp) => {
                println!("{:?}", tok.kind);
                exp.display_rec(parser, level + 1);
            }
            ExpressionKind::Binary(exp1, tok, exp2) => {
                println!("{:?}", tok.kind);
                exp1.display_rec(parser, level + 1);
                exp2.display_rec(parser, level + 1);
            }
            ExpressionKind::ArrayAccess(arr_exp, ind_exp) => {
                println!("Array");
                arr_exp.display_rec(parser, level);
                ind_exp.display_rec(parser, level);
            }
            ExpressionKind::FunctionCall(function_exp, args) => {
                println!("{:?}", function_exp);
                println!("{:?}", args);
            }
            ExpressionKind::Cast(exp, type_tok) => {
                println!("as {:?}", type_tok.kind);
                exp.display_rec(parser, level);
            }
            _ => {
                println!("{:?}", &self);
            }
        }
    }
}

impl Token {
    fn lbp(&self) -> u32 {
        match self.kind {
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

            TokenKind::Dot
            | TokenKind::OpenParen
            | TokenKind::OpenSquare
            | TokenKind::ColonColon => 700,

            _ => 0,
        }
    }

    fn rbp(&self) -> u32 {
        match self.kind {
            TokenKind::OpenParen | TokenKind::OpenSquare => 0,
            TokenKind::Inf => 400,
            TokenKind::Minus | TokenKind::Not => 800,
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

            _ => Err(format!(
                "Expected an expression. {:?}",
                parser.tokens.peek()
            )),
        }
    }

    fn led(&self, parser: &mut Parser, lhs: ExpressionKind) -> Result<ExpressionKind, String> {
        match self.kind {
            k if k.is_binary_op() => {
                println!("Found binary op: {:?}", self);
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
                let rhs = parser.expression(self.lbp())?;
                let mut args = vec![rhs];
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

            k => Err(format!("Expected an operator but got {:?}.", k)),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let tokens = lexer.peekable();
        Parser { tokens }
    }

    fn expect(&mut self, token_kind: TokenKind) -> Result<Token, String> {
        match self.tokens.peek() {
            Some(ref t) if t.kind == token_kind => Ok(self.tokens.next().unwrap()),
            Some(ref t) => Err(format!(
                "Expected {:?} token but got {:?}.",
                token_kind, t.kind
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

    pub fn expression(&mut self, rbp: u32) -> Result<ExpressionKind, String> {
        let mut left = self.parse_nud()?;

        while let Some(t) = self.tokens.peek() {
            if t.lbp() <= rbp {
                break;
            }

            left = self.parse_led(left)?;
        }

        Ok(left)
    }
}
