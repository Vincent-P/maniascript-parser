use super::token_kind::TokenKind;
use super::trivia_kind::TriviaKind;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: usize,
    pub len: usize,

    pub line: usize,
    pub col: usize,

    pub leading_trivia: Box<[TriviaKind]>,
    pub trailing_trivia: Box<[TriviaKind]>,
}

impl Token {
    pub fn span(&self) -> (usize, usize) {
        (self.position, self.position + self.len)
    }

    pub fn lbp(&self) -> u32 {
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

    pub fn rbp(&self) -> u32 {
        match self.kind {
            TokenKind::OpenParen | TokenKind::OpenSquare => 1,

            TokenKind::Inf => 450,
            TokenKind::Minus | TokenKind::Not => 700,
            _ => 0,
        }
    }
}
