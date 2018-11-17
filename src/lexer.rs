use crate::token_kind::TokenKind;
use crate::trivia_kind::TriviaKind;
use std::str::FromStr;

pub struct Lexer<'a> {
    source: &'a str,
    position: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: usize,
    pub len: usize,
    pub leading_trivia: Box<[TriviaKind]>,
    pub trailing_trivia: Box<[TriviaKind]>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer<'_> {
        Lexer {
            source,
            position: 0,
        }
    }

    fn char_next(&mut self) -> Option<char> {
        match self.source[self.position..].chars().next() {
            Some(c) => {
                self.position += c.len_utf8();
                Some(c)
            }
            _ => None,
        }
    }

    fn char_peek(&mut self) -> Option<char> {
        self.source[self.position..].chars().next()
    }

    fn char_peek_n(&mut self, n: usize) -> Option<char> {
        let mut chars = self.source[self.position..].chars();
        for _ in 0..n - 1 {
            chars.next();
        }
        chars.next()
    }

    fn scan_word(&mut self) -> TokenKind {
        let start = self.position;

        if let Some('#') = self.char_peek() {
            self.char_next();
        }

        while let Some(c) = self.char_peek() {
            if !is_ident_char(c) {
                break;
            }
            self.char_next();
        }

        let token = &self.source[start..self.position];

        match TokenKind::from_str(token) {
            Result::Ok(t) => t,
            _ => TokenKind::Identifier,
        }
    }

    fn scan_number(&mut self) -> TokenKind {
        let mut is_real = false;
        while let Some(c) = self.char_peek() {
            if !c.is_numeric() && c != '.' {
                break;
            }
            if c == '.' {
                is_real = true;
            }
            self.char_next();
        }
        if is_real {
            TokenKind::Real
        } else {
            TokenKind::Integer
        }
    }

    fn scan_operator(&mut self) -> TokenKind {
        for i in 1..4 {
            if self.position + (4 - i) > self.source.len() {
                continue;
            }

            let end = self.position + 4 - i * '='.len_utf8();
            let token = &self.source[self.position..end];

            let res = TokenKind::from_str_or_unknown(token);
            if res != TokenKind::Unknown {
                self.position = end;
                return res;
            }
        }

        self.position += 1;
        TokenKind::Unknown
    }

    fn scan_string(&mut self) -> TokenKind {
        self.char_next();
        match (self.char_peek(), self.char_peek_n(2)) {
            // multiline string
            (Some('"'), Some('"')) => {
                self.char_next();
                self.char_next();

                while let Some(_) = self.char_peek() {
                    match (self.char_next(), self.char_peek(), self.char_peek_n(2)) {
                        (Some('"'), Some('"'), Some('"')) => {
                            self.char_next();
                            self.char_next();
                            break;
                        }
                        _ => {}
                    }
                }
                TokenKind::BlockString
            }
            // line string
            _ => {
                while let Some(_) = self.char_peek() {
                    match (self.char_next(), self.char_peek()) {
                        (Some(c), Some('"')) if c != '\\' => {
                            self.char_next();
                            break;
                        }
                        (Some('"'), _) | (_, Some('\n')) => {
                            break;
                        }
                        _ => {
                            self.char_next();
                        }
                    }
                }
                TokenKind::LineString
            }
        }
    }

    fn scan_trivias(&mut self, is_trailing: bool) -> Vec<TriviaKind> {
        let mut trivias = vec![];

        loop {
            let start = self.position;

            let trivia = match (self.char_peek(), self.char_peek_n(2)) {
                (Some('\r'), Some('\n')) if !is_trailing => {
                    self.char_next();
                    self.char_next();
                    TriviaKind::CarriageReturnNewline(1)
                }
                (Some('\r'), _) if !is_trailing => {
                    self.char_next();
                    TriviaKind::CarriageReturn(1)
                }
                (Some('\n'), _) if !is_trailing => {
                    self.char_next();
                    TriviaKind::Newline(1)
                }
                (Some('\x09'), _) => {
                    self.char_next();
                    TriviaKind::Tab(1)
                }
                (Some('\x0b'), _) if !is_trailing => {
                    self.char_next();
                    TriviaKind::VerticalTab(1)
                }
                (Some('\x0c'), _) if !is_trailing => {
                    self.char_next();
                    TriviaKind::FormFeed(1)
                }
                (Some(' '), _) => {
                    self.char_next();
                    TriviaKind::Space(1)
                }
                (Some('/'), Some('/')) => {
                    self.char_next();
                    self.char_next();
                    while let Some(c) = self.char_peek() {
                        if c == '\n' {
                            break;
                        }
                        self.char_next();
                    }
                    let comment = &self.source[start..self.position];
                    TriviaKind::LineComment(String::from(comment))
                }
                (Some('/'), Some('*')) => {
                    self.char_next();
                    self.char_next();
                    while let (Some(c1), Some(c2)) = (self.char_peek(), self.char_peek_n(2)) {
                        self.char_next();
                        if c1 == '*' && c2 == '/' {
                            self.char_next();
                            break;
                        }
                    }
                    let comment = &self.source[start..self.position];
                    TriviaKind::BlockComment(String::from(comment))
                }
                _ => {
                    break;
                }
            };

            // count trivias
            match (trivia, trivias.last_mut()) {
                (TriviaKind::Space(_), Some(TriviaKind::Space(last_sp))) => *last_sp += 1,
                (TriviaKind::Tab(_), Some(TriviaKind::Space(last_sp))) => *last_sp += 1,
                (TriviaKind::Newline(_), Some(TriviaKind::Space(last_sp))) => *last_sp += 1,
                (TriviaKind::VerticalTab(_), Some(TriviaKind::Space(last_sp))) => *last_sp += 1,
                (TriviaKind::FormFeed(_), Some(TriviaKind::Space(last_sp))) => *last_sp += 1,
                (TriviaKind::CarriageReturn(_), Some(TriviaKind::Space(last_sp))) => *last_sp += 1,
                (TriviaKind::CarriageReturnNewline(_), Some(TriviaKind::Space(last_sp))) => {
                    *last_sp += 1
                }
                (t, _) => trivias.push(t),
            }
        }
        trivias
    }

    fn scan_token(&mut self) -> TokenKind {
        match self.char_peek() {
            Some(c) if is_punctuation_char(c) => self.scan_operator(),
            Some(c) if c.is_numeric() => self.scan_number(),
            Some(c) if c == '#' || is_ident_char(c) => self.scan_word(),
            Some(c) if c == '"' => self.scan_string(),
            Some(c) => {
                let token = &self.source[self.position..self.position + c.len_utf8()];
                self.position += c.len_utf8();
                TokenKind::from_str_or_unknown(token)
            }
            None => {
                self.position += 1;
                TokenKind::EOF
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.position > self.source.len() {
            return None;
        }

        let start = self.position;
        let leading_trivia = self.scan_trivias(false);
        let token_kind = self.scan_token();

        // Maybe the EOF is reached so the position > len
        let trailing_trivias = if self.position < self.source.len() {
            self.scan_trivias(true)
        } else {
            vec![]
        };

        Some(Token {
            kind: token_kind,
            position: start,
            len: self.position - start,
            leading_trivia: leading_trivia.into_boxed_slice(),
            trailing_trivia: trailing_trivias.into_boxed_slice(),
        })
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_punctuation_char(c: char) -> bool {
    c == ':'
        || c == '!'
        || c == '&'
        || c == '|'
        || c == '<'
        || c == '>'
        || c == '='
        || c == '-'
        || c == '+'
        || c == '-'
        || c == '*'
        || c == '/'
        || c == '%'
        || c == '^'
}
