pub mod token;
pub mod token_kind;
pub mod trivia_kind;

use std::str::FromStr;
use token::Token;
use token_kind::TokenKind;
use trivia_kind::TriviaKind;

pub struct Lexer<'a> {
    pub source: &'a str,
    position: usize,
    lines: usize,
    last_line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer<'_> {
        Lexer {
            source,
            position: 0,
            lines: 1,
            last_line: 0,
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
        let mut is_hash = false;

        if let Some('#') = self.char_peek() {
            self.char_next();
            is_hash = true;
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
            _ if is_hash => TokenKind::Unknown,
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
                    match (
                        self.char_next().unwrap(),
                        self.char_peek(),
                        self.char_peek_n(2),
                    ) {
                        ('"', Some('"'), Some('"')) => {
                            self.char_next();
                            self.char_next();
                            break;
                        }
                        ('\r', Some('\n'), _) => {
                            self.char_next();
                            self.last_line = self.position;
                            self.lines += 1;
                        }
                        ('\x0c', _, _) | ('\n', _, _) | ('\r', _, _) | ('\x0b', _, _) => {
                            self.last_line = self.position;
                            self.lines += 1;
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
                    while let (Some(_), Some(c2)) = (self.char_peek(), self.char_peek_n(2)) {
                        match (self.char_next().unwrap(), c2) {
                            ('*', '/') => {
                                self.char_next();
                                break;
                            }
                            ('\r', '\n') => {
                                self.char_next();
                                self.last_line = self.position;
                                self.lines += 1;
                            }
                            ('\x0c', _) | ('\n', _) | ('\r', _) | ('\x0b', _) => {
                                self.last_line = self.position;
                                self.lines += 1;
                            }
                            _ => {}
                        }
                    }
                    let comment = &self.source[start..self.position];
                    TriviaKind::BlockComment(String::from(comment))
                }
                _ => {
                    break;
                }
            };

            if trivia.is_line_break() {
                self.lines += 1;
                self.last_line = self.position;
            }

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
        let following = match self.char_peek_n(2) {
            Some(c) => c,
            _ => 'a',
        };

        match self.char_peek() {
            Some(c) if c == '.' && following.is_numeric() => self.scan_number(),
            Some(c) if c.is_numeric() => self.scan_number(),
            Some(c) if is_punctuation_char(c) => self.scan_operator(),
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

        let leading_trivia = self.scan_trivias(false);
        let start = self.position;
        let col = start - self.last_line;
        let token_kind = self.scan_token();
        let end = self.position;

        // Maybe the EOF is reached so the position > len
        let trailing_trivias = if self.position < self.source.len() {
            self.scan_trivias(true)
        } else {
            vec![]
        };

        let mut t = Token {
            kind: token_kind,
            position: start,
            len: end - start,
            line: self.lines,
            col,
            leading_trivia: leading_trivia.into_boxed_slice(),
            trailing_trivia: trailing_trivias.into_boxed_slice(),
        };

        if let TokenKind::EOF = t.kind {
            t.len = 0;
        }

        Some(t)
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
