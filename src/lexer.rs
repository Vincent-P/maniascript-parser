use token_kind::TokenKind;
use trivia_kind::TriviaKind;
use ::std::str::FromStr;

pub struct Lexer<'a> {
    source: &'a str,
    position: usize,
    last_position: usize,
    end_of_file_reached: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    kind: TokenKind,
    position: usize,
    len: usize,
    leading_trivia: Box<[TriviaKind]>,
    trailing_trivia: Box<[TriviaKind]>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,
            position: 0,
            last_position: 0,
            end_of_file_reached: false,
        }
    }

    fn char_next(&mut self) -> Option<char> {
        match self.source[self.position..].chars().next() {
            Some(c) => {
                self.last_position = self.position;
                self.position += c.len_utf8();
                Some(c)
            }
            None => {
                if self.end_of_file_reached {
                    None
                } else {
                    self.end_of_file_reached = true;
                    Some('\x00')
                }
            }
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
        self.char_next();

        let start = self.last_position;
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
            _ => TokenKind::Identifier
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

    fn scan_trivias(&mut self, is_trailing: bool) -> Vec<TriviaKind> {
        let mut trivias = vec![];
        let start = self.position;


        loop {
            let trivia = match (self.char_peek().unwrap_or('\x00'), self.char_peek().unwrap_or('\x00')) {
                ('\r', '\n') => {
                    if is_trailing {
                        break;
                    }
                    self.char_next();
                    self.char_next();

                    let mut count = 1;
                    while let (Some('\r'), Some('\n')) = (self.char_peek(), self.char_peek_n(2)) {
                        self.char_next();
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::CarriageReturnNewline(count)
                }
                ('\r', _) => {
                    if is_trailing {
                        break;
                    }
                    self.char_next();

                    let mut count = 1;
                    while let Some('\r') = self.char_peek() {
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::CarriageReturn(count)
                }
                ('\n', _) => {
                    if is_trailing {
                        break;
                    }
                    let mut count = 1;
                    while let Some('\n') = self.char_peek() {
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::Newline(count)
                }
                ('\x09', _) => {
                    self.char_next();

                    let mut count = 1;
                    while let Some('\x09') = self.char_peek() {
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::Tab(count)
                }
                ('\x0b', _) => {
                    if is_trailing {
                        break;
                    }
                    self.char_next();

                    let mut count = 1;
                    while let Some('\x0b') = self.char_peek() {
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::VerticalTab(count)
                }
                ('\x0c', _) => {
                    if is_trailing {
                        break;
                    }
                    self.char_next();

                    let mut count = 1;
                    while let Some('\x0c') = self.char_peek() {
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::FormFeed(count)
                }
                (' ', _) => {
                    self.char_next();

                    let mut count = 1;
                    while let Some(' ') = self.char_peek() {
                        self.char_next();
                        count += 1;
                    }

                    TriviaKind::Space(count)
                }
                ('/', '/') => {
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
                ('/', '*') => {
                    self.char_next();
                    self.char_next();

                    while let (Some(c1), Some(c2)) = (self.char_peek(), self.char_peek_n(2)) {
                        self.char_next();
                        self.char_next();
                        if c1 == '*' && c2 == '/' {
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

            trivias.push(trivia);
        }
        trivias
    }

    fn scan_token(&mut self) -> TokenKind {
        let c: char = match self.char_peek() {
            Some(c) => c,
            None => return TokenKind::EOF
        };

        if is_punctuation_char(c) {
            self.scan_operator()
        } else if c.is_numeric() {
            self.scan_number()
        } else if c == '#' || is_ident_char(c) {
            self.scan_word()
        } else if c == '"' {
            self.char_next();

            match (self.char_peek(), self.char_peek_n(2)) {
                // multiline string
                (Some('"'), Some('"')) =>  {
                    self.char_next();
                    self.char_next();

                    while let Some(next) = self.char_peek() {
                        self.char_next();

                        match (next, self.char_peek(), self.char_peek_n(2)) {
                            ('"', Some('"'), Some('"')) => {
                                self.char_next();
                                self.char_next();
                                break;
                            }
                            (_, _, None) => {
                                break;
                            }
                            _ => {
                                self.char_next();
                            }
                        }
                    }
                    TokenKind::BlockString
                }
                // empty string
                (Some('"'), _) =>  {
                    self.char_next();
                    TokenKind::LineString
                }
                // else
                _ => {
                    while let Some(next) = self.char_peek() {
                        self.char_next();

                        match (next, self.char_peek()) {
                            (c, Some('"')) if c != '\\' => {
                                self.char_next();
                                break;
                            }
                            (_, Some('\n')) => {
                                break;
                            }
                            _ => {
                                self.char_next();
                            }
                        }
                        if next == '"' {
                            break;
                        }
                    }
                    TokenKind::LineString
                }
            }


        } else {
            let token = &self.source[self.position..self.position + c.len_utf8()];
            self.position += c.len_utf8();
            TokenKind::from_str_or_unknown(token)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.char_peek() {
            Some(_) => {},
            None => return None
        }

        let start = self.position;
        let leading_trivia = self.scan_trivias(false);
        let token_kind = self.scan_token();
        let trailing_trivias = self.scan_trivias(true);

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
    c == '.' || c == ';' || c == ',' || c == ':' || c == '(' || c == ')' || c == '['
        || c == ']' || c == '{' || c == '}' || c == '!' || c == '&' || c == '|' || c == '<'
        || c == '>' || c == '=' || c == '-' || c == '+' || c == '-' || c == '*' || c == '/'
        || c == '%' || c == '^'
}