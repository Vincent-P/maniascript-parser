use token_kind::TokenKind;
use trivia_kind::TriviaKind;

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

    fn next(&mut self) -> Option<char> {
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

    fn peek(&mut self) -> Option<char> {
        self.source[self.position..].chars().next()
    }

    fn peek_n(&mut self, n: usize) -> Option<char> {
        let mut chars = self.source[self.position..].chars();
        for _ in 0..n - 1 {
            chars.next();
        }
        chars.next()
    }

    fn scan_word(&mut self) -> &str {
        let start = self.last_position;
        while let Some(c) = self.peek() {
            if !is_ident_char(c) {
                break;
            }
            self.next();
        }
        let word = &self.source[start..self.position];
        word
    }

    fn scan_integer(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_numeric() {
                break;
            }
            self.next();
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut current_char = match self.next() {
            Some(c) => c,
            None => return None,
        };

        let mut start = self.last_position;
        let mut leading_trivia: Vec<TriviaKind> = vec![];

        loop {
            let trivia = match (current_char, self.peek().unwrap_or('\x00')) {
                ('\r', '\n') => {
                    self.next();

                    let mut count = 1;
                    while let (Some('\r'), Some('\n')) = (self.peek(), self.peek_n(2)) {
                        self.next();
                        self.next();
                        count += 1;
                    }

                    TriviaKind::CarriageReturnNewline(count)
                }
                ('\r', _) => {
                    let mut count = 1;
                    while let Some('\r') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::CarriageReturn(count)
                }
                ('\n', _) => {
                    let mut count = 1;
                    while let Some('\n') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::Newline(count)
                }
                ('\x09', _) => {
                    let mut count = 1;
                    while let Some('\x09') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::Tab(count)
                }
                ('\x0b', _) => {
                    let mut count = 1;
                    while let Some('\x0b') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::VerticalTab(count)
                }
                ('\x0c', _) => {
                    let mut count = 1;
                    while let Some('\x0c') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::FormFeed(count)
                }
                (' ', _) => {
                    let mut count = 1;
                    while let Some(' ') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::Space(count)
                }
                ('/', '/') => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.next();
                    }
                    let comment = &self.source[start..self.position];
                    TriviaKind::LineComment(String::from(comment))
                }
                ('/', '*') => {
                    self.next();

                    while let (Some(c1), Some(c2)) = (self.peek(), self.peek_n(2)) {
                        self.next();
                        self.next();
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

            leading_trivia.push(trivia);

            current_char = match self.next() {
                Some(c) => c,
                None => return None,
            }
        }

        start = self.last_position;

        let token_kind = match (
            current_char,
            self.peek().unwrap_or('\x00'),
            self.peek_n(2).unwrap_or('\x00'),
        ) {
            /*
             * OPERATORS
             */
            ('(', _, _) => TokenKind::OpenParen,
            (')', _, _) => TokenKind::CloseParen,
            ('[', _, _) => TokenKind::OpenSquare,
            (']', _, _) => TokenKind::CloseSquare,
            ('{', _, _) => TokenKind::OpenBrace,
            ('}', _, _) => TokenKind::CloseBrace,
            ('<', '=', '>') => {
                self.next();
                self.next();
                TokenKind::Alias
            }
            ('<', '=', _) => {
                self.next();
                TokenKind::InfEq
            }
            ('<', _, _) => TokenKind::Inf,
            ('>', '=', _) => {
                self.next();
                TokenKind::SupEq
            }
            ('>', _, _) => TokenKind::Sup,
            ('+', '+', '+') => {
                self.next();
                self.next();
                TokenKind::LabelPlus
            }
            ('+', '=', _) => {
                self.next();
                TokenKind::PlusEq
            }
            ('+', _, _) => TokenKind::Plus,
            ('-', '-', '-') => {
                self.next();
                self.next();
                TokenKind::LabelMinus
            }
            ('-', '=', _) => {
                self.next();
                TokenKind::MinusEq
            }
            ('-', _, _) => TokenKind::Minus,
            ('*', '*', '*') => {
                self.next();
                self.next();
                TokenKind::LabelStar
            }
            ('*', '=', _) => {
                self.next();
                TokenKind::MultEq
            }
            ('*', _, _) => TokenKind::Mult,
            ('!', '=', _) => {
                self.next();
                TokenKind::NotEqual
            }
            ('!', _, _) => TokenKind::Not,
            ('%', _, _) => TokenKind::Modulo,
            ('=', '=', _) => {
                self.next();
                TokenKind::EqualEqual
            }
            ('=', '>', _) => {
                self.next();
                TokenKind::Arrow
            }
            ('=', _, _) => TokenKind::Equal,
            ('&', '=', _) => {
                self.next();
                TokenKind::AndEq
            }
            ('&', '&', _) => {
                self.next();
                TokenKind::And
            }
            ('|', '=', _) => {
                self.next();
                TokenKind::OrEq
            }
            ('|', '|', _) => {
                self.next();
                TokenKind::Or
            }
            ('^', '=', _) => {
                self.next();
                TokenKind::StrEq
            }
            ('^', _, _) => TokenKind::StrConcat,
            (',', _, _) => TokenKind::Comma,
            (';', _, _) => TokenKind::Semicolon,
            (':', ':', _) => {
                self.next();
                TokenKind::ColonColon
            }
            ('"', '"', '"') => {
                self.next();
                self.next();

                while let (Some(c1), Some(c2), Some(c3)) =
                    (self.peek(), self.peek_n(2), self.peek_n(3))
                {
                    self.next();
                    self.next();
                    self.next();
                    if c1 == '"' && c2 == '"' && c3 == '"' {
                        break;
                    }
                }

                TokenKind::BlockString
            }
            ('"', '"', _) => {
                self.next();
                TokenKind::LineString
            }
            ('"', _, _) => {
                while let (Some(c1), Some(c2)) = (self.peek(), self.peek_n(2)) {
                    if c1 != '\\' && c2 == '"' || c2 == '\n' {
                        break;
                    }
                    self.next();
                }

                TokenKind::LineString
            }
            ('/', '=', _) => {
                self.next();
                TokenKind::DivEq
            }
            ('/', _, _) => TokenKind::Div,

            /*
             * KEYWORDS, IDENTIFIERS AND NUMBERS
             */
            ('#', c, _) if is_ident_char(c) => {
                let kind = match self.scan_word() {
                    "#Include" => TokenKind::Include,
                    "#Const" => TokenKind::Const,
                    "#Setting" => TokenKind::Setting,
                    "#RequireContext" => TokenKind::RequireContext,
                    "#Extends" => TokenKind::Extends,
                    _ => TokenKind::Unknown,
                };

                kind
            }
            (c, _, _) if c.is_numeric() => {
                self.scan_integer();
                if let Some('.') = self.peek() {
                    self.next();

                    if let Some(c) = self.peek() {
                        if c.is_numeric() {
                            self.scan_integer();
                        }
                    }

                    TokenKind::Real
                } else {
                    TokenKind::Integer
                }
            }
            ('.', c, _) => {
                if c.is_numeric() {
                    self.next();
                    self.scan_integer();
                    TokenKind::Real
                } else {
                    TokenKind::Dot
                }
            }
            (c, _, _) if is_ident_char(c) => {
                let kind = match self.scan_word() {
                    "break" => TokenKind::Break,
                    "yield" => TokenKind::Yield,
                    "continue" => TokenKind::Continue,
                    "return" => TokenKind::Return,
                    "declare" => TokenKind::Declare,
                    "as" => TokenKind::As,
                    "for" => TokenKind::For,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "switch" => TokenKind::Switch,
                    "switchtype" => TokenKind::SwitchType,
                    "case" => TokenKind::Case,
                    "default" => TokenKind::Default,
                    "while" => TokenKind::While,
                    "foreach" => TokenKind::Foreach,
                    "in" => TokenKind::In,
                    "is" => TokenKind::Is,
                    "netwrite" => TokenKind::Netwrite,
                    "netread" => TokenKind::Netread,
                    "persistent" => TokenKind::Persistent,
                    "metadata" => TokenKind::Metadata,
                    "Null" => TokenKind::Null,
                    "NullId" => TokenKind::NullId,
                    "True" => TokenKind::True,
                    "False" => TokenKind::False,
                    _ => TokenKind::Identifier,
                };

                kind
            }
            ('\x00', _, _) => {
                return Some(Token {
                            kind: TokenKind::EOF,
                    position: self.position,
                    len: 0,
                    leading_trivia: leading_trivia.into_boxed_slice(),
                    trailing_trivia: vec![].into_boxed_slice(),
                });
            }
            _ => TokenKind::Unknown,
        };

        let mut token: Token = Token {
            kind: token_kind,
            position: start,
            len: self.position - start,
            leading_trivia: leading_trivia.into_boxed_slice(),
            trailing_trivia: vec![].into_boxed_slice(),
        };

        start = self.position;
        let mut trivias: Vec<TriviaKind> = vec![];
        loop {
            let trivia = match (
                self.peek().unwrap_or('\x00'),
                self.peek_n(2).unwrap_or('\x00'),
            ) {
                ('\x09', _) => {
                    self.next();
                    let mut count = 1;
                    while let Some('\x09') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::Tab(count)
                }
                (' ', _) => {
                    self.next();
                    let mut count = 1;
                    while let Some(' ') = self.peek() {
                        self.next();
                        count += 1;
                    }

                    TriviaKind::Space(count)
                }
                ('/', '/') => {
                    self.next();
                    self.next();
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.next();
                    }
                    let comment = &self.source[start..self.position];
                    TriviaKind::LineComment(String::from(comment))
                }
                ('/', '*') => {
                    self.next();
                    self.next();

                    while let (Some(c1), Some(c2)) = (self.peek(), self.peek_n(2)) {
                        self.next();
                        self.next();
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

        token.trailing_trivia = trivias.into_boxed_slice();

        Some(token)
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
