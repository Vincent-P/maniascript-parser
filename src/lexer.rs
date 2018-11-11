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

    fn scan_word(&mut self) -> &str {
        let start = self.last_position;
        while let Some(c) = self.char_peek() {
            if !is_ident_char(c) {
                break;
            }
            self.char_next();
        }
        let word = &self.source[start..self.position];
        word
    }

    fn scan_integer(&mut self) {
        while let Some(c) = self.char_peek() {
            if !c.is_numeric() {
                break;
            }
            self.char_next();
        }
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
        let first_char = self.char_next().unwrap();
        match (
            first_char,
            self.char_peek().unwrap_or('\x00'),
            self.char_peek_n(2).unwrap_or('\x00'),
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
                self.char_next();
                self.char_next();
                TokenKind::Alias
            }
            ('<', '=', _) => {
                self.char_next();
                TokenKind::InfEq
            }
            ('<', _, _) => TokenKind::Inf,
            ('>', '=', _) => {
                self.char_next();
                TokenKind::SupEq
            }
            ('>', _, _) => TokenKind::Sup,
            ('+', '+', '+') => {
                self.char_next();
                self.char_next();
                TokenKind::LabelPlus
            }
            ('+', '=', _) => {
                self.char_next();
                TokenKind::PlusEq
            }
            ('+', _, _) => TokenKind::Plus,
            ('-', '-', '-') => {
                self.char_next();
                self.char_next();
                TokenKind::LabelMinus
            }
            ('-', '=', _) => {
                self.char_next();
                TokenKind::MinusEq
            }
            ('-', _, _) => TokenKind::Minus,
            ('*', '*', '*') => {
                self.char_next();
                self.char_next();
                TokenKind::LabelStar
            }
            ('*', '=', _) => {
                self.char_next();
                TokenKind::MultEq
            }
            ('*', _, _) => TokenKind::Mult,
            ('/', '=', _) => {
                self.char_next();
                TokenKind::DivEq
            }
            ('/', _, _) => TokenKind::Div,


            ('!', '=', _) => {
                self.char_next();
                TokenKind::NotEqual
            }
            ('!', _, _) => TokenKind::Not,
            ('%', _, _) => TokenKind::Modulo,
            ('=', '=', _) => {
                self.char_next();
                TokenKind::EqualEqual
            }
            ('=', '>', _) => {
                self.char_next();
                TokenKind::Arrow
            }
            ('=', _, _) => TokenKind::Equal,
            ('&', '=', _) => {
                self.char_next();
                TokenKind::AndEq
            }
            ('&', '&', _) => {
                self.char_next();
                TokenKind::And
            }
            ('|', '=', _) => {
                self.char_next();
                TokenKind::OrEq
            }
            ('|', '|', _) => {
                self.char_next();
                TokenKind::Or
            }
            ('^', '=', _) => {
                self.char_next();
                TokenKind::StrEq
            }
            ('^', _, _) => TokenKind::StrConcat,
            (',', _, _) => TokenKind::Comma,
            (';', _, _) => TokenKind::Semicolon,
            (':', ':', _) => {
                self.char_next();
                TokenKind::ColonColon
            }

            /*
             * STRINGS
             */
            ('"', '"', '"') => {
                self.char_next();
                self.char_next();

                while let (Some(c1), Some(c2), Some(c3)) =
                    (self.char_peek(), self.char_peek_n(2), self.char_peek_n(3))
                {
                    self.char_next();
                    self.char_next();
                    self.char_next();
                    if c1 == '"' && c2 == '"' && c3 == '"' {
                        break;
                    }
                }

                TokenKind::BlockString
            }
            ('"', '"', _) => {
                self.char_next();
                TokenKind::LineString
            }
            ('"', _, _) => {
                while let (Some(c1), Some(c2)) = (self.char_peek(), self.char_peek_n(2)) {
                    if c1 != '\\' && c2 == '"' || c2 == '\n' {
                        break;
                    }
                    self.char_next();
                }

                TokenKind::LineString
            }
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
                if let Some('.') = self.char_peek() {
                    self.char_next();

                    if let Some(c) = self.char_peek() {
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
                    self.char_next();
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
                TokenKind::EOF
            }
            _ => TokenKind::Unknown,
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
