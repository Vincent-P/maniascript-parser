use crate::parser::SyntaxKind::{self, *};
use rowan::SmolStr;

#[derive(Clone, Copy)]
struct Interpol {
    multiline: bool,
}

#[derive(Clone, Copy)]
enum Todo {
    StringBody { multiline: bool },
    StringEnd,
    InterpolStart,
}

#[derive(Clone, Copy, Default)]
struct Context {
    interpol: Option<Interpol>,
    todo: Option<Todo>,
}

#[derive(Clone, Copy)]
struct State<'a> {
    input: &'a str,
    offset: usize,
}

impl PartialEq for State<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.input, other.input) && self.offset == other.offset
    }
}

impl Eq for State<'_> {}

pub struct Tokenizer<'a> {
    ctx: Vec<Context>,
    state: State<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            ctx: vec![Context::default()],
            state: State { input, offset: 0 },
        }
    }

    fn remaining(&self) -> &str {
        &self.state.input[self.state.offset..]
    }

    fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek2(&self) -> Option<char> {
        let mut chars = self.remaining().chars();
        chars.next();
        chars.next()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            self.state.offset += c.len_utf8();
        }
        c
    }

    fn starts_with_bump(&mut self, s: &str) -> bool {
        let starts_with = self.remaining().starts_with(s);
        if starts_with {
            self.state.offset += s.len();
        }
        starts_with
    }

    fn string_since(&self, previous: State) -> SmolStr {
        SmolStr::new(&previous.input[previous.offset..self.state.offset])
    }

    fn consume<P>(&mut self, mut predicate: P) -> usize
    where
        P: FnMut(char) -> bool,
    {
        let mut len = 0;
        while self.peek().map(|c| predicate(c)).unwrap_or(false) {
            self.next().unwrap();
            len += 1;
        }
        len
    }

    fn next_string(&mut self, multiline: bool) -> SyntaxKind {
        loop {
            let start = self.state;
            match self.next() {
                None => return TOKEN_ERROR,
                Some('"') if !multiline => {
                    self.state = start;
                    return TOKEN_STRING_CONTENT;
                }

                Some('\\') if !multiline => match self.next() {
                    None => return TOKEN_ERROR,
                    Some(_) => (),
                },

                Some('"') if multiline => {
                    let peeked = self.peek();
                    let peeked2 = self.peek2();
                    if let (Some('"'), Some('"')) = (peeked, peeked2) {
                        self.next().unwrap();
                        self.next().unwrap();
                        self.state = start;
                        return TOKEN_STRING_CONTENT;
                    }
                }

                Some('{') => {
                    let peeked = self.peek();
                    let peeked2 = self.peek2();
                    if let (Some('{'), Some('{')) = (peeked, peeked2) {
                        self.next().unwrap();
                        self.next().unwrap();

                        self.state = start;
                        self.ctx.push(Context {
                            interpol: Some(Interpol { multiline }),
                            todo: Some(Todo::InterpolStart),
                            ..Default::default()
                        });
                        return TOKEN_STRING_CONTENT;
                    }
                }
                Some(_) => (),
            }
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = (SyntaxKind, SmolStr);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.state;

        // Handle already started multi-token
        loop {
            let todo = &mut self.ctx.last_mut().unwrap().todo;
            match todo.take() {
                Some(Todo::InterpolStart) => {
                    if self.starts_with_bump("{{{") {
                        return Some((TOKEN_INTERPOL_START, self.string_since(start)));
                    }
                }
                Some(Todo::StringBody { multiline }) => {
                    *todo = Some(Todo::StringEnd);
                    let token = self.next_string(multiline);
                    if self.state == start {
                        continue;
                    }
                    return Some((token, self.string_since(start)));
                }
                Some(Todo::StringEnd) => {
                    let status = match self.peek() {
                        Some('"') => {
                            self.next().unwrap();
                            let peeked = self.peek();
                            let peeked2 = self.peek2();

                            match (peeked, peeked2) {
                                (Some('"'), Some('"')) => {
                                    self.next().unwrap();
                                    self.next().unwrap();
                                    true
                                }
                                (_, Some('"')) => false, // TODO: is it necessary ???
                                _ => true,
                            }
                        }
                        _ => false,
                    };
                    if !status {
                        return Some((TOKEN_ERROR, self.string_since(start)));
                    }

                    return Some((TOKEN_STRING_END, self.string_since(start)));
                }
                _ => (),
            }
            break;
        }

        if self.consume(char::is_whitespace) > 0 {
            return Some((TOKEN_WHITESPACE, self.string_since(start)));
        }

        let c = self.next()?;
        let peeked = self.peek();
        let peeked2 = self.peek2();

        // 3 chars tokens
        match (c, peeked, peeked2) {
            ('*', Some('*'), Some('*')) => {
                self.next().unwrap();
                self.next().unwrap();
                return Some((TOKEN_LABEL_STAR, self.string_since(start)));
            }

            ('+', Some('+'), Some('+')) => {
                self.next().unwrap();
                self.next().unwrap();
                return Some((TOKEN_LABEL_PLUS, self.string_since(start)));
            }

            ('"', Some('"'), Some('"')) => {
                self.next().unwrap();
                self.next().unwrap();
                self.ctx.last_mut().unwrap().todo = Some(Todo::StringBody { multiline: true });
                return Some((TOKEN_STRING_START, self.string_since(start)));
            }

            _ => (),
        }

        // directives
        if let Some(peeked_char) = peeked {
            match (c, peeked_char) {
                ('#', 'A'..='Z') => {
                    self.consume(|c| match c {
                        'a'..='z' | 'A'..='Z' => true,
                        _ => false,
                    });
                    let directive = self.string_since(start);

                    let syntax_kind = match &*directive {
                        "#Include" => TOKEN_INCLUDE,
                        "#Const" => TOKEN_CONST,
                        "#Setting" => TOKEN_SETTING,
                        "#RequireContext" => TOKEN_REQUIRE_CONTEXT,
                        "#Extends" => TOKEN_EXTENDS,
                        "#Struct" => TOKEN_STRUCT,
                        _ => TOKEN_ERROR,
                    };

                    return Some((syntax_kind, directive));
                }
                _ => (),
            }
        }

        match c {
            // multiline comment
            '/' if peeked == Some('*') => {
                self.next().unwrap();
                loop {
                    self.consume(|c| c != '*');
                    self.next().unwrap();
                    match self.peek() {
                        None => return Some((TOKEN_ERROR, self.string_since(start))),
                        Some('/') => {
                            self.next().unwrap();
                            return Some((TOKEN_COMMENT, self.string_since(start)));
                        }
                        _ => (),
                    }
                }
            }

            // single-line comment
            '/' if peeked == Some('/') => {
                self.next().unwrap();
                self.consume(|c| c != '\n');
                Some((TOKEN_COMMENT, self.string_since(start)))
            }

            // numbers
            '0'..='9' => {
                self.consume(|c| '0' <= c && c <= '9');
                if self.peek() == Some('.') {
                    self.next().unwrap();
                    self.consume(|c| '0' <= c && c <= '9');
                    Some((TOKEN_REAL, self.string_since(start)))
                } else {
                    Some((TOKEN_INTEGER, self.string_since(start)))
                }
            }

            '.' => {
                match peeked {
                    Some(p) if '0' <= p && p <= '9' => {
                        self.consume(|c| '0' <= c && c <= '9');
                        Some((TOKEN_REAL, self.string_since(start)))
                    }
                    _ => Some((TOKEN_DOT, self.string_since(start)))
                }
            }

            // strings
            '"' => {
                self.ctx.last_mut().unwrap().todo = Some(Todo::StringBody { multiline: false });
                Some((TOKEN_STRING_START, self.string_since(start)))
            }

            // idents
            'a'..='z' | 'A'..='Z' | '_' => {
                self.consume(|c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
                    _ => false,
                });
                let ident = self.string_since(start);
                let syntax_kind = match &*ident {
                    "break" => TOKEN_BREAK,
                    "yield" => TOKEN_YIELD,
                    "continue" => TOKEN_CONTINUE,
                    "return" => TOKEN_RETURN,
                    "declare" => TOKEN_DECLARE,
                    "as" => TOKEN_AS,
                    "for" => TOKEN_FOR,
                    "if" => TOKEN_IF,
                    "else" => TOKEN_ELSE,
                    "switch" => TOKEN_SWITCH,
                    "switchtype" => TOKEN_SWITCHTYPE,
                    "case" => TOKEN_CASE,
                    "default" => TOKEN_DEFAULT,
                    "while" => TOKEN_WHILE,
                    "foreach" => TOKEN_FOREACH,
                    "in" => TOKEN_IN,
                    "is" => TOKEN_IS,
                    "netwrite" => TOKEN_NETWRITE,
                    "netread" => TOKEN_NETREAD,
                    "persistent" => TOKEN_PERSISTENT,
                    "metadata" => TOKEN_METADATA,
                    _ => TOKEN_IDENT,
                };
                Some((syntax_kind, ident))
            }

            // multi-character tokens
            ':' if peeked == Some(':') => {
                self.next().unwrap();
                Some((TOKEN_COLON_COLON, self.string_since(start)))
            }

            '=' if peeked == Some('>') => {
                self.next().unwrap();
                Some((TOKEN_ARROW, self.string_since(start)))
            }

            '!' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_NOT_EQ, self.string_since(start)))
            }

            '-' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_MINUS_EQ, self.string_since(start)))
            }

            '+' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_PLUS_EQ, self.string_since(start)))
            }

            '*' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_MULT_EQ, self.string_since(start)))
            }

            '/' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_DIV_EQ, self.string_since(start)))
            }

            '%' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_MOD_EQ, self.string_since(start)))
            }

            '^' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_CONCAT_EQ, self.string_since(start)))
            }

            '&' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_AND_EQ, self.string_since(start)))
            }

            '|' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_OR_EQ, self.string_since(start)))
            }

            '<' if peeked == Some('=') => {
                self.next().unwrap();

                if self.peek() == Some('>') {
                    self.next().unwrap();
                    Some((TOKEN_ALIAS, self.string_since(start)))
                } else {
                    Some((TOKEN_LESS_OR_EQ, self.string_since(start)))
                }
            }

            '>' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_MORE_OR_EQ, self.string_since(start)))
            }

            '=' if peeked == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_EQ_EQ, self.string_since(start)))
            }

            '&' if peeked == Some('&') => {
                self.next().unwrap();
                Some((TOKEN_AND, self.string_since(start)))
            }

            '|' if peeked == Some('|') => {
                self.next().unwrap();
                Some((TOKEN_OR, self.string_since(start)))
            }

            // single character tokens
            ',' => Some((TOKEN_COMMA, self.string_since(start))),
            ';' => Some((TOKEN_SEMICOLON, self.string_since(start))),
            ':' => Some((TOKEN_COLON, self.string_since(start))),

            '(' => Some((TOKEN_OPEN_PAREN, self.string_since(start))),
            ')' => Some((TOKEN_CLOSE_PAREN, self.string_since(start))),
            '[' => Some((TOKEN_OPEN_SQUARE, self.string_since(start))),
            ']' => Some((TOKEN_CLOSE_SQUARE, self.string_since(start))),
            '{' => Some((TOKEN_OPEN_BRACE, self.string_since(start))),
            '}' => {
                if let (Some('}'), Some('}')) = (peeked, peeked2) {
                    if let Some(Interpol { multiline }) = self.ctx.last_mut().unwrap().interpol {
                        self.next().unwrap();
                        self.next().unwrap();
                        self.ctx.pop().unwrap();

                        self.ctx.last_mut().unwrap().todo = Some(Todo::StringBody { multiline });
                        return Some((TOKEN_INTERPOL_END, self.string_since(start)));
                    }
                }
                Some((TOKEN_CLOSE_BRACE, self.string_since(start)))
            }

            '!' => Some((TOKEN_NOT, self.string_since(start))),
            '-' => Some((TOKEN_MINUS, self.string_since(start))),
            '+' => Some((TOKEN_PLUS, self.string_since(start))),
            '*' => Some((TOKEN_MULT, self.string_since(start))),
            '/' => Some((TOKEN_DIV, self.string_since(start))),
            '%' => Some((TOKEN_MOD, self.string_since(start))),
            '^' => Some((TOKEN_CONCAT, self.string_since(start))),
            '<' => Some((TOKEN_LESS, self.string_since(start))),
            '>' => Some((TOKEN_MORE, self.string_since(start))),
            '=' => Some((TOKEN_EQUAL, self.string_since(start))),

            _ => Some((TOKEN_ERROR, self.string_since(start))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{SyntaxKind::{self, *}, Tokenizer};
    use rowan::SmolStr;


    fn tokenize(input: &str) -> Vec<(SyntaxKind, SmolStr)> {
        Tokenizer::new(input).collect()
    }


    macro_rules! tokens {
        ($(($token:expr, $str:expr)),*) => {
            vec![$(($token, $str.into())),*]
        }
    }

    fn single_token_test(token: &str, kind: SyntaxKind) {
        assert_eq!(tokenize(token), tokens![(kind, token)]);
    }


    #[test]
    fn single_line_comment() {
        assert_eq!(tokenize(" // salut ca va?"), tokens![
            (TOKEN_WHITESPACE, " "),
            (TOKEN_COMMENT, "// salut ca va?")
        ]);
    }

    #[test]
    fn multi_line_comment() {
        assert_eq!(tokenize("/**/"), tokens![(TOKEN_COMMENT, "/**/")]);
        assert_eq!(tokenize("/***/"), tokens![(TOKEN_COMMENT, "/***/")]);

        assert_eq!(
            tokenize(
                r#"
/* salut
* ca va? */
 "#
            ),
            tokens![
                (TOKEN_WHITESPACE, "\n"),
                (
                    TOKEN_COMMENT,
                    r#"/* salut
* ca va? */"#
                ),
                (TOKEN_WHITESPACE, "\n ")
            ]
        );
    }

    #[test]
    fn single_line_string() {
        assert_eq!(tokenize(r#" " \"hello there!\" " "#), tokens![
            (TOKEN_WHITESPACE, " "),
            (TOKEN_STRING_START, "\""),
            (TOKEN_STRING_CONTENT, r#" \"hello there!\" "#),
            (TOKEN_STRING_END, "\""),
            (TOKEN_WHITESPACE, " ")
        ]);
    }

    #[test]
    fn multi_line_string() {
        assert_eq!(
            tokenize(
                r#""""
this is a
"multi line"
string!""""#
            ),
            tokens![
                (TOKEN_STRING_START, "\"\"\""),
                (
                    TOKEN_STRING_CONTENT,
                    r#"
this is a
"multi line"
string!"#
                ),
                (TOKEN_STRING_END, "\"\"\"")
            ]
        );
    }

    #[test]
    fn multi_line_interpol() {
        assert_eq!(tokenize(r#"""" {{{MyVariable}}} """"#), tokens![
            (TOKEN_STRING_START, "\"\"\""),
            (TOKEN_STRING_CONTENT, " "),
            (TOKEN_INTERPOL_START, "{{{"),
            (TOKEN_IDENT, "MyVariable"),
            (TOKEN_INTERPOL_END, "}}}"),
            (TOKEN_STRING_CONTENT, " "),
            (TOKEN_STRING_END, "\"\"\"")
        ]);
    }

    #[test]
    fn directive_include() {
        assert_eq!(tokenize(r#"#Include "TextLib" as TL"#), tokens![
            (TOKEN_INCLUDE, "#Include"),
            (TOKEN_WHITESPACE, " "),
            (TOKEN_STRING_START, "\""),
            (TOKEN_STRING_CONTENT, "TextLib"),
            (TOKEN_STRING_END, "\""),
            (TOKEN_WHITESPACE, " "),
            (TOKEN_AS, "as"),
            (TOKEN_WHITESPACE, " "),
            (TOKEN_IDENT, "TL")
        ]);
    }

    #[test]
    fn directive_const() {
        assert_eq!(
            tokenize(r#"#Const C_SettingsUpdateInterval 1000"#),
            tokens![
                (TOKEN_CONST, "#Const"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "C_SettingsUpdateInterval"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1000")
            ]
        );
    }

    #[test]
    fn directive_setting() {
        assert_eq!(
            tokenize(r#"#Setting S_TimeLimit -1 as _("Time limit")"#),
            tokens![
                (TOKEN_SETTING, "#Setting"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "S_TimeLimit"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MINUS, "-"),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AS, "as"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "_"),
                (TOKEN_OPEN_PAREN, "("),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, "Time limit"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_CLOSE_PAREN, ")")
            ]
        );
    }

    #[test]
    fn directive_require_context() {
        assert_eq!(tokenize(r#"#RequireContext CSmMapType"#), tokens![
            (TOKEN_REQUIRE_CONTEXT, "#RequireContext"),
            (TOKEN_WHITESPACE, " "),
            (TOKEN_IDENT, "CSmMapType")
        ]);
    }

    #[test]
    fn directive_extends() {
        assert_eq!(
            tokenize(r#"#Extends "Modes/ShootMania/Base/ModeShootMania.Script.txt""#),
            tokens![
                (TOKEN_EXTENDS, "#Extends"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (
                    TOKEN_STRING_CONTENT,
                    "Modes/ShootMania/Base/ModeShootMania.Script.txt"
                ),
                (TOKEN_STRING_END, "\"")
            ]
        );
    }

    #[test]
    fn directive_struct() {
        assert_eq!(
            tokenize(
                r#"
#Struct SCPTotalTime {
    Text Login;
    Text Nickname;
    Integer Time;
}"#
            ),
            tokens![
                (TOKEN_WHITESPACE, "\n"),
                (TOKEN_STRUCT, "#Struct"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "SCPTotalTime"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_OPEN_BRACE, "{"),
                (TOKEN_WHITESPACE, "\n    "),
                (TOKEN_IDENT, "Text"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "Login"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, "\n    "),
                (TOKEN_IDENT, "Text"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "Nickname"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, "\n    "),
                (TOKEN_IDENT, "Integer"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "Time"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, "\n"),
                (TOKEN_CLOSE_BRACE, "}")
            ]
        );
    }

    #[test]
    fn keywords() {
        assert_eq!(
            tokenize("break yield continue return declare as for if else switch switchtype case default while foreach in is netwrite netread persistent metadata"),
            tokens![
                (TOKEN_BREAK, "break"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_YIELD, "yield"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CONTINUE, "continue"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_RETURN, "return"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_DECLARE, "declare"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AS, "as"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FOR, "for"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IF, "if"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ELSE, "else"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SWITCH, "switch"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SWITCHTYPE, "switchtype"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CASE, "case"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_DEFAULT, "default"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_WHILE, "while"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FOREACH, "foreach"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IN, "in"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IS, "is"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_NETWRITE, "netwrite"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_NETREAD, "netread"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_PERSISTENT, "persistent"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_METADATA, "metadata")
            ]
        );
    }

    #[test]
    fn symbols() {
        single_token_test("***", TOKEN_LABEL_STAR);
        single_token_test("+++", TOKEN_LABEL_PLUS);
        single_token_test(".", TOKEN_DOT);
        single_token_test(",", TOKEN_COMMA);
        single_token_test(";", TOKEN_SEMICOLON);
        single_token_test(":", TOKEN_COLON);
        single_token_test("::", TOKEN_COLON_COLON);
        single_token_test("(", TOKEN_OPEN_PAREN);
        single_token_test(")", TOKEN_CLOSE_PAREN);
        single_token_test("[", TOKEN_OPEN_SQUARE);
        single_token_test("]", TOKEN_CLOSE_SQUARE);
        single_token_test("{", TOKEN_OPEN_BRACE);
        single_token_test("}", TOKEN_CLOSE_BRACE);
        single_token_test("=>", TOKEN_ARROW);
    }

    #[test]
    fn operators() {
        single_token_test("!", TOKEN_NOT);
        single_token_test("-", TOKEN_MINUS);
        single_token_test("+", TOKEN_PLUS);
        single_token_test("*", TOKEN_MULT);
        single_token_test("/", TOKEN_DIV);
        single_token_test("%", TOKEN_MOD);
        single_token_test("^", TOKEN_CONCAT);
        single_token_test("==", TOKEN_EQ_EQ);
        single_token_test("!=", TOKEN_NOT_EQ);
        single_token_test("<", TOKEN_LESS);
        single_token_test("<=", TOKEN_LESS_OR_EQ);
        single_token_test(">", TOKEN_MORE);
        single_token_test(">=", TOKEN_MORE_OR_EQ);
        single_token_test("&&", TOKEN_AND);
        single_token_test("||", TOKEN_OR);
    }

    #[test]
    fn assignment() {
        single_token_test("=", TOKEN_EQUAL);
        single_token_test("<=>", TOKEN_ALIAS);
        single_token_test("+=", TOKEN_PLUS_EQ);
        single_token_test("-=", TOKEN_MINUS_EQ);
        single_token_test("*=", TOKEN_MULT_EQ);
        single_token_test("/=", TOKEN_DIV_EQ);
        single_token_test("^=", TOKEN_CONCAT_EQ);
        single_token_test("&=", TOKEN_AND_EQ);
        single_token_test("|=", TOKEN_OR_EQ);
        single_token_test("%=", TOKEN_MOD_EQ);
    }

    #[test]
    fn identifiers() {
        single_token_test("asif", TOKEN_IDENT);
        single_token_test("MyIdent", TOKEN_IDENT);
        single_token_test("_MyIdent", TOKEN_IDENT);
        single_token_test("_MyIdent2", TOKEN_IDENT);
        single_token_test("_My_Ident2", TOKEN_IDENT);
    }

    #[test]
    fn numbers() {
        single_token_test("07", TOKEN_INTEGER);
        single_token_test("12", TOKEN_INTEGER);
        single_token_test("1200000", TOKEN_INTEGER);
        single_token_test("1", TOKEN_INTEGER);

        single_token_test("0.7", TOKEN_REAL);
        single_token_test("0.", TOKEN_REAL);
        single_token_test("0.0", TOKEN_REAL);
        single_token_test(".01", TOKEN_REAL);
    }
    // TODO: more tests...
}
