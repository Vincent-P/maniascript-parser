#[derive(Debug, PartialEq, Clone)]
pub enum TriviaKind {
    Space(usize),
    Tab(usize),
    Newline(usize),
    VerticalTab(usize),
    FormFeed(usize),
    CarriageReturn(usize),
    CarriageReturnNewline(usize),
    LineComment(String),
    BlockComment(String),
}

impl TriviaKind {
    pub fn is_line_break(&self) -> bool {
        self.line_count() > 0
    }

    pub fn line_count(&self) -> usize {
        match self {
            TriviaKind::Newline(s)
            | TriviaKind::FormFeed(s)
            | TriviaKind::CarriageReturn(s)
            | TriviaKind::CarriageReturnNewline(s)
            | TriviaKind::VerticalTab(s) => *s,
            _ => 0,
        }
    }

    pub fn to_print(&self) -> String {
        match self {
            TriviaKind::Space(c) => String::from(" ").repeat(*c),
            TriviaKind::Tab(c) => String::from("\x09").repeat(*c),
            TriviaKind::Newline(c) => String::from("\n").repeat(*c),
            TriviaKind::VerticalTab(c) => String::from("\0xb").repeat(*c),
            TriviaKind::FormFeed(c) => String::from("\x0c").repeat(*c),
            TriviaKind::CarriageReturn(c) => String::from("\r").repeat(*c),
            TriviaKind::CarriageReturnNewline(c) => String::from("\r\n").repeat(*c),
            TriviaKind::LineComment(s) => s.clone(),
            TriviaKind::BlockComment(s) => s.clone(),
        }
    }
}
