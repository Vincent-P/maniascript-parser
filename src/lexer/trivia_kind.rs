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
        match self {
            TriviaKind::Newline(_)
            | TriviaKind::FormFeed(_)
            | TriviaKind::CarriageReturn(_)
            | TriviaKind::CarriageReturnNewline(_)
            | TriviaKind::VerticalTab(_) => true,
            _ => false,
        }
    }
}
