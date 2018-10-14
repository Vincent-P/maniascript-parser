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
