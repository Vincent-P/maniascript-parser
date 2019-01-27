use crate::lexer::token::Token;

#[derive(Debug)]
pub enum ExpressionKind {
    Type,

    Identifier,
    Literal,
    Vector,
    Array,

    Not,
    Negative,

    BinaryOp,
    And,
    Or,
    IsEqual,
    IsNotEqual,

    Inf,
    Sup,
    InfEq,
    SupEq,

    StrConcat,

    Plus,
    Minus,
    Mult,
    Div,
    Modulo,

    Member,
    Namespace,

    ArrayAccess,
    FunctionCall,
    Cast,
    Is,
    MapsTo,
}

#[derive(Debug)]
pub enum VarDecKind {
    Typed,
    Valued
}

#[derive(Debug)]
pub enum NodeKind {
    Include,
    Const,
    Setting,
    RequireContext,
    Extends,

    FuncDec,
    LabelDec,

    VarDec,
    VarDecModifier,

    Assignment,

    LabelCall,

    If,
    Else,
    Switch,
    Case,
    SwitchType,
    CaseType,
    Default,

    For,
    Foreach,
    While,

    Block,
    Statement,
    Expr(ExpressionKind),
    ExprStatement,

    Continue,
    Break,
    Return,
    Yield,

    File,
    Parenthesised,
    Token(Token),
}

impl From<ExpressionKind> for NodeKind {
    fn from(expr_kind: ExpressionKind) -> NodeKind {
        NodeKind::Expr(expr_kind)
    }
}
