use strum_macros::{Display, EnumString};

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString, Display)]
pub enum TokenKind {
    Unknown,
    #[strum(disabled = "true")]
    EOF,
    #[strum(disabled = "true")]
    Identifier,
    #[strum(disabled = "true")]
    Integer,
    #[strum(disabled = "true")]
    Real,
    #[strum(disabled = "true")]
    LineString,
    #[strum(disabled = "true")]
    BlockString,
    #[strum(serialize = "***")]
    LabelStar,
    #[strum(serialize = "---")]
    LabelMinus,
    #[strum(serialize = "+++")]
    LabelPlus,
    #[strum(serialize = ".")]
    Dot,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = ";")]
    Semicolon,
    #[strum(serialize = ":")]
    Colon,
    #[strum(serialize = "(")]
    OpenParen,
    #[strum(serialize = ")")]
    CloseParen,
    #[strum(serialize = "[")]
    OpenSquare,
    #[strum(serialize = "]")]
    CloseSquare,
    #[strum(serialize = "{")]
    OpenBrace,
    #[strum(serialize = "}")]
    CloseBrace,
    #[strum(serialize = "::")]
    ColonColon,

    // Operator
    #[strum(serialize = "!")]
    Not,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "*")]
    Mult,
    #[strum(serialize = "/")]
    Div,
    #[strum(serialize = "%")]
    Modulo,
    #[strum(serialize = "^")]
    StrConcat,

    //
    #[strum(serialize = "==")]
    EqualEqual,
    #[strum(serialize = "!=")]
    NotEqual,
    #[strum(serialize = "<")]
    Inf,
    #[strum(serialize = "<=")]
    InfEq,
    #[strum(serialize = ">")]
    Sup,
    #[strum(serialize = ">=")]
    SupEq,
    #[strum(serialize = "&&")]
    And,
    #[strum(serialize = "||")]
    Or,

    // Assignment
    #[strum(serialize = "=")]
    Equal,
    #[strum(serialize = "<=>")]
    Alias,
    #[strum(serialize = "+=")]
    PlusEq,
    #[strum(serialize = "-=")]
    MinusEq,
    #[strum(serialize = "*=")]
    MultEq,
    #[strum(serialize = "/=")]
    DivEq,
    #[strum(serialize = "^=")]
    StrEq,
    #[strum(serialize = "&=")]
    AndEq,
    #[strum(serialize = "|=")]
    OrEq,

    #[strum(serialize = "=>")]
    Arrow,

    // Keywords
    #[strum(serialize = "#Include")]
    Include,
    #[strum(serialize = "#Const")]
    Const,
    #[strum(serialize = "#Setting")]
    Setting,
    #[strum(serialize = "#RequireContext")]
    RequireContext,
    #[strum(serialize = "#Extends")]
    Extends,
    #[strum(serialize = "#Struct")]
    Struct,

    #[strum(serialize = "break")]
    Break,
    #[strum(serialize = "yield")]
    Yield,
    #[strum(serialize = "continue")]
    Continue,
    #[strum(serialize = "return")]
    Return,
    #[strum(serialize = "declare")]
    Declare,
    #[strum(serialize = "as")]
    As,
    #[strum(serialize = "for")]
    For,
    #[strum(serialize = "if")]
    If,
    #[strum(serialize = "else")]
    Else,
    #[strum(serialize = "switch")]
    Switch,
    #[strum(serialize = "switchtype")]
    SwitchType,
    #[strum(serialize = "case")]
    Case,
    #[strum(serialize = "default")]
    Default,
    #[strum(serialize = "while")]
    While,
    #[strum(serialize = "foreach")]
    Foreach,
    #[strum(serialize = "in")]
    In,
    #[strum(serialize = "is")]
    Is,
    #[strum(serialize = "netwrite")]
    Netwrite,
    #[strum(serialize = "netread")]
    Netread,
    #[strum(serialize = "persistent")]
    Persistent,
    #[strum(serialize = "metadata")]
    Metadata,

    // Constants
    Null,
    NullId,
    True,
    False,
}

impl TokenKind {
    pub fn from_str_or_unknown(word: &str) -> TokenKind {
        use std::str::FromStr;
        match TokenKind::from_str(word) {
            Result::Ok(token_kind) => token_kind,
            _ => TokenKind::Unknown,
        }
    }

    pub fn is_binary_op(self) -> bool {
        match self {
            TokenKind::StrConcat
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::EqualEqual
            | TokenKind::NotEqual
            | TokenKind::Inf
            | TokenKind::InfEq
            | TokenKind::Sup
            | TokenKind::SupEq
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mult
            | TokenKind::Div
            | TokenKind::Modulo
            | TokenKind::Dot
            | TokenKind::As
            | TokenKind::Is
            | TokenKind::Arrow
            | TokenKind::ColonColon => true,
            _ => false,
        }
    }

    pub fn is_litteral_value(self) -> bool {
        match self {
            TokenKind::Integer
            | TokenKind::Real
            | TokenKind::LineString
            | TokenKind::BlockString
            | TokenKind::Null
            | TokenKind::NullId
            | TokenKind::False
            | TokenKind::True => true,
            _ => false,
        }
    }

    pub fn is_hash(self) -> bool {
        match self {
            TokenKind::Include
            | TokenKind::Const
            | TokenKind::Setting
            | TokenKind::RequireContext
            | TokenKind::Extends => true,
            | TokenKind::Struct => true,
            _ => false,
        }
    }

    pub fn is_assign_op(self) -> bool {
        match self {
            TokenKind::Equal
            | TokenKind::Alias
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::MultEq
            | TokenKind::DivEq
            | TokenKind::StrEq
            | TokenKind::AndEq
            | TokenKind::OrEq => true,
            _ => false,
        }
    }
}
