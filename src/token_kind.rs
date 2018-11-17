#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString)]
pub enum TokenKind {
    #[strum(disabled="true")]
    Unknown,
    #[strum(disabled="true")]
    EOF,
    #[strum(disabled="true")]
    Identifier,
    #[strum(disabled="true")]
    Integer,
    #[strum(disabled="true")]
    Real,
    #[strum(disabled="true")]
    LineString,
    #[strum(disabled="true")]
    BlockString,
    #[strum(serialize="***")]
    LabelStar,
    #[strum(serialize="---")]
    LabelMinus,
    #[strum(serialize="+++")]
    LabelPlus,
    #[strum(serialize=".")]
    Dot,
    #[strum(serialize=",")]
    Comma,
    #[strum(serialize=";")]
    Semicolon,
    #[strum(serialize="(")]
    OpenParen,
    #[strum(serialize=")")]
    CloseParen,
    #[strum(serialize="[")]
    OpenSquare,
    #[strum(serialize="]")]
    CloseSquare,
    #[strum(serialize="{")]
    OpenBrace,
    #[strum(serialize="}")]
    CloseBrace,
    #[strum(serialize="::")]
    ColonColon,

    // Operator
    #[strum(serialize="!")]
    Not,
    #[strum(serialize="-")]
    Minus,
    #[strum(serialize="+")]
    Plus,
    #[strum(serialize="*")]
    Mult,
    #[strum(serialize="/")]
    Div,
    #[strum(serialize="%")]
    Modulo,
    #[strum(serialize="^")]
    StrConcat,

    //
    #[strum(serialize="==")]
    EqualEqual,
    #[strum(serialize="!=")]
    NotEqual,
    #[strum(serialize="<")]
    Inf,
    #[strum(serialize="<=")]
    InfEq,
    #[strum(serialize=">")]
    Sup,
    #[strum(serialize=">=")]
    SupEq,
    #[strum(serialize="&&")]
    And,
    #[strum(serialize="||")]
    Or,

    // Assignment
    #[strum(serialize="=")]
    Equal,
    #[strum(serialize="<=>")]
    Alias,
    #[strum(serialize="+=")]
    PlusEq,
    #[strum(serialize="-=")]
    MinusEq,
    #[strum(serialize="*=")]
    MultEq,
    #[strum(serialize="/=")]
    DivEq,
    #[strum(serialize="^=")]
    StrEq,
    #[strum(serialize="&=")]
    AndEq,
    #[strum(serialize="|=")]
    OrEq,
    #[strum(serialize="=>")]
    Arrow,

    // Keywords
    #[strum(serialize="#Include")]
    Include,
    #[strum(serialize="#Const")]
    Const,
    #[strum(serialize="#Setting")]
    Setting,
    #[strum(serialize="#RequireContext")]
    RequireContext,
    #[strum(serialize="#Extends")]
    Extends,

    #[strum(serialize="break")]
    Break,
    #[strum(serialize="yield")]
    Yield,
    #[strum(serialize="continue")]
    Continue,
    #[strum(serialize="return")]
    Return,
    #[strum(serialize="declare")]
    Declare,
    #[strum(serialize="as")]
    As,
    #[strum(serialize="for")]
    For,
    #[strum(serialize="if")]
    If,
    #[strum(serialize="else")]
    Else,
    #[strum(serialize="switch")]
    Switch,
    #[strum(serialize="switchtype")]
    SwitchType,
    #[strum(serialize="case")]
    Case,
    #[strum(serialize="default")]
    Default,
    #[strum(serialize="while")]
    While,
    #[strum(serialize="foreach")]
    Foreach,
    #[strum(serialize="in")]
    In,
    #[strum(serialize="is")]
    Is,
    #[strum(serialize="netwrite")]
    Netwrite,
    #[strum(serialize="netread")]
    Netread,
    #[strum(serialize="persistent")]
    Persistent,
    #[strum(serialize="metadata")]
    Metadata,

    // Constants
    Null,
    NullId,
    True,
    False,
}

impl TokenKind {
    pub fn from_str_or_unknown(word: &str) -> TokenKind {
        use ::std::str::FromStr;
        match TokenKind::from_str(word) {
            Result::Ok(token_kind) => token_kind,
            _ => TokenKind::Unknown
        }
    }
}
