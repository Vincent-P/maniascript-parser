#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    // Internals
    TOKEN_COMMENT,
    TOKEN_ERROR,
    TOKEN_WHITESPACE,

    // Keywords
    TOKEN_BREAK,
    TOKEN_YIELD,
    TOKEN_CONTINUE,
    TOKEN_RETURN,
    TOKEN_DECLARE,
    TOKEN_AS,
    TOKEN_FOR,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_SWITCH,
    TOKEN_SWITCHTYPE,
    TOKEN_CASE,
    TOKEN_DEFAULT,
    TOKEN_WHILE,
    TOKEN_FOREACH,
    TOKEN_IN,
    TOKEN_IS,
    TOKEN_NETWRITE,
    TOKEN_NETREAD,
    TOKEN_PERSISTENT,
    TOKEN_METADATA,

    // Directives
    TOKEN_INCLUDE,
    TOKEN_CONST,
    TOKEN_SETTING,
    TOKEN_REQUIRE_CONTEXT,
    TOKEN_EXTENDS,
    TOKEN_STRUCT,

    // Symbols
    TOKEN_LABEL_STAR,
    TOKEN_LABEL_PLUS,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_COLON_COLON,
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_SQUARE,
    TOKEN_CLOSE_SQUARE,
    TOKEN_OPEN_BRACE,
    TOKEN_CLOSE_BRACE,
    TOKEN_ARROW,

    // Operators
    TOKEN_NOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_MULT,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_CONCAT,
    TOKEN_EQ_EQ,
    TOKEN_NOT_EQ,
    TOKEN_LESS,
    TOKEN_LESS_OR_EQ,
    TOKEN_MORE,
    TOKEN_MORE_OR_EQ,
    TOKEN_AND,
    TOKEN_OR,

    // Asignment
    TOKEN_EQUAL,
    TOKEN_ALIAS,
    TOKEN_PLUS_EQ,
    TOKEN_MINUS_EQ,
    TOKEN_MULT_EQ,
    TOKEN_DIV_EQ,
    TOKEN_CONCAT_EQ,
    TOKEN_AND_EQ,
    TOKEN_OR_EQ,
    TOKEN_MOD_EQ,

    // Identifiers and values
    TOKEN_IDENT,
    TOKEN_INTEGER,
    TOKEN_REAL,
    TOKEN_INTERPOL_START,
    TOKEN_INTERPOL_END,
    TOKEN_STRING_START,
    TOKEN_STRING_CONTENT,
    TOKEN_STRING_END,

    // Internals
    NODE_ROOT,
    NODE_ERROR,

    // Directives
    NODE_INCLUDE,
    NODE_CONST,
    NODE_SETTING,
    NODE_REQUIRE_CONTEXT,
    NODE_EXTENDS,
    NODE_STRUCT,

    NODE_STRUCT_FIELD,
    NODE_VAR_DECL,
    NODE_FUNC_DECL,
    NODE_FORMAL_ARG, // need formal arg??
    NODE_LABEL_DECL,

    // Control flow
    NODE_IF_ELSE,
    NODE_SWITCH,
    NODE_CASE, // need case and default??
    NODE_DEFAULT,
    NODE_FOR,
    NODE_FOREACH,
    NODE_WHILE,

    // Statements
    NODE_STATEMENT,
    NODE_CONTINUE,
    NODE_BREAK,
    NODE_RETURN,
    NODE_YIELD,
    NODE_LABEL_CALL,
    NODE_ASSIGNMENT,

    // Expressions
    NODE_EXPRESSION, // not used for now, just inside error node

    NODE_BLOCK,
    NODE_PARENTHESISED,

    NODE_IDENTIFIER,
    NODE_STRING,
    NODE_STRING_INTERPOL,
    NODE_LITERAL,
    NODE_VECTOR,
    NODE_ARRAY,
    NODE_UNARY_OP,
    NODE_BINARY_OP,

    NODE_ARRAY_ACCESS,
    NODE_FUNCTION_CALL,
    NODE_STRUCT_INIT,

    NODE_TYPE,

    #[doc(hidden)]
    __LAST,
}
use SyntaxKind::*;

impl SyntaxKind {
    pub fn is_literal(self) -> bool {
        match self {
            TOKEN_INTEGER | TOKEN_REAL => true,
            // NODE_STRING?
            _ => false,
        }
    }

    pub fn is_keyword(self) -> bool {
        match self {
            TOKEN_BREAK | TOKEN_YIELD | TOKEN_CONTINUE | TOKEN_RETURN | TOKEN_DECLARE
            | TOKEN_AS | TOKEN_FOR | TOKEN_IF | TOKEN_ELSE | TOKEN_SWITCH | TOKEN_SWITCHTYPE
            | TOKEN_CASE | TOKEN_DEFAULT | TOKEN_WHILE | TOKEN_FOREACH | TOKEN_IN | TOKEN_IS
            | TOKEN_NETWRITE | TOKEN_NETREAD | TOKEN_PERSISTENT | TOKEN_METADATA => true,
            _ => false,
        }
    }

    pub fn is_directive(self) -> bool {
        match self {
            TOKEN_INCLUDE
            | TOKEN_CONST
            | TOKEN_SETTING
            | TOKEN_REQUIRE_CONTEXT
            | TOKEN_EXTENDS
            | TOKEN_STRUCT => true,
            _ => false,
        }
    }

    pub fn is_operator(self) -> bool {
        match self {
            TOKEN_NOT | TOKEN_MINUS | TOKEN_PLUS | TOKEN_MULT | TOKEN_DIV | TOKEN_MOD
            | TOKEN_CONCAT | TOKEN_EQ_EQ | TOKEN_NOT_EQ | TOKEN_LESS | TOKEN_LESS_OR_EQ
            | TOKEN_MORE | TOKEN_MORE_OR_EQ | TOKEN_AND | TOKEN_OR => true,
            _ => false,
        }
    }

    /// Returns true if this token is a binary operation
    /// the parser will create a BINARY_OP node from them
    pub fn is_binary_operator(self) -> bool {
        match self {
            TOKEN_MINUS | TOKEN_PLUS | TOKEN_MULT | TOKEN_DIV | TOKEN_MOD | TOKEN_CONCAT
            | TOKEN_EQ_EQ | TOKEN_NOT_EQ | TOKEN_LESS | TOKEN_LESS_OR_EQ | TOKEN_MORE
            | TOKEN_MORE_OR_EQ | TOKEN_AND | TOKEN_OR | TOKEN_AS | TOKEN_IN | TOKEN_ARROW
            | TOKEN_DOT | TOKEN_COLON_COLON | TOKEN_IS => true,
            _ => false,
        }
    }

    pub const ASSIGNMENT_OPERATORS: &'static [Self] = &[
        TOKEN_EQUAL,
        TOKEN_ALIAS,
        TOKEN_PLUS_EQ,
        TOKEN_MINUS_EQ,
        TOKEN_MULT_EQ,
        TOKEN_DIV_EQ,
        TOKEN_CONCAT_EQ,
        TOKEN_AND_EQ,
        TOKEN_OR_EQ,
        TOKEN_MOD_EQ,
    ];

    pub fn is_assignment_operator(self) -> bool {
        match self {
            TOKEN_EQUAL | TOKEN_ALIAS | TOKEN_PLUS_EQ | TOKEN_MINUS_EQ | TOKEN_MULT_EQ
            | TOKEN_DIV_EQ | TOKEN_CONCAT_EQ | TOKEN_AND_EQ | TOKEN_OR_EQ | TOKEN_MOD_EQ => true,
            _ => false,
        }
    }

    /// Returns true if this token is a comment, whitespace, or similar, and
    /// should be skipped over by the parser.
    pub fn is_trivia(self) -> bool {
        match self {
            TOKEN_COMMENT | TOKEN_ERROR | TOKEN_WHITESPACE => true,
            _ => false,
        }
    }

    // when token is used in the middle of an expression
    pub fn lbp(self) -> i32 {
        match self {
            TOKEN_DOT | TOKEN_COLON_COLON | TOKEN_ARROW => 9, // ??

            TOKEN_CONCAT => 7, // ??

            TOKEN_MULT | TOKEN_DIV | TOKEN_MOD => 6,
            TOKEN_PLUS | TOKEN_MINUS => 5,

            TOKEN_LESS | TOKEN_LESS_OR_EQ | TOKEN_MORE | TOKEN_MORE_OR_EQ => 4,
            TOKEN_EQ_EQ | TOKEN_NOT_EQ => 3,

            TOKEN_AND => 2,
            TOKEN_OR => 1,
            TOKEN_IS | TOKEN_AS => 1, // ??
            TOKEN_OPEN_PAREN | TOKEN_OPEN_SQUARE => 1, // ???

            _ => -1,
        }
    }

    // when token is used to start an expression
    pub fn rbp(self) -> i32 {
        match self {
            TOKEN_MINUS | TOKEN_NOT => 8,

            TOKEN_OPEN_PAREN | TOKEN_OPEN_SQUARE => 1,
            TOKEN_LESS => 5,
            _ => -1,
        }
    }
}
