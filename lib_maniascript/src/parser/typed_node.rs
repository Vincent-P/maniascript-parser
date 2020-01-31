use std::fmt;

pub use rowan::{NodeOrToken, WalkEvent};

use crate::parser::{
    language::{SyntaxElement, SyntaxNode, SyntaxToken},
    SyntaxKind::{self, *},
};

macro_rules! typed {
    ($($kind:expr => $name:ident$(: $trait:ident)*$(: { $($block:tt)* })*),*) => {
        $(
            #[derive(Clone, Debug)]
            pub struct $name(SyntaxNode);

            impl TypedNode for $name {
                fn cast(from: SyntaxNode) -> Option<Self> {
                    if from.kind() == $kind {
                        Some(Self(from))
                    } else {
                        None
                    }
                }
                fn node(&self) -> &SyntaxNode {
                    &self.0
                }
            }
            $(impl $trait for $name {})*
            $(impl $name { $($block)* })*
        )*
    }
}
macro_rules! nth {
    ($self:expr; $index:expr) => {
        $self.node().children()
            .nth($index)
    };
    ($self:expr; ($kind:ident) $index:expr) => {
        nth!($self; $index).and_then($kind::cast)
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Concat,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessOrEq,
    More,
    MoreOrEq,
    Add,
    Sub,
    Mult,
    Div,
    Mod,

    Member,
    As,
    Is,
    In,
    KeyValue,
    Namespace,
}

impl BinOpKind {
    pub fn from_token(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_CONCAT => Some(BinOpKind::Concat),
            TOKEN_AND => Some(BinOpKind::And),
            TOKEN_OR => Some(BinOpKind::Or),
            TOKEN_EQ_EQ => Some(BinOpKind::Equal),
            TOKEN_NOT_EQ => Some(BinOpKind::NotEqual),
            TOKEN_LESS => Some(BinOpKind::Less),
            TOKEN_LESS_OR_EQ => Some(BinOpKind::LessOrEq),
            TOKEN_MORE => Some(BinOpKind::More),
            TOKEN_MORE_OR_EQ => Some(BinOpKind::MoreOrEq),
            TOKEN_PLUS => Some(BinOpKind::Add),
            TOKEN_MINUS => Some(BinOpKind::Sub),
            TOKEN_MULT => Some(BinOpKind::Mult),
            TOKEN_DIV => Some(BinOpKind::Div),
            TOKEN_MOD => Some(BinOpKind::Mod),

            TOKEN_DOT => Some(BinOpKind::Member),
            TOKEN_AS => Some(BinOpKind::As),
            TOKEN_IS => Some(BinOpKind::Is),
            TOKEN_IN => Some(BinOpKind::In),
            TOKEN_ARROW => Some(BinOpKind::KeyValue),
            TOKEN_COLON_COLON => Some(BinOpKind::Namespace),

            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Not,
    Negate,
}

impl UnaryOpKind {
    /// Get the operation kind from a token in the AST
    pub fn from_token(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_NOT => Some(UnaryOpKind::Not),
            TOKEN_MINUS => Some(UnaryOpKind::Negate),
            _ => None,
        }
    }
}

/// A struct that prints out the textual representation of a node in a
/// stable format. See TypedNode::dump.
pub struct TextDump(SyntaxNode);

impl fmt::Display for TextDump {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        let mut skip_newline = true;
        for event in self.0.preorder_with_tokens() {
            if skip_newline {
                skip_newline = false;
            } else {
                writeln!(f)?;
            }
            match &event {
                WalkEvent::Enter(enter) => {
                    write!(f, "{:i$}{:?}", "", enter.kind(), i = indent)?;
                    if let NodeOrToken::Token(token) = enter {
                        write!(f, "(\"{}\")", token.text().escape_default())?
                    }
                    write!(
                        f,
                        " {}..{}",
                        enter.text_range().start(),
                        enter.text_range().end()
                    )?;
                    if let NodeOrToken::Node(_) = enter {
                        write!(f, " {{")?;
                    }
                    indent += 2;
                }
                WalkEvent::Leave(leave) => {
                    indent -= 2;
                    if let NodeOrToken::Node(_) = leave {
                        write!(f, "{:i$}}}", "", i = indent)?;
                    } else {
                        skip_newline = true;
                    }
                }
            }
        }
        Ok(())
    }
}

/// Internal function to get an iterator over non-trivia tokens
pub(crate) fn tokens(node: &SyntaxNode) -> impl Iterator<Item = SyntaxToken> {
    node.children_with_tokens()
        .filter_map(|element| element.into_token())
        .filter(|token| !token.kind().is_trivia())
}

/// A TypedNode is simply a wrapper around an untyped node to provide a type
/// system in some sense.
pub trait TypedNode: Clone {
    /// Cast an untyped node into this strongly-typed node. This will return
    /// None if the type was not correct.
    fn cast(from: SyntaxNode) -> Option<Self>;
    /// Return a reference to the inner untyped node
    fn node(&self) -> &SyntaxNode;
    /// Return all errors of all children, recursively
    fn errors(&self) -> Vec<SyntaxElement> {
        self.node()
            .descendants_with_tokens()
            // Empty errors can happen if it encounteres EOF while
            // creating them, which in case a root error is added.
            //.filter(|node| !node.text_range().is_empty())
            .filter(|node| node.kind() == NODE_ERROR || node.kind() == TOKEN_ERROR)
            .collect()
    }
    /// Return the first non-trivia token
    fn first_token(&self) -> Option<SyntaxToken> {
        tokens(self.node()).next()
    }
    /// Return a dump of the AST. One of the goals is to be a stable
    /// format that can be used in tests.
    fn dump(&self) -> TextDump {
        TextDump(self.node().clone())
    }
}

pub trait TokenWrapper: TypedNode {
    fn as_str(&self) -> &str {
        self.node()
            .green()
            .children()
            .next()
            .unwrap()
            .as_token()
            .unwrap()
            .text()
            .as_str()
    }
}

/*
// TODO: maybe for arrays, vectors?
/// Provides the function `.entries()`
pub trait EntryHolder: TypedNode {
    /// Return an iterator over all key=value entries
    fn entries(&self) -> Box<dyn Iterator<Item = KeyValue>> {
        Box::new(self.node().children().filter_map(KeyValue::cast))
    }
    /// Return an iterator over all inherit entries
    fn inherits(&self) -> Box<dyn Iterator<Item = Inherit>> {
        Box::new(self.node().children().filter_map(Inherit::cast))
    }
}
 */

/// Provides the function `.inner()` for wrapping types like parenthensis
pub trait Wrapper: TypedNode {
    /// Return the inner value
    fn inner(&self) -> Option<SyntaxNode> {
        nth!(self; 0)
    }
}

/// Provides the function `.name()` for nodes whose first identifier is a name
pub trait NamedNode: TypedNode {
    /// Return the name of the node
    fn name(&self) -> Option<Identifier> {
        self.node()
            .children()
            .find(|child| child.kind() == NODE_IDENTIFIER)
            .and_then(Identifier::cast)
    }
}

#[derive(Debug)]
pub struct ParsedTypeError(pub SyntaxKind);

#[derive(Debug)]
pub enum ParsedType {
    Root(Root),

    Include(Include),
    Const(Const),
    Setting(Setting),
    RequireContext(RequireContext),
    Extends(Extends),
    Struct(Struct),

    StructField(StructField), // TODO: not needed?
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
    FormalArg(FormalArg), // TODO: not needed?
    LabelDecl(LabelDecl),

    IfElse(IfElse),
    Switch(Switch),
    Case(Case),       // TODO: not needed?
    Default(Default), // TODO: not needed?
    For(For),
    Foreach(Foreach),
    While(While),

    Continue(Continue), // TODO: not needed?
    Break(Break),       // TODO: not needed?
    Return(Return),     // TODO: not needed?
    Yield(Yield),       // TODO: not needed?
    LabelCall(LabelCall),
    Assignment(Assignment),

    Block(Block),
    Parenthesised(Parenthesised),

    Identifier(Identifier),
    Str(Str),
    Literal(Literal),
    Vector(Vector),
    Array(Array),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    ArrayAccess(ArrayAccess),
    FunctionCall(FunctionCall),
    StructInit(StructInit),

    Type(Type),
}

impl core::convert::TryFrom<SyntaxNode> for ParsedType {
    type Error = ParsedTypeError;

    fn try_from(node: SyntaxNode) -> Result<Self, ParsedTypeError> {
        match node.kind() {
            NODE_ROOT => Ok(ParsedType::Root(Root::cast(node).unwrap())),

            NODE_INCLUDE => Ok(ParsedType::Include(Include::cast(node).unwrap())),
            NODE_CONST => Ok(ParsedType::Const(Const::cast(node).unwrap())),
            NODE_SETTING => Ok(ParsedType::Setting(Setting::cast(node).unwrap())),
            NODE_REQUIRE_CONTEXT => Ok(ParsedType::RequireContext(
                RequireContext::cast(node).unwrap(),
            )),
            NODE_EXTENDS => Ok(ParsedType::Extends(Extends::cast(node).unwrap())),
            NODE_STRUCT => Ok(ParsedType::Struct(Struct::cast(node).unwrap())),

            NODE_STRUCT_FIELD => Ok(ParsedType::StructField(StructField::cast(node).unwrap())),
            NODE_VAR_DECL => Ok(ParsedType::VarDecl(VarDecl::cast(node).unwrap())),
            NODE_FUNC_DECL => Ok(ParsedType::FuncDecl(FuncDecl::cast(node).unwrap())),
            NODE_FORMAL_ARG => Ok(ParsedType::FormalArg(FormalArg::cast(node).unwrap())),
            NODE_LABEL_DECL => Ok(ParsedType::LabelDecl(LabelDecl::cast(node).unwrap())),

            NODE_IF_ELSE => Ok(ParsedType::IfElse(IfElse::cast(node).unwrap())),
            NODE_SWITCH => Ok(ParsedType::Switch(Switch::cast(node).unwrap())),
            NODE_CASE => Ok(ParsedType::Case(Case::cast(node).unwrap())),
            NODE_DEFAULT => Ok(ParsedType::Default(Default::cast(node).unwrap())),
            NODE_FOR => Ok(ParsedType::For(For::cast(node).unwrap())),
            NODE_FOREACH => Ok(ParsedType::Foreach(Foreach::cast(node).unwrap())),
            NODE_WHILE => Ok(ParsedType::While(While::cast(node).unwrap())),

            NODE_CONTINUE => Ok(ParsedType::Continue(Continue::cast(node).unwrap())),
            NODE_BREAK => Ok(ParsedType::Break(Break::cast(node).unwrap())),
            NODE_RETURN => Ok(ParsedType::Return(Return::cast(node).unwrap())),
            NODE_YIELD => Ok(ParsedType::Yield(Yield::cast(node).unwrap())),
            NODE_LABEL_CALL => Ok(ParsedType::LabelCall(LabelCall::cast(node).unwrap())),
            NODE_ASSIGNMENT => Ok(ParsedType::Assignment(Assignment::cast(node).unwrap())),

            NODE_BLOCK => Ok(ParsedType::Block(Block::cast(node).unwrap())),
            NODE_PARENTHESISED => Ok(ParsedType::Parenthesised(
                Parenthesised::cast(node).unwrap(),
            )),

            NODE_IDENTIFIER => Ok(ParsedType::Identifier(Identifier::cast(node).unwrap())),
            NODE_STRING => Ok(ParsedType::Str(Str::cast(node).unwrap())),
            NODE_LITERAL => Ok(ParsedType::Literal(Literal::cast(node).unwrap())),
            NODE_VECTOR => Ok(ParsedType::Vector(Vector::cast(node).unwrap())),
            NODE_ARRAY => Ok(ParsedType::Array(Array::cast(node).unwrap())),
            NODE_UNARY_OP => Ok(ParsedType::UnaryOp(UnaryOp::cast(node).unwrap())),
            NODE_BINARY_OP => Ok(ParsedType::BinaryOp(BinaryOp::cast(node).unwrap())),
            NODE_ARRAY_ACCESS => Ok(ParsedType::ArrayAccess(ArrayAccess::cast(node).unwrap())),
            NODE_FUNCTION_CALL => Ok(ParsedType::FunctionCall(FunctionCall::cast(node).unwrap())),
            NODE_STRUCT_INIT => Ok(ParsedType::StructInit(StructInit::cast(node).unwrap())),

            NODE_TYPE => Ok(ParsedType::Type(Type::cast(node).unwrap())),
            other => Err(ParsedTypeError(other)),
        }
    }
}

// TODO: implems
typed![
    NODE_ROOT => Root: {
        /// Return all the global functions of the file
        pub fn functions(&self) -> impl Iterator<Item = FuncDecl> {
            self.node().children().filter_map(FuncDecl::cast)
        }

        /// Return all the label implementations of the file
        pub fn labels(&self) -> impl Iterator<Item = LabelDecl> {
            self.node().children().filter_map(LabelDecl::cast)
        }

        /// Return all the includes of the file
        pub fn includes(&self) -> impl Iterator<Item = Include> {
            self.node().children().filter_map(Include::cast)
        }

        /// Return all the consts of the file
        pub fn consts(&self) -> impl Iterator<Item = Const> {
            self.node().children().filter_map(Const::cast)
        }

        /// Return all the consts of the file
        pub fn settings(&self) -> impl Iterator<Item = Setting> {
            self.node().children().filter_map(Setting::cast)
        }

        /// Return all the globals of the file
        pub fn globals(&self) -> impl Iterator<Item = VarDecl> {
            self.node().children().filter_map(VarDecl::cast)
        }

        /// Return all the globals of the file
        pub fn structs(&self) -> impl Iterator<Item = Struct> {
            self.node().children().filter_map(Struct::cast)
        }
    },

    NODE_INCLUDE => Include: NamedNode,
    NODE_CONST => Const: NamedNode,
    NODE_SETTING => Setting: NamedNode,
    NODE_REQUIRE_CONTEXT => RequireContext,
    NODE_EXTENDS => Extends,
    NODE_STRUCT => Struct: NamedNode,

    NODE_STRUCT_FIELD => StructField,
    NODE_VAR_DECL => VarDecl: NamedNode,

    NODE_FUNC_DECL => FuncDecl: NamedNode: {
    },

    NODE_FORMAL_ARG => FormalArg,
    NODE_LABEL_DECL => LabelDecl: NamedNode,

    NODE_IF_ELSE => IfElse,
    NODE_SWITCH => Switch,
    NODE_CASE => Case,
    NODE_DEFAULT => Default,
    NODE_FOR => For,
    NODE_FOREACH => Foreach,
    NODE_WHILE => While,

    NODE_CONTINUE => Continue,
    NODE_BREAK => Break,
    NODE_RETURN => Return,
    NODE_YIELD => Yield,
    NODE_LABEL_CALL => LabelCall,
    NODE_ASSIGNMENT => Assignment,

    NODE_BLOCK => Block,
    NODE_PARENTHESISED => Parenthesised,

    NODE_IDENTIFIER => Identifier: TokenWrapper,
    NODE_STRING => Str,
    NODE_LITERAL => Literal,
    NODE_VECTOR => Vector,
    NODE_ARRAY => Array,
    NODE_UNARY_OP => UnaryOp,
    NODE_BINARY_OP => BinaryOp: {
        /// Returns the left hand side of the binary operation
        pub fn lhs(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }

        /// Return the operator kind
        pub fn operator_kind(&self) -> BinOpKind {
            self.first_token().and_then(|t| BinOpKind::from_token(t.kind())).expect("invalid ast")
        }

        /// Returns the right hand side of the binary operation
        pub fn rhs(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_ARRAY_ACCESS => ArrayAccess,
    NODE_FUNCTION_CALL => FunctionCall,
    NODE_STRUCT_INIT => StructInit,

    NODE_TYPE => Type
];
