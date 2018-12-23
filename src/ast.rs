use crate::token::Token;
use crate::token_kind::TokenKind;
use std::string::ToString;
use strum_macros::ToString;

#[derive(Debug, ToString)]
pub enum ExpressionKind {
    Identifier,
    Literal,
    Vector,
    Array,

    Not,
    Negative,

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

#[derive(Debug, ToString)]
pub enum NodeKind {
    Include,
    Const,
    Setting,
    RequireContext,
    Extends,
    FuncDec,
    LabelDec,
    LabelCall,
    VarDec,
    If,
    Else,
    Switch,
    SwitchType,
    Case,
    CaseType,
    Default,
    For,
    Foreach,
    While,
    Expr(ExpressionKind),
    Type,
    Token(Token),
    File,
    Block,
    Continue,
    Break,
    Return,
    Assignment,
}

#[derive(Debug)]
pub struct Node {
    pub node_kind: NodeKind,
    pub span: (usize, usize),
    pub children: Vec<Node>,
}

impl Node {
    pub fn new(node_kind: NodeKind) -> Node {
        Node {
            node_kind,
            span: (0, 0),
            children: vec![],
        }
    }

    pub fn new_expr(expr_kind: ExpressionKind) -> Node {
        Node::new(NodeKind::Expr(expr_kind))
    }

    fn with_span(node_kind: NodeKind, span: (usize, usize)) -> Node {
        Node {
            node_kind,
            span,
            children: vec![],
        }
    }

    pub fn add_child(&mut self, child: Node) {
        if self.children.is_empty() {
            self.span = child.span;
        } else {
            let (start, _) = self.span;
            let (_, end) = child.span;
            self.span = (start, end);
        }
        self.children.push(child)
    }

    pub fn format_dot(&self, source_file: &str) -> String {
        match &self.node_kind {
            NodeKind::Expr(e) => e.to_string(),
            NodeKind::Token(t) => {
                if t.kind == TokenKind::BlockString {
                    return "Multiline String".to_string();
                }
                let (begin, end) = self.span;
                source_file[begin..end].to_string()
            }
            k => k.to_string(),
        }
    }
}

impl From<Token> for Node {
    fn from(token: Token) -> Node {
        let span = (token.position, token.position + token.len);
        let kind = NodeKind::Token(token);
        Node::with_span(kind, span)
    }
}

impl From<&Token> for Node {
    fn from(token: &Token) -> Node {
        let kind = NodeKind::Token(token.clone());
        Node::with_span(kind, (token.position, token.position + token.len))
    }
}

impl From<TokenKind> for ExpressionKind {
    fn from(token_kind: TokenKind) -> ExpressionKind {
        match token_kind {
            TokenKind::StrConcat => ExpressionKind::StrConcat,
            TokenKind::And => ExpressionKind::And,
            TokenKind::Or => ExpressionKind::Or,
            TokenKind::EqualEqual => ExpressionKind::IsEqual,
            TokenKind::NotEqual => ExpressionKind::IsNotEqual,
            TokenKind::Inf => ExpressionKind::Inf,
            TokenKind::InfEq => ExpressionKind::InfEq,
            TokenKind::Sup => ExpressionKind::Sup,
            TokenKind::SupEq => ExpressionKind::SupEq,
            TokenKind::Plus => ExpressionKind::Plus,
            TokenKind::Minus => ExpressionKind::Minus,
            TokenKind::Mult => ExpressionKind::Mult,
            TokenKind::Div => ExpressionKind::Div,
            TokenKind::Modulo => ExpressionKind::Modulo,
            TokenKind::Dot => ExpressionKind::Member,
            TokenKind::ColonColon => ExpressionKind::Namespace,
            TokenKind::Identifier => ExpressionKind::Identifier,
            TokenKind::As => ExpressionKind::Cast,
            TokenKind::Is => ExpressionKind::Is,
            TokenKind::Arrow => ExpressionKind::MapsTo,
            k if k.is_litteral_value() => ExpressionKind::Literal,
            _ => panic!(format!(
                "An ExpressionKind cannot be created from a {:?}",
                token_kind
            )),
        }
    }
}
