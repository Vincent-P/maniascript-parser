use lib_macros::AstNode;
use crate::lexer::token::Token;

#[derive(Debug)]
pub enum NodeKind {
    // Root
    File(File),

    // Top level structures
    Include(Include),
    Const(Const),
    Setting(Setting),
    RequireContext(RequireContext),
    Extends(Extends),
    VarDec(VarDec),
    FormalArg(FormalArg),
    FuncDec(FuncDec),
    LabelImpl(LabelImpl),

    // Control flow
    If(If),
    Else(Else),
    Switch(Switch),
    Case(Case),
    Default(DefaultCase),
    For(For),
    Foreach(Foreach),
    While(While),

    // Block of codes
    Block(Block),
    Parenthesised(Parenthesised),

    // Each statement is a Statement node with one child which is the specific statement
    Statement(Statement),
    Continue,
    Break,
    Return,
    Yield,
    LabelCall(LabelCall),
    Assignment(Assignment),

    // Expressions, same as statement
    Expr,
    Identifier,
    Literal,
    Vector(Vector),
    Array(Array),
    UnOp(UnOp),
    BinaryOp(BinaryOp),
    // bin ops?
    // Cast(Cast),
    // Is(Is),
    // MapsTo(MapsTo),
    ArrayAccess(ArrayAccess),
    FunctionCall(FunctionCall),

    Type(Type),

    // A single token (leafs)
    Token(Token),
    Dummy
}

pub type NodeId = usize;
pub type NodeRef = Option<NodeId>;

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub span: (usize, usize),
}

pub trait AstNode {
    fn syntax(&self) -> NodeId;
}

impl Default for Node {
    fn default() -> Self {
        Node {
            kind: NodeKind::Dummy,
            span: (0, 0),
        }
    }
}

#[derive(AstNode, Default, Debug)]
pub struct File {
    syntax: NodeId,
    hashes: Vec<NodeId>,
    globals: Vec<NodeId>,
    labels: Vec<NodeId>,
    functions: Vec<NodeId>,
    eof: NodeRef
}

impl File {
    pub fn add_hash(&mut self, data: NodeId) {
        self.hashes.push(data);
    }
    pub fn add_global(&mut self, data: NodeId) {
        self.globals.push(data);
    }
    pub fn add_label(&mut self, data: NodeId) {
        self.labels.push(data);
    }
    pub fn add_function(&mut self, data: NodeId) {
        self.functions.push(data);
    }
}

#[derive(AstNode, Default, Debug)]
pub struct Include {
    syntax: NodeId,
    include: NodeRef,
    path: NodeRef,
    as_: NodeRef,
    name: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Const {
    syntax: NodeId,
    const_: NodeRef,
    name: NodeRef,
    value: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Setting {
    syntax: NodeId,
    setting: NodeRef,
    name: NodeRef,
    value: NodeRef,
    as_: NodeRef,
    description: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct RequireContext {
    syntax: NodeId,
    require_context: NodeRef,
    name: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Extends {
    syntax: NodeId,
    extends: NodeRef,
    path: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct VarDec {
    syntax: NodeId,
    declare: NodeRef,
    netread: NodeRef,
    netwrite: NodeRef,
    persistent: NodeRef,
    metadata: NodeRef,
    type_: NodeRef,
    name: NodeRef,
    as_: NodeRef,
    alias: NodeRef,
    for_: NodeRef,
    target: NodeRef,
    assignment: NodeRef,
    value: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct FormalArg {
    syntax: NodeId,
    type_: NodeRef,
    name: NodeRef,
    comma: NodeRef
}

#[derive(AstNode, Default, Debug)]
pub struct FuncDec {
    syntax: NodeId,
    type_: NodeRef,
    name: NodeRef,
    lparen: NodeRef,
    args: Vec<NodeId>,
    rparen: NodeRef,
    body: NodeRef,
}

impl FuncDec {
    pub fn add_arg(&mut self, data: NodeId) {
        self.args.push(data);
    }
}

#[derive(AstNode, Default, Debug)]
pub struct LabelImpl {
    syntax: NodeId,
    stars1: NodeRef,
    name: NodeRef,
    stars2: NodeRef,
    stars3: NodeRef,
    statements: Vec<NodeId>,
    stars4: NodeRef,
}

impl LabelImpl {
    pub fn add_statement(&mut self, data: NodeId) {
        self.statements.push(data);
    }
}

#[derive(AstNode, Default, Debug)]
pub struct If {
    syntax: NodeId,
    if_: NodeRef,
    lparen: NodeRef,
    condition: NodeRef,
    rparen: NodeRef,
    body: NodeRef,
    else_: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Else {
    syntax: NodeId,
    else_: NodeRef,
    if_: NodeRef,
    body: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Switch {
    syntax: NodeId,
    switch: NodeRef,
    lparen: NodeRef,
    value: NodeRef,
    rparen: NodeRef,
    lbrace: NodeRef,
    cases: Vec<NodeId>,
    default: NodeRef,
    rbrace: NodeRef,
    is_type: bool,
}

impl Switch {
    pub fn add_case(&mut self, data: NodeId) {
        self.cases.push(data);
    }
}

#[derive(AstNode, Default, Debug)]
pub struct Case {
    syntax: NodeId,
    case: NodeRef,
    value: NodeRef,
    colon: NodeRef,
    statement: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct DefaultCase {
    syntax: NodeId,
    default: NodeRef,
    colon: NodeRef,
    statement: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct For {
    syntax: NodeId,
    for_: NodeRef,
    lparen: NodeRef,
    name: NodeRef,
    comma1: NodeRef,
    value_start: NodeRef,
    comma2: NodeRef,
    value_end: NodeRef,
    rparen: NodeRef,
    body: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Foreach {
    syntax: NodeId,
    foreach: NodeRef,
    lparen: NodeRef,
    name1: NodeRef,
    arrow: NodeRef,
    name2: NodeRef,
    in_: NodeRef,
    value: NodeRef,
    rparen: NodeRef,
    body: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct While {
    syntax: NodeId,
    while_: NodeRef,
    lparen: NodeRef,
    condition: NodeRef,
    rparen: NodeRef,
    body: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Block {
    syntax: NodeId,
    lbrace: NodeRef,
    statements: Vec<NodeId>,
    rbrace: NodeRef,
}

impl Block {
    pub fn add_statement(&mut self, data: NodeId) {
        self.statements.push(data);
    }
}

#[derive(AstNode, Default, Debug)]
pub struct Parenthesised {
    syntax: NodeId,
    lparen: NodeRef,
    expr: NodeRef,
    rparen: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Statement {
    syntax: NodeId,
    statement: NodeRef,
    semicolon: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Type {
    syntax: NodeId,
    basename: NodeRef,
    members: Vec<(NodeId, NodeId)>,
    arrays: Vec<(NodeId, NodeRef, NodeId)>,
}

impl Type {
    pub fn add_member(&mut self, coloncolon: NodeId, member: NodeId) {
        self.members.push((coloncolon, member));
    }
    pub fn add_array(&mut self, open: NodeId, array_type: NodeRef, close: NodeId) {
        self.arrays.push((open, array_type, close));
    }
}

#[derive(AstNode, Default, Debug)]
pub struct LabelCall {
    syntax: NodeId,
    start: NodeRef,
    name: NodeRef,
    end: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Assignment {
    syntax: NodeId,
    lvalue: NodeRef,
    operator: NodeRef,
    rvalue: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct Vector {
    syntax: NodeId,
    langle: NodeRef,
    values: Vec<(NodeId, NodeRef)>,
    rangle: NodeRef,
}

impl Vector {
    pub fn add_value(&mut self, value: NodeId, comma: NodeRef) {
        self.values.push((value, comma));
    }
}

#[derive(AstNode, Default, Debug)]
pub struct Array {
    syntax: NodeId,
    lsquare: NodeRef,
    values: Vec<(NodeId, NodeRef)>,
    rsquare: NodeRef,
}

impl Array {
    pub fn add_value(&mut self, value: NodeId, comma: NodeRef) {
        self.values.push((value, comma));
    }
}

#[derive(AstNode, Default, Debug)]
pub struct UnOp {
    syntax: NodeId,
    operator: NodeRef,
    operand: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct BinaryOp {
    syntax: NodeId,
    lhs: NodeRef,
    operator: NodeRef,
    rhs: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct ArrayAccess {
    syntax: NodeId,
    lhs: NodeRef,
    lsquare: NodeRef,
    index: NodeRef,
    rsquare: NodeRef,
}

#[derive(AstNode, Default, Debug)]
pub struct FunctionCall {
    syntax: NodeId,
    lhs: NodeRef,
    lparen: NodeRef,
    args: Vec<(NodeId, NodeRef)>,
    rparen: NodeRef,
}

impl FunctionCall {
    pub fn add_arg(&mut self, arg: NodeId, comma: NodeRef) {
        self.args.push((arg, comma));
    }
}
