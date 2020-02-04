use std::convert::TryFrom;
use std::fmt;
use std::collections::HashMap;
use std::cmp::Ordering;

use crate::parser::{
    self,
    language::SyntaxNode,
    typed_node::{NamedNode, ParsedType, Root, TokenWrapper, TypedNode, VarDecl, WalkEvent, BinOpKind, BinaryOp},
    SyntaxKind, AST,
};
use rowan::{SmolStr, TextRange, TextUnit};

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Void,
    Integer,
    Real,
    Boolean,
    Text,
    Ident,
    Vec2,
    Vec3,
    Int3,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    value: Type,
    index: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    name: SmolStr,
    members: HashMap<SmolStr, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(Box<ArrayType>),
    Struct(StructType),
    Namespace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope {
    Global,
    Range(TextRange),
}

impl Ord for Scope {
    fn cmp(&self, other: &Self) -> Ordering {
        use Scope::*;

        match (self, other) {
            (Global, Global) => Ordering::Equal,
            (Global, Range(_)) => Ordering::Greater,
            (Range(_), Global) => Ordering::Less,
            (Range(a), Range(b)) => a.len().cmp(&b.len())
        }
    }
}

impl PartialOrd for Scope {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct Symbol {
    type_: Type,
    name: SmolStr,
    scope: Scope,
    definition: Option<ParsedType>,
    references: Vec<ParsedType>,
}

impl PartialEq<Symbol> for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.type_ == other.type_ && self.name == other.name && self.scope == other.scope
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    UnexpectedType(TextRange, Type, Type),
    Undefined(TextRange, SmolStr),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::UnexpectedType(_, expecting, got) => write!(f, "Expecting an expression of type {:?} but the type is {:?}.", expecting, got),
            TypeError::Undefined(_, name) => write!(f, "{} is undefined.", name),
        }
    }
}

impl std::error::Error for TypeError {}

#[derive(Debug, Default)]
pub struct Environment {
    pub symbols: HashMap<SmolStr, Vec<Symbol>>,
    pub errors: Vec<TypeError>,
}

fn find_closest_scope(node: &SyntaxNode) -> Option<Scope> {
    let parent = node.parent().unwrap();
    let mut scope: Option<Scope> = None;
    match parent.kind() {
        SyntaxKind::NODE_ROOT => {
            scope = Some(Scope::Global);
        }

        SyntaxKind::NODE_BLOCK => {
            let start = node.text_range().end();
            let end = parent.text_range().end();
            scope = Some(Scope::Range(TextRange::from_to(start, end)));
        }

        _ => (),
    }
    scope
}

fn range_diff(inner_range: &TextRange, outer_range: &TextRange) -> TextUnit {
    assert!(inner_range.is_subrange(outer_range));
    (inner_range.start() - outer_range.start()) + (outer_range.end() - inner_range.end())
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
            errors: vec![]
        }
    }

    fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols
            .entry(symbol.name.clone())
            .or_insert_with(Vec::new)
            .push(symbol)
    }

    pub fn fill(&mut self, ast: &AST) {
        let root = ast.node();

        for event in root.preorder() {
            if let WalkEvent::Enter(node) = event {
                if let SyntaxKind::NODE_VAR_DECL = node.kind() {
                    let var_decl = VarDecl::cast(node).unwrap();
                    let name = match var_decl.name() {
                        Some(ident) => SmolStr::new(ident.as_str()),
                        None => continue,
                    };
                    let scope = find_closest_scope(var_decl.node());
                    self.add_symbol(Symbol {
                        name,
                        type_: Type::Primitive(PrimitiveType::Void),
                        scope: scope.unwrap(),
                        definition: Some(ParsedType::VarDecl(var_decl)),
                        references: vec![],
                    });
                }
            }
        }

        let root = Root::cast(root).unwrap();
        for function in root.functions() {
            let name = match function.name() {
                Some(ident) => SmolStr::new(ident.as_str()),
                None => continue,
            };

            self.add_symbol(Symbol {
                name,
                type_: Type::Primitive(PrimitiveType::Void),
                scope: Scope::Global,
                definition: Some(ParsedType::FuncDecl(function)),
                references: vec![],
            });
        }

        for const_ in root.consts() {
            let name = match const_.name() {
                Some(ident) => SmolStr::new(ident.as_str()),
                None => continue,
            };

            self.add_symbol(Symbol {
                name,
                type_: Type::Primitive(PrimitiveType::Void),
                scope: Scope::Global,
                definition: Some(ParsedType::Const(const_)),
                references: vec![],
            });
        }
    }


    /// Can be called on nested expression
    fn check_expr_rec(&mut self, parsed_node: ParsedType) {
    }

    /// Called only on top level expression
    fn check_expr(&mut self, parsed_node: ParsedType) {
        match parsed_node {

            ParsedType::Identifier(ident) => {
                let variable_range = ident.node().text_range();
                let variable_name = SmolStr::new(ident.as_str());

                let symbols  = match self.symbols.get(&variable_name) {
                    Some(symbols) => symbols,
                    None => {
                        self.errors.push(TypeError::Undefined(variable_range, variable_name));
                        return;
                    }
                };

                let mut symbols : Vec<&Symbol> = symbols.iter().filter(|s| match s.scope {
                    Scope::Global => true,
                    Scope::Range(range) => variable_range.is_subrange(&range)
                }).collect();

                symbols.sort_by(|a, b| a.scope.cmp(&b.scope));

                let symbol =  symbols.pop();
            }

            // resolve function name
            ParsedType::FunctionCall(func_call) => {
                let function_name = match func_call.name() {
                    Some(ident) => ident,
                    None => return
                };
                // find function and args in environment
                for child in func_call.node().children() {
                    if let Ok(n) = ParsedType::try_from(child) {
                        self.check_expr(n);
                    }
                }
            }

            ParsedType::ArrayAccess(array_access) => {
                for child in array_access.node().children() {
                    if let Ok(n) = ParsedType::try_from(child) {
                        self.check_expr(n);
                    }
                }
            }

            ParsedType::BinaryOp(bin_op) => {
                let lhs = bin_op.lhs();
                let rhs = bin_op.rhs();

                match bin_op.operator_kind() {
                    BinOpKind::Namespace => {
                        // check lhs is imported
                    }

                    BinOpKind::Member => {
                        if let Some(n) = lhs {
                            if let Ok(n) = ParsedType::try_from(n) {
                                self.check_expr(n);
                            }
                        }
                    },

                    // dont know because of setting
                    BinOpKind::As => {
                    }

                    // dont know
                    BinOpKind::Is => {
                    }

                    // dont know
                    BinOpKind::In => {
                    }

                    // Invalid
                    BinOpKind::KeyValue => {
                    }

                    _ => {
                        if let Some(n) = lhs {
                            if let Ok(n) = ParsedType::try_from(n) {
                                self.check_expr(n);
                            }
                        }
                        if let Some(n) = rhs {
                            if let Ok(n) = ParsedType::try_from(n) {
                                self.check_expr(n);
                            }
                        }
                    }

                }
            }
            _ => ()
        }
    }

    pub fn check_errors(&mut self, ast: &AST) {
        let root = ast.node();

        for event in root.preorder() {
            if let WalkEvent::Enter(node) = event {
                if let SyntaxKind::NODE_EXPRESSION = node.kind() {
                    let expr = match node.first_child() {
                        Some(n) => n,
                        None => continue
                    };

                    let parsed_expr = match ParsedType::try_from(expr) {
                        Ok(parsed) => parsed,
                        _ => continue
                    };

                    self.check_expr(parsed_expr);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fill_variable() {
        let code = r#"
declare Void G_GlobalVariable;

main()
{
    declare Void LocalVariable;
    declare Void LocalVariable;
}
"#;
        let ast = parser::parse(code);

        let mut env = Environment::new();
        env.fill(&ast);

        let mut expected: HashMap<SmolStr, Vec<Symbol>> = HashMap::new();

        let main_name = SmolStr::new("main");
        expected
            .entry(main_name.clone())
            .or_insert_with(Vec::new)
            .push(Symbol {
                name: main_name,
                type_: Type::Primitive(PrimitiveType::Void),
                scope: Scope::Global,
                definition: None,
                references: vec![],
            });

        let global_name = SmolStr::new("G_GlobalVariable");
        expected
            .entry(global_name.clone())
            .or_insert_with(Vec::new)
            .push(Symbol {
                name: global_name,
                type_: Type::Primitive(PrimitiveType::Void),
                scope: Scope::Global,
                definition: None,
                references: vec![],
            });

        let local_name = SmolStr::new("LocalVariable");
        expected
            .entry(local_name.clone())
            .or_insert_with(Vec::new)
            .push(Symbol {
                name: local_name.clone(),
                type_: Type::Primitive(PrimitiveType::Void),
                scope: Scope::Range(TextRange::from_to(
                    TextUnit::from_usize(73),
                    TextUnit::from_usize(107),
                )),
                definition: None,
                references: vec![],
            });

        expected
            .entry(local_name.clone())
            .or_insert_with(Vec::new)
            .push(Symbol {
                name: local_name,
                type_: Type::Primitive(PrimitiveType::Void),
                scope: Scope::Range(TextRange::from_to(
                    TextUnit::from_usize(105),
                    TextUnit::from_usize(107),
                )),
                definition: None,
                references: vec![],
            });

        assert_eq!(env.symbols.len(), expected.len());

        for key in expected.keys() {
            assert!(env.symbols.contains_key(key));
            assert_eq!(env.symbols.get(key).unwrap(), expected.get(key).unwrap());
        }
    }
}
