use crate::ast::node_kind::*;
use crate::ast::*;
use super::r#type::Type;

#[derive(Debug)]
pub struct Symbol {
    name: String,
    type_: Type
}


impl Symbol {
    pub fn new(name: String, type_: Type) -> Self {
        Symbol {
            name,
            type_
        }
    }

    pub fn dummy(name: String) -> Self {
        Symbol::new(name, Type::Dummy)
    }
}

fn unchecked_find_symbols_rec(children: &[Vec<NodeId>], nodes: &Vec<Node>, n: usize, text: &str, symbols: &mut Vec<Symbol>) {
    let node_kind = nodes[n].kind.clone();

    match node_kind {
        NodeKind::VarDec(v) => {
            let name_idx = v.get_name().unwrap();
            match nodes[name_idx].kind {
                NodeKind::Token(ref t) => {
                    let (start, end) = t.span();
                    let symbol = Symbol::dummy(text[start..end].to_string());
                    symbols.push(symbol);
                }
                _ => {}
            }
        }

        NodeKind::FuncDec(f) => {
            let name_idx = f.get_name().unwrap();
            match nodes[name_idx].kind {
                NodeKind::Token(ref t) => {
                    let (start, end) = t.span();
                    let symbol = Symbol::dummy(text[start..end].to_string());
                    symbols.push(symbol);
                }
                _ => {}
            }
        }
        _ => {}
    }

    // Recursive calls to children
    for child_id in &children[n] {
        unchecked_find_symbols_rec(children, nodes, *child_id, text, symbols);
    }
}

pub fn unchecked_find_symbols(tree: &Tree, text: &str) -> Vec<Symbol> {
    let mut symbols = vec![];
    unchecked_find_symbols_rec(&tree.children, &tree.nodes, 0, text, &mut symbols);
    symbols
}
