pub mod symbol;
pub mod r#type;

use crate::ast::Tree;
use symbol::Symbol;

#[derive(Debug)]
pub struct Error;

#[derive(Debug)]
pub struct Document {
    text: String,
    tree: Tree,
    errors: Vec<Error>,
    symbols: Vec<Symbol>
}

impl Document {
    pub fn new(source: &str) -> Self {
        Document {
            text: source.to_string(),
            tree: Tree::new(),
            errors: vec![],
            symbols: vec![],
        }
    }

    pub fn from_string(text: String) -> Self {
        Document {
            text,
            tree: Tree::new(),
            errors: vec![],
            symbols: vec![],
        }
    }
}
