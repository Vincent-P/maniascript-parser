pub mod symbol;
pub mod r#type;

use crate::ast::Tree;
use crate::lexer::*;
use crate::parser::*;
use crate::validator::*;
use symbol::*;

#[derive(Debug)]
pub enum DocumentError {
    ParseError(ParseError),
    ValidationError(ValidationError),
}

#[derive(Debug)]
pub struct Document {
    filename: String,
    text: String,
    tree: Tree,
    errors: Vec<DocumentError>,
    symbols: Vec<Symbol>
}

impl Document {
    pub fn errors(&self) -> &Vec<DocumentError> {
        &self.errors
    }

    pub fn update(&mut self, new_source: &str) {
        self.text = new_source.to_string();
        let lexer = Lexer::new(&self.text);

        self.errors.clear();

        match Parser::new(lexer).parse() {
            Ok(tree) => self.tree = tree,
            // Exit early if there is a parsing error
            Err(e) => {self.errors.push(DocumentError::ParseError(e)); return;}
        };

        // If there are validation errors push them and exit
        let validation_errors = self.tree.validate();
        if !validation_errors.is_empty() {
            if self.errors.capacity() < validation_errors.len() {
                self.errors.reserve(validation_errors.len() - self.errors.capacity());
            }

            for e in validation_errors {
                self.errors.push(DocumentError::ValidationError(e));
            }

            return;
        }

        // Update symbols if there are no errors
        self.symbols = unchecked_find_symbols(&self.tree, &self.text);
    }

    pub fn new(filename: &str, source: &str) -> Self {
        Document {
            filename: filename.to_string(),
            text: source.to_string(),
            tree: Tree::new(),
            errors: vec![],
            symbols: vec![],
        }
    }

    pub fn from_string(filename: &str, text: String) -> Self {
        Document {
            filename: filename.to_string(),
            text,
            tree: Tree::new(),
            errors: vec![],
            symbols: vec![],
        }
    }
}
