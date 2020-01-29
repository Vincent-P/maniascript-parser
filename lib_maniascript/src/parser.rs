mod language;
mod syntax_kind;
mod tokenizer;
mod typed_node;
mod parser;

// export to modules
use tokenizer::Tokenizer;
// export to other crates
pub use syntax_kind::SyntaxKind;
pub use parser::{AST, ParseError as NewParseError, TextRange};
pub use typed_node::TypedNode;

/// A convenience function for first tokenizing and then parsing given input
pub fn parse(input: &str) -> AST {
    parser::parse(Tokenizer::new(input))
}
