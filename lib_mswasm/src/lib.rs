mod utils;

use std::fmt::Write;

use wasm_bindgen::prelude::*;
use lib_maniascript::parser::{self, typed_node::TypedNode};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn dump_ast(code: &str) -> String {
    let ast = parser::parse(code);

    let mut actual = String::new();
    for error in ast.errors() {
        writeln!(actual, "Error: {}", error).unwrap();
    }
    write!(actual, "{}", ast.root().dump()).unwrap();

    actual
}
