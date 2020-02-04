mod utils;

use std::fmt::Write;

use wasm_bindgen::prelude::*;
use lib_maniascript::{parser::{self, typed_node::TypedNode}, symbols};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct AstInfos {
    ast: parser::AST,
    env: symbols::Environment,
    dump: String,
}

#[wasm_bindgen]
impl AstInfos {

    #[wasm_bindgen(getter)]
    pub fn dump(&self) -> String {
        self.dump.clone()
    }
}

#[wasm_bindgen]
pub fn infos_from_string(code: &str) -> AstInfos
{
    let ast = parser::parse(code);
    let mut env = symbols::Environment::new();
    env.fill(&ast);

    let mut dump = String::new();
    write!(dump, "{}", ast.root().dump()).unwrap();

    if ast.errors().is_empty() {
        env.check_errors(&ast);
    }

    AstInfos {
        ast,
        env,
        dump
    }
}
