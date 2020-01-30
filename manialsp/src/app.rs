use lib_maniascript::parser::AST;
use lsp_server::Connection;
use lsp_types::Url;
use std::collections::HashMap;

mod handlers;
mod main_loop;

pub struct App {
    files: HashMap<Url, (AST, String)>,
    conn: Connection,
}

impl App {
    pub fn new(conn: Connection) -> Self {
        App {
            files: HashMap::new(),
            conn,
        }
    }
}
