use std::collections::HashMap;
use lsp_types::Url;
use lsp_server::Connection;
use lib_maniascript::parser::AST;


mod main_loop;
mod handlers;


pub struct App {
    files: HashMap<Url, (AST, String)>,
    conn: Connection,
}

impl App {
    pub fn new(conn: Connection) -> Self {
        App {
            files: HashMap::new(),
            conn
        }
    }
}
