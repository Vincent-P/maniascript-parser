mod ast;
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::ast::printer::print;

use std::env;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

fn main() -> Result<(), Box<std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(if args.len() == 1 {
        "test.Script.txt"
    } else {
        &args[1]
    })?;

    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let now = Instant::now();

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let root = parser.parse_file()?;
    print(&parser.tree, root, &input)?;

    println!("Done in {}ms.", now.elapsed().as_millis());

    Ok(())
}
