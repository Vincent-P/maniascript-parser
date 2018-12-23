#![feature(duration_as_u128)]

mod ast;
mod ast_printer;
mod lexer;
mod parser;
mod token;
mod token_kind;
mod trivia_kind;

use crate::ast_printer::print;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::fs::File;
use std::time::Instant;
use std::io::Read;
use std::env;

fn main() -> Result<(), Box<std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(if args.len() == 1 { "test.Script.txt" } else { &args[1] })?;

    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let now = Instant::now();

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let tree = parser.parse_file()?;
    print(&tree, &input)?;

    println!("Done in {}ms.", now.elapsed().as_millis());

    Ok(())
}
