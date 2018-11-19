#![feature(duration_as_u128)]

mod lexer;
mod parser;
mod token_kind;
mod trivia_kind;
mod ast;

use crate::lexer::Lexer;
use crate::parser::Parser;
use std::fs::File;
use std::time::Instant;

fn main() {
    use std::io::Read;

    let mut file = File::open("HungerGames.Script.txt").expect("Unable to open the file");
    let mut input = String::new();
    file.read_to_string(&mut input)
        .expect("Unable to read the file");

    let now = Instant::now();

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    match parser.parse_file() {
        Ok(e) => println!("{:?}", e),
        Err(e) => println!("{:#?}", e),
    }

    println!("Done in {}ms.", now.elapsed().as_millis());
}
