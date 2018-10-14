#![feature(duration_as_u128)]

extern crate strum;
#[macro_use]
extern crate strum_macros;

mod lexer;
mod token_kind;
mod trivia_kind;

use lexer::Lexer;
use std::fs::File;
use std::time::Instant;

fn main() {
    use std::io::Read;

    let mut file = File::open("test.Script.txt").expect("Unable to open the file");
    let mut input = String::new();
    file.read_to_string(&mut input)
        .expect("Unable to read the file");

    let now = Instant::now();

    let lexer = Lexer::new(&input);

    for thing in lexer {
        println!("{:?}", thing);
    }

    println!("Done in {}ms.", now.elapsed().as_millis());
}
