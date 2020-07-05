//use lib_maniascript::{ast::printer::{print_ast, print_dot}, lexer::Lexer, parser::Parser};

use clap::{App, Arg};

use std::{fs::File, io::{self, Read}, path::Path, time::Instant};
use lib_maniascript::{parser::{self, typed_node::TypedNode}, symbols};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = App::new("MSfmt - ManiaScript formatter")
        .version("0.1.0")
        .arg(
            Arg::with_name("dot")
                .short("d")
                .long("dot")
                .help("Generates an AST of the file in the dot format"),
        )
        .arg(
            Arg::with_name("overwrite")
                .short("o")
                .long("overwrite")
                .help("Overwrite the input files, if not present the output is the stdout"),
        )
        .arg(
            Arg::with_name("files")
                .help("the files to be formatted")
                .index(1)
                .required(true)
                .multiple(true),
        )
        .get_matches();

    let main_start = Instant::now();
    for file in matches.values_of("files").unwrap() {
        let path = Path::new(file);
        // file name without extension
        let file_stem = path.file_stem().unwrap();

        // Read the file
        let mut input = String::new();
        {
            let mut file = File::open(path)?;
            file.read_to_string(&mut input)?;
        }

        let file_start = Instant::now();

        let tree = parser::parse(&input);

        println!("Tree:");
        println!("{}", tree.root().dump());

        println!(
            "{} processed in {}ms.",
            file_stem.to_str().unwrap(),
            file_start.elapsed().as_millis()
        );
    }

    println!("Done in {}ms.", main_start.elapsed().as_millis());

    Ok(())
}
