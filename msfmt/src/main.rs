use lib_maniascript::ast::printer::{print_ast, print_dot};
use lib_maniascript::lexer::Lexer;
use lib_maniascript::parser::Parser;

use clap::{App, Arg};

use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;
use std::time::Instant;

fn main() -> Result<(), Box<std::error::Error>> {
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
        let file_ext = path.extension().unwrap();

        // Read the file
        let mut input = String::new();
        {
            let mut file = File::open(path)?;
            file.read_to_string(&mut input)?;
        }

        let file_start = Instant::now();

        // parse the file
        let lexer = Lexer::new(&input);

        let mut tree = Parser::new(lexer).parse()?;

        tree.format();

        if matches.is_present("dot") {
            let out_dot = format!("{}.dot", file_stem.to_str().unwrap());
            let mut dotfile = File::create(out_dot)?;
            print_dot(&mut dotfile, &mut tree, 0, &input)?;
        }

        if matches.is_present("overwrite") {
            let mut formated_file = File::create(path)?;
            print_ast(&mut formated_file, &mut tree, 0, &input)?;
        } else {
            print_ast(&mut io::stdout(), &mut tree, 0, &input)?;
        }

        // print(file_stem.to_str().unwrap(), &tree, 0, &input)?;
        println!(
            "{} processed in {}ms.",
            file_stem.to_str().unwrap(),
            file_start.elapsed().as_millis()
        );
    }

    println!("Done in {}ms.", main_start.elapsed().as_millis());

    Ok(())
}
