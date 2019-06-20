use lib_maniascript::lexer::Lexer;
use lib_maniascript::parser::Parser;
use lib_maniascript::ast::printer::{print_dot, print_ast};

use clap::{App, Arg};

use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::time::Instant;

fn main() -> Result<(), Box<std::error::Error>> {
    let matches = App::new("MSfmt")
        .arg(Arg::with_name("files")
             .help("the files to be formatted")
             .index(1)
             .required(true)
             .multiple(true)
        )
        .get_matches();

    let main_start = Instant::now();
    for file in matches.values_of("files").unwrap() {


        let path = Path::new(file);
        // file name without extension
        let file_stem = path.file_stem().unwrap();
        let file_ext = path.extension().unwrap();

        // Read the file
        let mut file = File::open(path)?;
        let mut input = String::new();
        file.read_to_string(&mut input)?;

        let file_start = Instant::now();

        // parse the file
        let lexer = Lexer::new(&input);

        let mut tree = Parser::new(lexer).parse()?;

        tree.format();

        let out_dot = format!("{}.dot", file_stem.to_str().unwrap());
        let mut dotfile = File::create(out_dot)?;
        print_dot(&mut dotfile, &mut tree, 0, &input)?;

        let out_file = format!("formatted-{}.{}", file_stem.to_str().unwrap(), file_ext.to_str().unwrap());
        let mut formated_file = File::create(out_file)?;
        print_ast(&mut formated_file, &mut tree, 0, &input)?;

        // print(file_stem.to_str().unwrap(), &tree, 0, &input)?;
        println!("{} processed in {}ms.", file_stem.to_str().unwrap(), file_start.elapsed().as_millis());
    }

    println!("Done in {}ms.", main_start.elapsed().as_millis());

    Ok(())
}
