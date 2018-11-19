use crate::ast::Node;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn ast_print(
    tree: &Node,
    file: &mut File,
    level: &mut usize,
    source_file: &str,
) -> std::io::Result<()> {
    let parent_level = *level;
    *level += 1;

    let mut label_msg = tree.format_dot(source_file);
    label_msg = label_msg.replace("\"", "'");

    let label = format!("    {} [label=\"{}\"];\n", parent_level, label_msg);
    file.write(label.as_bytes())?;

    for child in &tree.children {
        let link = format!("    {} -> {};\n", parent_level, *level);
        file.write(link.as_bytes())?;
        ast_print(&child, file, level, source_file)?;
    }

    Ok(())
}

pub fn print(tree: &Node, source_file: &str) -> std::io::Result<()> {
    let path = Path::new("out/tree.dot");

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = File::create(&path)?;

    let mut level = 0;
    file.write(b"digraph ast {\n")?;
    ast_print(tree, &mut file, &mut level, source_file)?;
    file.write(b"}\n")?;

    Ok(())
}
