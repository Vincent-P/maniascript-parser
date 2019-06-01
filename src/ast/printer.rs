use crate::ast::node_kind::NodeKind;
use crate::ast::{NodeId, Tree};

use std::collections::hash_map::DefaultHasher;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::prelude::*;
use std::path::Path;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

fn print_dot_file(tree: &Tree, root: NodeId, source: &str, file: &mut File) -> std::io::Result<()> {
    let node = tree.get_node(root).unwrap();
    let hashed_root = calculate_hash(&root);

    match &node.kind {
        NodeKind::Token(_) => {
            file.write_all(b"    ")?;
            file.write_all(hashed_root.to_string().as_bytes())?;
            file.write_all(b"[label=\"")?;
            file.write_all(
                source[node.span.0..node.span.1]
                    .replace("\"", "\\\"")
                    .as_bytes(),
            )?;
            file.write_all(b"\"];\n")?;
        }
        _ => {
            let label = format!("    {} [label=\"{:?}\"];\n", hashed_root, node.kind);
            file.write_all(label.as_bytes())?;
        }
    };

    for child in &tree.children[root] {
        let link = format!("    {} -> {};\n", hashed_root, calculate_hash(&child));
        file.write_all(link.as_bytes())?;

        print_dot_file(tree, *child, source, file)?;
    }

    Ok(())
}

pub fn print_dot(tree: &Tree, root: NodeId, source: &str) -> std::io::Result<()> {
    let path = Path::new("out/tree.dot");

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = File::create(&path)?;

    file.write_all(b"digraph ast {\n")?;
    print_dot_file(tree, root, source, &mut file)?;
    file.write_all(b"}\n")?;

    Ok(())
}

pub fn print_ast_file(
    tree: &Tree,
    root: NodeId,
    source: &str,
    file: &mut File,
) -> std::io::Result<()> {
    let node = tree.get_node(root).unwrap();

    if let NodeKind::Token(token) = &node.kind {
        for trivia in token.leading_trivia.iter() {
            file.write_all(trivia.to_print().as_bytes())?;
        }

        let span = token.span();
        file.write_all(source[span.0..span.1].as_bytes())?;

        for trivia in token.trailing_trivia.iter() {
            file.write_all(trivia.to_print().as_bytes())?;
        }
    }

    for child in &tree.children[root] {
        print_ast_file(tree, *child, source, file)?;
    }

    Ok(())
}

pub fn print_ast(tree: &Tree, root: NodeId, source: &str) -> std::io::Result<()> {
    let path = Path::new("out/tree.txt");
    let mut file = File::create(&path)?;
    print_ast_file(tree, root, source, &mut file)?;
    Ok(())
}

pub fn print(tree: &Tree, root: NodeId, source: &str) -> std::io::Result<()> {
    print_dot(tree, root, source)?;
    print_ast(tree, root, source)?;
    Ok(())
}
