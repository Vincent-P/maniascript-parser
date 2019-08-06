use crate::ast::node_kind::NodeKind;
use crate::ast::{NodeId, Tree};
use std::io::prelude::*;

fn print_dot_rec<T: Write>(
    buffer: &mut T,
    tree: &Tree,
    root: NodeId,
    source: &str,
) -> std::io::Result<()> {
    let node = tree.get_node(root).unwrap();

    match &node.kind {
        NodeKind::Token(_) => {
            buffer.write_all(b"    ")?;
            buffer.write_all(root.to_string().as_bytes())?;
            buffer.write_all(b"[label=\"")?;
            buffer.write_all(
                source[node.span.0..node.span.1]
                    .replace("\"", "\\\"")
                    .as_bytes(),
            )?;
            buffer.write_all(b"\"];\n")?;
        }
        _ => {
            let label = format!("    {} [label=\"{:?} {:?}\"];\n", root, node.kind, node.span);
            buffer.write_all(label.as_bytes())?;
        }
    };


    for child in &tree.children[root] {
        let link = format!("    {} -> {};\n", root, *child);
        buffer.write_all(link.as_bytes())?;

        print_dot_rec(buffer, tree, *child, source)?;
    }

    Ok(())
}

/* Print an AST Tree in DOT format in a Write object.
 * The dot format is used in the graphviz tools.
 */
pub fn print_dot<T: Write>(
    buffer: &mut T,
    tree: &Tree,
    root: NodeId,
    source: &str,
) -> std::io::Result<()> {
    buffer.write_all(b"digraph ast {\n")?;
    print_dot_rec(buffer, tree, root, source)?;
    buffer.write_all(b"}\n")?;

    Ok(())
}

/* Print an AST Tree as text in a Write object.
 * If the AST is exactly the one parsed from a file, print_ast will write the exact same content as the file.
 */
pub fn print_ast<T: Write>(
    buffer: &mut T,
    tree: &Tree,
    root: NodeId,
    source: &str,
) -> std::io::Result<()> {
    let node = tree.get_node(root).unwrap();

    if let NodeKind::Token(token) = &node.kind {
        for trivia in token.leading_trivia.iter() {
            buffer.write_all(trivia.to_print().as_bytes())?;
        }

        let span = token.span();
        buffer.write_all(source[span.0..span.1].as_bytes())?;

        for trivia in token.trailing_trivia.iter() {
            buffer.write_all(trivia.to_print().as_bytes())?;
        }
    }

    for child in &tree.children[root] {
        print_ast(buffer, tree, *child, source)?;
    }

    Ok(())
}
