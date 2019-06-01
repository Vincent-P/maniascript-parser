/*
use crate::ast::node_kind::*;
use crate::ast::*;
use crate::lexer::token_kind::*;
use crate::lexer::trivia_kind::*;

#[derive(Debug, Copy, Clone)]
struct FormatContext {
    pub indent: usize,
    pub lines_before: usize,
    pub space_before: usize,
    pub space_after: usize,
}

fn is_not_space(trivia: &TriviaKind) -> bool {
    if let TriviaKind::Space(_) = trivia {
        return false;
    }
    true
}

fn is_not_tab(trivia: &TriviaKind) -> bool {
    if let TriviaKind::Tab(_) = trivia {
        return false;
    }
    true
}

fn is_not_newline(trivia: &TriviaKind) -> bool {
    !trivia.is_line_break()
}

fn format_node(node: &mut Node, mut ctx: FormatContext) {
    if let NodeKind::Token(t) = &node.kind {
        let mut leadings = vec![];
        let mut trailings = t.trailing_trivia.to_vec();

        println!("{:?}", ctx);
        println!("{:?}", node);

        // Remove tabs
        trailings.retain(is_not_tab);

        let mut leading_newlines = false;
        for trivia in t.leading_trivia.iter() {
            if trivia.is_line_break() {
                leading_newlines = true;
                break;
            }
        }

        let spaces = if leading_newlines {
            ctx.indent
        } else {
            ctx.space_before
        };

        for trivia in t.leading_trivia.iter() {
            match trivia {
                TriviaKind::LineComment(_) | TriviaKind::BlockComment(_) => {
                    if ctx.lines_before > 0 {
                        leadings.push(TriviaKind::Newline(1));
                        if spaces > 0 {
                            leadings.push(TriviaKind::Space(spaces));
                        }
                    }
                    leadings.push(trivia.clone());
                }
                _ => {}
            }
        }

        if ctx.lines_before > 0 {
            leadings.push(TriviaKind::Newline(ctx.lines_before));

            if spaces > 0 {
                leadings.push(TriviaKind::Space(spaces));
            }
        }

        if let TokenKind::Comma = t.kind {
            ctx.space_after = 1;
        }

        trailings.retain(is_not_space);
        if ctx.space_after > 0 {
            trailings.push(TriviaKind::Space(ctx.space_after));
        }

        // Remove spaces
        match t.kind {
            TokenKind::Semicolon | TokenKind::ColonColon | TokenKind::Dot => {
                leadings.retain(is_not_space);
                trailings.retain(is_not_space);
            }
            _ => {}
        }

        let mut new_token = t.clone();
        new_token.leading_trivia = leadings.into_boxed_slice();
        new_token.trailing_trivia = trailings.into_boxed_slice();
        node.kind = NodeKind::Token(new_token);

        println!("{:?}", node);
        println!();
    }
}

fn format_rec(
    parents: &[NodeId],
    children: &[Vec<NodeId>],
    nodes: &mut Vec<Node>,
    i: usize,
    mut ctx: FormatContext,
) {
    let node = &mut nodes[i];

    // Update the formatting context based on the nodes
    let mut multiline = false;
    match &node.kind {
        NodeKind::File => {
            multiline = true;
            ctx.lines_before = 2;
        }
        NodeKind::Block => {
            multiline = true;
            ctx.indent += 4;
            ctx.lines_before = 1;
            ctx.space_before = 0;
            ctx.space_after = 1;
        }
        NodeKind::FuncDec => {
            // Start a new line
            ctx.lines_before = 2;
            format_rec(parents, children, nodes, children[i][0], ctx);

            // Stay on the same line, dont put a space between 'name ('
            ctx.lines_before = 0;
            ctx.space_after = 0;
            format_rec(parents, children, nodes, children[i][1], ctx);

            // print the rest
            for j in 2..children[i].len() {
                format_rec(parents, children, nodes, children[i][j], ctx);
            }

            return;
        }
        NodeKind::ExprStatement => {
            ctx.lines_before = 1;
            ctx.space_before = 0;
            ctx.space_after = 0;
        }
        _ => {}
    }

    format_node(node, ctx);
    let mut first = false;
    for child_id in &children[i] {
        format_rec(parents, children, nodes, *child_id, ctx);

        if first || !multiline && ctx.lines_before > 0 {
            ctx.lines_before = 0;
        }
        first = false;
    }
}

impl Tree {
    pub fn format(&mut self) {
        let ctx = FormatContext {
            indent: 0,
            lines_before: 1,
            space_before: 0,
            space_after: 1,
        };
        format_rec(&self.parents, &self.children, &mut self.nodes, 0, ctx);
    }
}
*/
