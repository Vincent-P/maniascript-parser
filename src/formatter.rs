use crate::ast::node_kind::*;
use crate::ast::*;
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

fn format_node(node: &mut Node, ctx: FormatContext) {
    if let NodeKind::Token(t) = &node.kind {
        let mut leadings = vec![];
        let mut trailings = t.trailing_trivia.to_vec();

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

        let mut has_inserted_newline = false;
        for trivia in t.leading_trivia.iter() {
            match trivia {
                TriviaKind::LineComment(_) | TriviaKind::BlockComment(_) => {
                    if ctx.lines_before > 0 {
                        let newlines = if has_inserted_newline {
                            1
                        } else {
                            has_inserted_newline = true;
                            ctx.lines_before
                        };

                        leadings.push(TriviaKind::Newline(newlines));
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
            let newlines = if has_inserted_newline {
                1
            } else {
                ctx.lines_before
            };
            leadings.push(TriviaKind::Newline(newlines));

            if spaces > 0 {
                leadings.push(TriviaKind::Space(spaces));
            }
        }

        trailings.retain(is_not_space);
        if ctx.space_after > 0 {
            trailings.push(TriviaKind::Space(ctx.space_after));
        }

        let mut new_token = t.clone();
        new_token.leading_trivia = leadings.into_boxed_slice();
        new_token.trailing_trivia = trailings.into_boxed_slice();
        node.kind = NodeKind::Token(new_token);
    }
}

fn format_rec(children: &[Vec<NodeId>], nodes: &mut Vec<Node>, i: usize, mut ctx: FormatContext) {
    let node_kind = nodes[i].kind.clone();

    // Update the formatting context based on the nodes
    match node_kind {
        NodeKind::File(f) => {
            ctx.lines_before = 0;
            for hash in &f.hashes {
                format_rec(children, nodes, *hash, ctx);
                ctx.lines_before = 1;
            }

            for global in &f.globals {
                format_rec(children, nodes, *global, ctx);
            }
            for label in &f.labels {
                format_rec(children, nodes, *label, ctx);
            }
            for function in &f.functions {
                format_rec(children, nodes, *function, ctx);
            }
            if let Some(eof) = f.get_eof() {
                format_rec(children, nodes, *eof, ctx);
            }
        }

        NodeKind::Const(c) => {
            ctx.space_after = 1;

            if let Some(child) = c.get_const_() {
                format_rec(children, nodes, *child, ctx);
            }

            if let Some(child) = c.get_name() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = 0;

            if let Some(child) = c.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::FuncDec(f) => {
            ctx.lines_before = 3;
            ctx.space_after = 1;

            if let Some(child) = f.get_type_() {
                format_rec(children, nodes, *child, ctx);
                ctx.lines_before = 0;
            }

            ctx.space_after = 0;

            if let Some(child) = f.get_name() {
                format_rec(children, nodes, *child, ctx);
                ctx.lines_before = 0;
            }

            if let Some(child) = f.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }

            for child in &f.args {
                format_rec(children, nodes, *child, ctx);
            }

            if let Some(child) = f.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            if let Some(child) = f.get_body() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Block(b) => {
            ctx.lines_before = 1;

            if let Some(child) = b.get_lbrace() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.indent += 4;

            for child in &b.statements {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.indent -= 4;

            if let Some(child) = b.get_rbrace() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Statement(s) => {
            if let Some(child) = s.get_statement() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;

            if let Some(child) = s.get_semicolon() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Expr => {
            for child_id in &children[i] {
                format_rec(children, nodes, *child_id, ctx);
                ctx.lines_before = 0;
            }
        }

        NodeKind::Array(a) => {
            if let Some(child) = a.get_lsquare() {
                format_rec(children, nodes, *child, ctx);
            }

            for (value, comma) in &a.values {
                format_rec(children, nodes, *value, ctx);

                if let Some(child) = comma {
                    ctx.space_after = 1;
                    format_rec(children, nodes, *child, ctx);
                    ctx.space_after = 0;
                }
            }

            if let Some(child) = a.get_rsquare() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::FunctionCall(f) => {
            if let Some(child) = f.get_lhs() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = f.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }

            for (value, comma) in &f.args {
                format_rec(children, nodes, *value, ctx);

                if let Some(child) = comma {
                    ctx.space_after = 1;
                    format_rec(children, nodes, *child, ctx);
                    ctx.space_after = 0;
                }
            }

            if let Some(child) = f.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        // Recursive calls to children
        _ => {
            format_node(&mut nodes[i], ctx);

            for child_id in &children[i] {
                format_rec(children, nodes, *child_id, ctx);
            }
        }
    }
}

impl Tree {
    pub fn format(&mut self) {
        let ctx = FormatContext {
            indent: 0,
            lines_before: 1,
            space_before: 0,
            space_after: 0,
        };
        format_rec(&self.children, &mut self.nodes, 0, ctx);
    }
}
