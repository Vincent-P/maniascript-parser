use crate::ast::node_kind::*;
use crate::ast::*;
use crate::lexer::trivia_kind::*;
use crate::lexer::token_kind::*;

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

        leading_newlines =  leading_newlines || ctx.lines_before > 0;

        let spaces = if leading_newlines {
            ctx.indent * 4
        } else {
            ctx.space_before
        };

        let mut has_inserted_newline = false;
        let mut leading_lines = 0;
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

                        if leading_lines < newlines {
                            leadings.push(TriviaKind::Newline(newlines));
                        }
                        leading_lines = 0;
                        if spaces > 0 {
                            leadings.push(TriviaKind::Space(spaces));
                        }
                    }
                    leadings.push(trivia.clone());
                }
                t if t.is_line_break() && ctx.lines_before > 0  => {
                    leading_lines += t.line_count();
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
            if leading_lines < newlines {
                leadings.push(TriviaKind::Newline(newlines-leading_lines));
            }

            if spaces > 0 {
                leadings.push(TriviaKind::Space(spaces));
            }
        }

        let mut last_comment = 0;
        for i in 0..trailings.len() {
            match trailings[i] {
                TriviaKind::LineComment(_) | TriviaKind::BlockComment(_) => {
                    last_comment = i;
                }
                _ => {}
            }
        }

        for i in last_comment+1..trailings.len() {
            trailings.pop();
        }

        let trailing_spaces = if let Some(TriviaKind::Space(s)) = trailings.last() {
            *s
        } else {
            0
        };

        if ctx.space_after > 0 && trailing_spaces < ctx.space_after {
            trailings.push(TriviaKind::Space(ctx.space_after));
        }
        else if trailing_spaces > ctx.space_after {
            trailings.pop();
            if ctx.space_after != 0 {
                trailings.push(TriviaKind::Space(ctx.space_after));
            }
        }

        let mut new_token = t.clone();
        new_token.leading_trivia = leadings.into_boxed_slice();
        new_token.trailing_trivia = trailings.into_boxed_slice();
        node.kind = NodeKind::Token(new_token);
    }
}

fn format_rec(children: &[Vec<NodeId>], nodes: &mut Vec<Node>, n: usize, mut ctx: FormatContext) {
    let node_kind = nodes[n].kind.clone();

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
                ctx.lines_before = 1;
            }

            for label in &f.labels {
                format_rec(children, nodes, *label, ctx);
                ctx.lines_before = 1;
            }

            for function in &f.functions {
                format_rec(children, nodes, *function, ctx);
                ctx.lines_before = 1;
            }

            if let Some(eof) = f.get_eof() {
                format_rec(children, nodes, *eof, ctx);
            }
        }

        NodeKind::Include(i) => {
            ctx.space_after = 1;

            if let Some(child) = i.get_include() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;

            if let Some(child) = i.get_path() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_as_() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = 0;

            if let Some(child) = i.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Const(c) => {
            ctx.space_after = 1;

            if let Some(child) = c.get_const_() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;

            if let Some(child) = c.get_name() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = 0;

            if let Some(child) = c.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Setting(i) => {
            ctx.space_after = 1;

            if let Some(child) = i.get_setting() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;

            if let Some(child) = i.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = 0;
            if let Some(child) = i.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::RequireContext(i) => {
            ctx.space_after = 1;

            if let Some(child) = i.get_require_context() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = i.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Extends(i) => {
            ctx.space_after = 1;

            if let Some(child) = i.get_extends() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = i.get_path() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::VarDec(f) => {
            ctx.space_after = 1;

            if !children[f.syntax()].is_empty() {
                format_rec(children, nodes, children[f.syntax()][0], ctx);
            }

            ctx.lines_before = 0;

            for i in 1..children[f.syntax()].len()-2 {
                format_rec(children, nodes, children[f.syntax()][i], ctx);
            }

            ctx.space_after = 0;
            if children[f.syntax()].len() > 2 {
                format_rec(children, nodes, children[f.syntax()][children[f.syntax()].len()-2], ctx);
            }
            if children[f.syntax()].len() > 1 {
                format_rec(children, nodes, children[f.syntax()][children[f.syntax()].len()-1], ctx);
            }
        }

        NodeKind::FormalArg(f) => {
            let old_ctx = ctx.clone();

            if let Some(child) = f.get_type_() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = 0;
            if let Some(child) = f.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = old_ctx.space_after;

            if let Some(child) = f.get_comma() {
                ctx.space_after = old_ctx.space_after;
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

            ctx.space_after = 1;
            for child in f.get_args() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = 0;

            if let Some(child) = f.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            if let Some(child) = f.get_body() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }

        NodeKind::LabelImpl(l) => {
            ctx.lines_before = 3;
            ctx.space_after = 0;

            if let Some(child) = l.get_stars1() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;
            if let Some(child) = l.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = l.get_stars2() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 1;
            if let Some(child) = l.get_stars3() {
                format_rec(children, nodes, *child, ctx);
            }
            for child in l.get_statements() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = l.get_stars4() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::If(i) => {
            let old_lines = ctx.lines_before;

            ctx.space_after = 1;
            if let Some(child) = i.get_if_() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;

            ctx.space_after = 0;
            if let Some(child) = i.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_condition() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = i.get_body() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }

            ctx.lines_before = 1;

            if let Some(child) = i.get_else_() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Else(i) => {
            let old_lines = ctx.lines_before;

            ctx.space_after = if i.get_if_().is_none() {
                0
            } else {
                1
            };

            if let Some(child) = i.get_else_() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = i.get_if_() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = old_lines;

            if let Some(child) = i.get_body() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }

        NodeKind::Switch(i) => {
            let old_lines = ctx.lines_before;

            ctx.space_after = 1;
            if let Some(child) = i.get_switch() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = i.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = i.get_lbrace() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.indent += 1;
            for child in i.get_cases() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_default() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.indent -= 1;
            if let Some(child) = i.get_rbrace() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Case(c) => {
            let old_lines = ctx.lines_before;
            ctx.space_after = 1;

            if let Some(child) = c.get_case() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = c.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = c.get_colon() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = c.get_statement() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }

        NodeKind::Default(c) => {
            let old_lines = ctx.lines_before;
            ctx.space_after = 1;

            if let Some(child) = c.get_default() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = c.get_colon() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = c.get_statement() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }

        NodeKind::For(i) => {
            let old_lines = ctx.lines_before;

            ctx.space_after = 1;
            if let Some(child) = i.get_for_() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;

            ctx.space_after = 0;
            if let Some(child) = i.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = 1;
            if let Some(child) = i.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_comma1() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_value_start() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_comma2() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = 0;
            if let Some(child) = i.get_value_end() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = i.get_body() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }

        NodeKind::Foreach(i) => {
            let old_lines = ctx.lines_before;

            ctx.space_after = 1;
            if let Some(child) = i.get_foreach() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;

            ctx.space_after = 0;
            if let Some(child) = i.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = 1;
            if let Some(child) = i.get_name1() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_arrow() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_name2() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_in_() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = 0;
            if let Some(child) = i.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = i.get_body() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }

        NodeKind::While(i) => {
            let old_lines = ctx.lines_before;

            ctx.space_after = 1;
            if let Some(child) = i.get_while_() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;
            ctx.space_after = 0;

            if let Some(child) = i.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_condition() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = i.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = old_lines;

            if let Some(child) = i.get_body() {
                ctx.indent += 1;
                format_rec(children, nodes, *child, ctx);
                ctx.indent -= 1;
            }
        }


        NodeKind::Block(b) => {
            ctx.lines_before = 1;
            let is_indented = ctx.indent > 0;

            if is_indented {
                ctx.indent -= 1;
            }

            if let Some(child) = b.get_lbrace() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.indent += 1;

            for child in b.get_statements() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.indent -= 1;

            if let Some(child) = b.get_rbrace() {
                format_rec(children, nodes, *child, ctx);
            }

            if is_indented {
                ctx.indent += 1;
            }

        }

        NodeKind::Parenthesised(p) => {
            let old_ctx = ctx.clone();
            ctx.space_after = 0;

            if let Some(child) = p.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.lines_before = 0;
            if let Some(child) = p.get_expr() {
                format_rec(children, nodes, *child, ctx);
            }
            ctx.space_after = old_ctx.space_after;
            if let Some(child) = p.get_rparen() {
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

        NodeKind::Type(t) => {
            let old_ctx = ctx.clone();
            ctx.space_after = 0;
            for i in 0..children[t.syntax()].len()-1 {
                format_rec(children, nodes, children[t.syntax()][i], ctx);
            }
            ctx.space_after = old_ctx.space_after;
            if !children[t.syntax()].is_empty() {
                format_rec(children, nodes, *children[t.syntax()].last().unwrap(), ctx);
            }
        }

        NodeKind::Return(t) => {
            let old_ctx = ctx.clone();

            ctx.space_after = if t.get_value().is_none() {
                0
            } else {
                1
            };

            if let Some(child) = t.get_return_() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            ctx.space_after = old_ctx.space_after;
            if let Some(child) = t.get_value() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::LabelCall(l) => {
            ctx.space_after = 0;
            if let Some(child) = l.get_start() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = l.get_name() {
                format_rec(children, nodes, *child, ctx);
            }
            if let Some(child) = l.get_end() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Assignment(a) => {
            ctx.space_after = 1;

            if let Some(child) = a.get_lvalue() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;

            if let Some(child) = a.get_operator() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = 0;

            if let Some(child) = a.get_rvalue() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::Vector(a) => {
            if let Some(child) = a.get_langle() {
                format_rec(children, nodes, *child, ctx);
            }

            for (value, comma) in a.get_values() {
                format_rec(children, nodes, *value, ctx);

                if let Some(child) = comma {
                    ctx.space_after = 1;
                    format_rec(children, nodes, *child, ctx);
                    ctx.space_after = 0;
                }
            }

            if let Some(child) = a.get_rangle() {
                format_rec(children, nodes, *child, ctx);
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

        NodeKind::UnOp(o) => {
            let old_ctx = ctx.clone();
            ctx.space_after = 0;

            if let Some(child) = o.get_operator() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = old_ctx.space_after;

            if let Some(child) = o.get_operand() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::BinaryOp(o) => {
            let should_space = match o.get_operator() {
                Some(tn) => {
                    let token = &nodes[*tn];
                    match token.kind {
                        NodeKind::Token(ref t) if t.kind == TokenKind::ColonColon || t.kind == TokenKind::Dot => false,
                        _ => true
                    }
                }
                _ => false
            };

            let old_safter = ctx.space_after;
            if should_space {
                ctx.space_after = 1;
            } else {
                ctx.space_after = 0;
            }

            if let Some(child) = o.get_lhs() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;

            if let Some(child) = o.get_operator() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.space_after = old_safter;

            if let Some(child) = o.get_rhs() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        NodeKind::ArrayAccess(f) => {
            let old_ctx = ctx.clone();
            ctx.space_after = 0;

            if !children[f.syntax()].is_empty() {
                format_rec(children, nodes, children[f.syntax()][0], ctx);
            }

            ctx.lines_before = 0;

            for i in 1..children[f.syntax()].len()-1 {
                format_rec(children, nodes, children[f.syntax()][i], ctx);
            }

            ctx.space_after = old_ctx.space_after;
            if children[f.syntax()].len() > 1 {
                format_rec(children, nodes, children[f.syntax()][children[f.syntax()].len()-1], ctx);
            }
        }

        NodeKind::FunctionCall(f) => {
            let old_safter = ctx.space_after;

            ctx.space_after = 0;

            if let Some(child) = f.get_lhs() {
                format_rec(children, nodes, *child, ctx);
            }

            ctx.lines_before = 0;
            if let Some(child) = f.get_lparen() {
                format_rec(children, nodes, *child, ctx);
            }

            for (value, comma) in f.get_args() {
                format_rec(children, nodes, *value, ctx);

                if let Some(child) = comma {
                    ctx.space_after = 1;
                    format_rec(children, nodes, *child, ctx);
                    ctx.space_after = 0;
                }
            }

            ctx.space_after = old_safter;
            if let Some(child) = f.get_rparen() {
                format_rec(children, nodes, *child, ctx);
            }
        }

        // Recursive calls to children
        _ => {
            format_node(&mut nodes[n], ctx);

            for child_id in &children[n] {
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
