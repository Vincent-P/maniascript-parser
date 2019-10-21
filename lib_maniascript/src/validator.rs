use crate::ast::{node_kind::*, *};

#[derive(Debug, Clone, Copy)]
pub struct ValidationError {
    pub span: (usize, usize),
    pub msg: &'static str,
}

macro_rules! not_present {
    ($msg:expr, $errors:expr, $n: expr, $nodes:expr) => {{
        $errors.push(ValidationError {
            span: $nodes[$n].span,
            msg: $msg,
        });
    }};
}

macro_rules! check_present {
    ($e:expr, $msg:expr, $errors:expr, $n: expr, $nodes:expr) => {{
        if let None = $e {
            not_present!($msg, $errors, $n, $nodes);
        }
    }};
}

fn validate_rec(
    children: &[Vec<NodeId>],
    nodes: &Vec<Node>,
    n: usize,
    errors: &mut Vec<ValidationError>,
) {
    let node_kind = nodes[n].kind.clone();

    match node_kind {
        NodeKind::Include(i) => {
            check_present!(i.get_path(), "Missing path", errors, n, nodes);

            match (i.get_as_(), i.get_name()) {
                (Some(_), None) => not_present!("Missing name", errors, n, nodes),
                (None, Some(_)) => not_present!("Missing as", errors, n, nodes),
                _ => {}
            }
        }
        NodeKind::Const(c) => {
            check_present!(c.get_name(), "Missing name", errors, n, nodes);
            check_present!(c.get_value(), "Missing value", errors, n, nodes);
        }

        NodeKind::Setting(i) => {
            check_present!(i.get_name(), "Missing name", errors, n, nodes);
            check_present!(i.get_value(), "Missing value", errors, n, nodes);
        }

        NodeKind::RequireContext(i) => {
            check_present!(i.get_name(), "Missing name", errors, n, nodes);
        }

        NodeKind::Extends(i) => {
            check_present!(i.get_path(), "Missing path", errors, n, nodes);
        }

        NodeKind::StructField(i) => {
            check_present!(
                i.get_name(),
                "Missing struct's field name",
                errors,
                n,
                nodes
            );
            check_present!(
                i.get_semicolon(),
                "Missing struct's field semicolon",
                errors,
                n,
                nodes
            );
        }

        NodeKind::Struct(i) => {
            check_present!(i.get_name(), "Missing struct's name", errors, n, nodes);
            check_present!(i.get_lbrace(), "Missing left brace", errors, n, nodes);
            check_present!(i.get_rbrace(), "Missing right brace", errors, n, nodes);
        }

        NodeKind::VarDec(i) => {
            check_present!(i.get_name(), "Missing name", errors, n, nodes);
            check_present!(i.get_semicolon(), "Missing semicolon", errors, n, nodes);

            match (i.get_as_(), i.get_alias()) {
                (Some(_), None) => not_present!("Missing alias", errors, n, nodes),
                (None, Some(_)) => not_present!("Missing as", errors, n, nodes),
                _ => {}
            }

            match (i.get_for_(), i.get_target()) {
                (Some(_), None) => not_present!("Missing for target", errors, n, nodes),
                (None, Some(_)) => not_present!("Missing for", errors, n, nodes),
                _ => {}
            }
        }

        NodeKind::FormalArg(i) => {
            check_present!(i.get_name(), "Missing argument name", errors, n, nodes);
            // TODO(vincent): It makes a false positive for the last argument
            // check_present!(i.get_comma(), "Missing comma", errors, n, nodes);
        }

        NodeKind::FuncDec(i) => {
            check_present!(i.get_type_(), "Missing return type", errors, n, nodes);
            check_present!(i.get_name(), "Missing name", errors, n, nodes);
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
            check_present!(i.get_body(), "Missing body", errors, n, nodes);
        }

        NodeKind::LabelImpl(i) => {
            check_present!(i.get_stars1(), "Missing label stars", errors, n, nodes);
            check_present!(i.get_stars2(), "Missing label stars", errors, n, nodes);
            check_present!(i.get_stars3(), "Missing label stars", errors, n, nodes);
            check_present!(i.get_stars4(), "Missing label stars", errors, n, nodes);

            check_present!(i.get_name(), "Missing name", errors, n, nodes);
        }

        NodeKind::If(i) => {
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
            check_present!(i.get_condition(), "Missing condition", errors, n, nodes);
            check_present!(i.get_body(), "Missing body", errors, n, nodes);
        }

        NodeKind::Else(i) => match (i.get_if_(), i.get_body()) {
            (Some(_), None) => {}
            (None, Some(_)) => {}
            _ => not_present!(
                "A else can have either an if or an else body, not both.",
                errors,
                n,
                nodes
            ),
        },

        NodeKind::Switch(i) => {
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
            check_present!(i.get_lbrace(), "Missing left brace", errors, n, nodes);
            check_present!(i.get_rbrace(), "Missing right brace", errors, n, nodes);

            check_present!(i.get_value(), "Missing value", errors, n, nodes);
        }

        NodeKind::Case(i) => {
            check_present!(i.get_value(), "Missing value", errors, n, nodes);
            check_present!(i.get_colon(), "Missing colon", errors, n, nodes);
            check_present!(i.get_statement(), "Missing statement", errors, n, nodes);
        }

        NodeKind::Default(i) => {
            check_present!(i.get_colon(), "Missing colon", errors, n, nodes);
            check_present!(i.get_statement(), "Missing statement", errors, n, nodes);
        }

        NodeKind::For(i) => {
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
            check_present!(i.get_comma1(), "Missing first comma", errors, n, nodes);
            check_present!(i.get_comma2(), "Missing second comma", errors, n, nodes);

            check_present!(i.get_name(), "Missing name", errors, n, nodes);
            check_present!(i.get_value_start(), "Missing start value", errors, n, nodes);
            check_present!(i.get_value_end(), "Missing end value", errors, n, nodes);

            check_present!(i.get_body(), "Missing body", errors, n, nodes);
        }

        NodeKind::Foreach(i) => {
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
            check_present!(i.get_name1(), "Missing first name", errors, n, nodes);

            match (i.get_arrow(), i.get_name2()) {
                (Some(_), None) => not_present!("Missing secondary name", errors, n, nodes),
                (None, Some(_)) => {
                    not_present!("Missing arrow between the two names", errors, n, nodes)
                }
                _ => {}
            }

            check_present!(i.get_in_(), "Missing in keyword", errors, n, nodes);
            check_present!(i.get_value(), "Missing value", errors, n, nodes);

            check_present!(i.get_body(), "Missing body", errors, n, nodes);
        }

        NodeKind::While(i) => {
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
            check_present!(i.get_condition(), "Missing condition", errors, n, nodes);
            check_present!(i.get_body(), "Missing body", errors, n, nodes);
        }

        NodeKind::Block(i) => {
            check_present!(i.get_lbrace(), "Missing left brace", errors, n, nodes);
            check_present!(i.get_rbrace(), "Missing right brace", errors, n, nodes);
        }

        NodeKind::Parenthesised(i) => {
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(i.get_expr(), "Missing expression", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
        }

        NodeKind::Statement(i) => {
            check_present!(i.get_semicolon(), "Missing semicolon", errors, n, nodes);
        }

        NodeKind::LabelCall(i) => {
            check_present!(i.get_end(), "Missing label ++ or --", errors, n, nodes);
        }

        NodeKind::Assignment(i) => {
            check_present!(i.get_lvalue(), "Missing lvalue", errors, n, nodes);
            check_present!(i.get_rvalue(), "Missing rvalue", errors, n, nodes);
            check_present!(i.get_operator(), "Missing operator", errors, n, nodes);
        }

        NodeKind::Vector(i) => {
            check_present!(
                i.get_langle(),
                "Missing left angled bracket",
                errors,
                n,
                nodes
            );
            check_present!(
                i.get_rangle(),
                "Missing right angled bracket",
                errors,
                n,
                nodes
            );
        }

        NodeKind::Array(i) => {
            check_present!(
                i.get_lsquare(),
                "Missing left squared bracket",
                errors,
                n,
                nodes
            );
            check_present!(
                i.get_rsquare(),
                "Missing right squared bracket",
                errors,
                n,
                nodes
            );
        }

        NodeKind::UnOp(i) => {
            check_present!(i.get_operator(), "Missing operator", errors, n, nodes);
            check_present!(i.get_operand(), "Missing operand", errors, n, nodes);
        }

        NodeKind::BinaryOp(i) => {
            check_present!(i.get_operator(), "Missing operator", errors, n, nodes);
            check_present!(i.get_lhs(), "Missing lhs", errors, n, nodes);
            check_present!(i.get_rhs(), "Missing rhs", errors, n, nodes);
        }

        NodeKind::ArrayAccess(i) => {
            check_present!(i.get_lhs(), "Missing lhs", errors, n, nodes);
            check_present!(
                i.get_lsquare(),
                "Missing left squared bracket",
                errors,
                n,
                nodes
            );
            check_present!(
                i.get_rsquare(),
                "Missing right squared bracket",
                errors,
                n,
                nodes
            );
            check_present!(i.get_index(), "Missing index", errors, n, nodes);
        }

        NodeKind::FunctionCall(i) => {
            check_present!(i.get_lhs(), "Missing lhs", errors, n, nodes);
            check_present!(i.get_lparen(), "Missing left parenthesis", errors, n, nodes);
            check_present!(
                i.get_rparen(),
                "Missing right parenthesis",
                errors,
                n,
                nodes
            );
        }

        _ => {}
    }

    // Recursive calls to children
    for child_id in &children[n] {
        validate_rec(children, nodes, *child_id, errors);
    }
}

impl Tree {
    pub fn validate(&self) -> Vec<ValidationError> {
        let mut errors = vec![];
        validate_rec(&self.children, &self.nodes, 0, &mut errors);
        errors
    }
}
