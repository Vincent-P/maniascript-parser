use crate::ast::node_kind::*;
use crate::ast::*;

pub struct ValidationError {
    pub span: (usize, usize),
    pub msg: &'static str,
}

macro_rules! not_present {
    ($msg:expr, $errors:expr, $n: expr, $nodes:expr) => {
        {
            $errors.push(ValidationError {
                span: $nodes[$n].span,
                msg: $msg
            });
        }
    };
}

macro_rules! check_present {
    ($e:expr, $msg:expr, $errors:expr, $n: expr, $nodes:expr) => {
        {
            if let None = $e {
                not_present!($msg, $errors, $n, $nodes);
            }
        }
    };
}

fn validate_rec(children: &[Vec<NodeId>], nodes: &Vec<Node>, n: usize, errors: &mut Vec<ValidationError>) {
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

        NodeKind::VarDec(i) => {
            check_present!(i.get_name(), "Missing name", errors, n, nodes);
        }

        NodeKind::Statement(i) => {
            check_present!(i.get_semicolon(), "Missing semicolon", errors, n, nodes);
        }

        // Recursive calls to children
        _ => {
            for child_id in &children[n] {
                validate_rec(children, nodes, *child_id, errors);
            }
        }
    }
}

impl Tree {
    pub fn validate(&self) -> Vec<ValidationError> {
        let mut errors = vec![];
        validate_rec(&self.children, &self.nodes, 0, &mut errors);
        errors
    }
}
