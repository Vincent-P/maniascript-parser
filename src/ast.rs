pub mod node_kind;
pub mod printer;

use generational_arena::{Index, Arena};
use crate::lexer::token::Token;
use node_kind::NodeKind;

pub type NodeId = Index;
pub type Nodes = Arena<Node>;

#[derive(Debug)]
pub struct Tree {
    nodes: Nodes
}

impl Tree {
    pub fn new() -> Tree {
        Tree {
            nodes: Arena::new()
        }
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        self.nodes.insert(node)
    }

    pub fn get_node(&self, node: NodeId) -> Option<&Node> {
        self.nodes.get(node)
    }

    pub fn get_mut_node(&mut self, node: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(node)
    }

    pub fn remove_node(&mut self, node: NodeId) -> Option<Node> {
        self.nodes.remove(node)
    }

    pub fn add_child_id(&mut self, parent_id: NodeId, child_id: NodeId) -> NodeId {
        if let Some(_) = self.nodes[child_id].parent {
            panic!("Cannot change the parent an already linked node.");
        }

        if self.nodes[parent_id].children.len() == 0 {
            self.nodes[parent_id].span = self.nodes[child_id].span;
        } else {
            // .1 is the second element of the span
            self.nodes[parent_id].span.1 = self.nodes[child_id].span.1;
        }

        self.nodes[parent_id].children.push(child_id);
        self.nodes[child_id].parent = Some(parent_id);

        child_id
    }

    pub fn add_child(&mut self, parent_id: NodeId, child: Node) -> NodeId {
        let tmp = self.add_node(child);
        self.add_child_id(parent_id, tmp)
    }
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub span: (usize, usize),

    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
}

impl Node {
    pub fn new(kind: NodeKind) -> Node {
        Node {
            kind,
            span: (0, 0),
            parent: None,
            children: vec![],
        }
    }
}

impl From<&Token> for Node {
    fn from(token: &Token) -> Node {
        Node {
            kind: NodeKind::Token(token.clone()),
            parent: None,
            span: token.span(),
            children: vec![]
        }
    }
}

impl From<Token> for Node {
    fn from(token: Token) -> Node {
        Node::from(&token)
    }
}
