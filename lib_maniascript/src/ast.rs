pub mod node_kind;
pub mod printer;

use crate::lexer::token::*;
pub use node_kind::*;

#[derive(Debug, Default)]
pub struct Tree {
    pub parents: Vec<NodeId>,
    pub children: Vec<Vec<NodeId>>,
    pub nodes: Vec<Node>,
    current_parent: usize,
    building_nodes: Vec<usize>,
}

impl Tree {
    pub fn new() -> Tree {
        Tree {
            parents: vec![],
            children: vec![],
            nodes: vec![],
            current_parent: 0,
            // a stack of the nodes that are not finished
            building_nodes: vec![],
        }
    }

    fn set_parent(&mut self, p: usize) {
        self.building_nodes.push(self.current_parent);
        self.current_parent = p;
    }

    fn revert_parent(&mut self) {
        self.current_parent = match self.building_nodes.pop() {
            Some(old_parent) => old_parent,
            None => 0,
        };
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        let node_id = self.nodes.len();

        self.nodes.push(node);
        self.children.push(vec![]);
        self.parents.push(0);

        if self.current_parent != node_id {
            self.link_nodes(self.current_parent, node_id);
        }

        node_id
    }

    pub fn new_node(&mut self) -> NodeId {
        self.add_node(Default::default())
    }

    pub fn start_node(&mut self) -> NodeId {
        let node_id = self.new_node();
        self.set_parent(node_id);
        node_id
}

pub fn end_node(&mut self, node_kind: NodeKind) -> NodeId {
    let node_id = self.current_parent;
    self.nodes[node_id].kind = node_kind;
    self.revert_parent();
    node_id
}

pub fn get_node(&self, node: NodeId) -> Option<&Node> {
    self.nodes.get(node)
}

    fn update_span(&mut self, node: NodeId) {
        let parent = self.parents[node];
        let children = &self.children[node];

        let mut span = (0, 0);

        for child in children {
            let cspan = self.nodes[*child].span;

            if span.0 == 0 && span.1 == 0 {
                span = cspan;
            } else {
                span.1 = cspan.1;
            }
        }

        self.nodes[node].span = span;

        if parent != node {
            self.update_span(parent);
        }
    }

    // Link two nodes
    pub fn link_nodes(&mut self, parent_id: NodeId, child_id: NodeId) -> NodeId {
        if parent_id == child_id {
            panic!("Cannot link a node to itself");
        }

        let old_parent = self.parents[child_id];

        if self.children[old_parent].contains(&child_id) {
            self.children[old_parent].retain(|&x| x != child_id);
        }

        let child_to_parent = self.parents[child_id] == parent_id;
        let parent_to_child = self.children[parent_id].contains(&child_id);

        match (child_to_parent, parent_to_child) {
            (false, true) => {
                self.parents[child_id] = parent_id;
            }

            (true, false) => {
                self.children[parent_id].push(child_id);
            }

            (false, false) => {
                self.parents[child_id] = parent_id;
                self.children[parent_id].push(child_id);
            }

            _ => {
                panic!(
                    "You tried to link the sames nodes two times in a row, {:?} and {:?}.",
                    self.nodes[parent_id], self.nodes[child_id]
                );
            }
        }

        if self.children[parent_id].is_empty() {
            self.nodes[parent_id].span = self.nodes[child_id].span;
        } else {
            // .1 is the second element of the span
            self.nodes[parent_id].span.1 = self.nodes[child_id].span.1;
        }

        self.update_span(parent_id);

        child_id
    }
}

impl From<Token> for Node {
    fn from(token: Token) -> Node {
        Node {
            span: token.span(),
            kind: NodeKind::Token(token),
        }
    }
}

impl From<&Token> for Node {
    fn from(token: &Token) -> Node {
        Node::from(token.clone())
    }
}
