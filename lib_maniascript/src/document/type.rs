#[derive(Debug)]
pub enum Type {
    Simple(String),
    Array(String, Option<Box<Type>>),
    Function(Box<Type>, Vec<Type>),
    Dummy,
}
