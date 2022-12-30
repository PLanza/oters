use std::collections::VecDeque;

pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    List(VecDeque<Box<Value>>),
    Tuple(Vec<Box<Value>>),
    // Fn
    // Struct
    // Enum
}
