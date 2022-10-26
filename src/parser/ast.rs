#[derive(Debug, PartialEq)]
pub enum Expr {
    True,
    False,
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    BinOp(Box<Expr>, Opcode, Box<Expr>),
    UnOp(Opcode, Box<Expr>),
    List(Vec<Box<Expr>>),
    StructVal(String, Vec<(String, Box<Expr>)>),
    Tuple(Vec<Box<Expr>>),
    Let(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
    DeTuple(Box<Expr>, i64),
    DeStruct(Box<Expr>, String),
    Variant(String, Option<Box<Expr>>),
    Var(String),
    Type(String, Vec<(bool, String)>, Box<Type>), // Type Alias with Generic Parameters (bool corresponds to a stable tag) and defined type
}

#[derive(Debug, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    Neg,
    Not,
    Delay,
    Box,
    Adv,
    Unbox,
    Mod,
    Cons,
    Stream,
    Eq,
    Lt,
    Gt,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Float,
    String,
    Bool,
    Tuple(Vec<Box<Type>>),
    List(Box<Type>),
    User(String, Vec<Box<Type>>), // Structs, Enums, Generics, and Type Aliases with their generic arguments
    Function(Box<Type>, Box<Type>),
    Delay(Box<Type>),
    Box(Box<Type>),
    Fix(String, Box<Type>), // Fixed point argument and type expression
    Var(String),            // A Fix type's variable
}
