type GenericParams = Vec<(bool, String)>; // Each parameter is a pair of the parameter name and a boolean marking whether the parameter is stable or not

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
    Fn(Vec<(String, Box<Type>)>, Box<Expr>),
    Fix(String, Box<Expr>),
    Let(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
    DeTuple(Box<Expr>, i64),
    DeStruct(Box<Expr>, String),
    Variant(String, Option<Box<Expr>>),
    Var(String),
    Type(String, GenericParams, Box<Type>), // Type Alias with Generic Parameters and defined type
    Struct(String, GenericParams, Vec<(String, Box<Type>)>), // Struct with generic parameters
    Enum(String, GenericParams, Vec<(String, Option<Box<Type>>)>) // Enum with generic parameters
}

#[derive(Debug, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    Cons,
    Stream,
    Eq,
    Lt,
    Gt,
    And,
    Or,
    Neg,
    Not,
    Delay,
    Box,
    Adv,
    Unbox,
    Out,
    Into,
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
