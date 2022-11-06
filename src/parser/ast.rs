type Program = Vec<Box<Expr>>;

type GenericParams = Vec<(bool, String)>; // Each parameter is a pair of the parameter name and a boolean marking whether the parameter is stable or not

#[derive(Debug, PartialEq, Clone)]
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
    StructExpr(String, Vec<(String, Box<Expr>)>),
    Tuple(Vec<Box<Expr>>),
    Fn(Vec<(String, Box<Type>)>, Box<Expr>),
    Fix(String, Box<Expr>), // From Patrick Bahr's Rattus 
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Box<Expr>>),
    App(Box<Expr>, Box<Expr>),
    ProjTuple(Box<Expr>, i64),
    ProjStruct(Box<Expr>, String),
    Variant(String, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<(Box<Pattern>, Box<Expr>)>),
    Var(String),
    TypeDef(String, GenericParams, Box<Type>), // Type Alias with Generic Parameters and defined type
    StructDef(String, GenericParams, Vec<(String, Box<Type>)>), // Struct with generic parameters
    EnumDef(String, GenericParams, Vec<(String, Option<Box<Type>>)>), // Enum with generic parameters
    Let(String, GenericParams, Box<Expr>), // Let generic parameters for binding with functions
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Delay, // From Patrick Bahr's Rattus 
    Stable, // From Patrick Bahr's Rattus 
    Adv, // From Patrick Bahr's Rattus 
    Unbox, // From Patrick Bahr's Rattus 
    Out, // From Patrick Bahr's Rattus 
    Into, // From Patrick Bahr's Rattus 
}

#[derive(Debug, PartialEq, Clone)]
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
    Delay(Box<Type>), // From Patrick Bahr's Rattus 
    Stable(Box<Type>), // From Patrick Bahr's Rattus 
    Fix(String, Box<Type>), // Fixed point argument and type expression as defined in Rattus
    Var(String),            // A Fix type's variable
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Underscore,
    True,
    False,
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    Tuple(Vec<Box<Pattern>>),
    List(Vec<Box<Pattern>>),
    Variant(String, Option<Box<Pattern>>),
    Struct(String, Vec<(String, Option<Box<Pattern>>)>),
    Cons(Box<Pattern>, Box<Pattern>),
    Stream(Box<Pattern>, Box<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
    Delay(Box<Pattern>), // From Patrick Bahr's Rattus 
    Stable(Box<Pattern>), // From Patrick Bahr's Rattus 
    Var(String),
}
