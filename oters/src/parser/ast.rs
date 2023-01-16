pub type Program = Vec<Box<PExpr>>;

type GenericParams = Vec<(bool, String)>; // Each parameter is a pair of the parameter name and a boolean marking whether the parameter is stable or not

use std::collections::VecDeque;

// The parsed expressions of the grammar, need to translated into syntax::Exprs
#[derive(Debug, PartialEq, Clone)]
pub enum PExpr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    BinOp(Box<PExpr>, Opcode, Box<PExpr>),
    UnOp(Opcode, Box<PExpr>),
    List(VecDeque<Box<PExpr>>),
    StructExpr(String, Vec<(String, Box<PExpr>)>),
    Tuple(Vec<Box<PExpr>>),
    Fn(Vec<(String, bool)>, Box<PExpr>),
    If(Box<PExpr>, Box<PExpr>, Box<PExpr>),
    Block(Vec<Box<PExpr>>),
    App(Box<PExpr>, Box<PExpr>),
    ProjStruct(Box<PExpr>, String),
    Variant(String, Option<Box<PExpr>>),
    Match(Box<PExpr>, Vec<(Box<Pattern>, Box<PExpr>)>),
    Var(String),
    TypeDef(String, Vec<String>, Box<TypeExpr>), // Type Alias with Generic Parameters and defined type
    StructDef(String, Vec<String>, Vec<(String, Box<TypeExpr>)>), // Struct with generic parameters
    EnumDef(String, Vec<String>, Vec<(String, Option<Box<TypeExpr>>)>), // Enum with generic parameters
    Let(String, Box<PExpr>),
    LetAndWith(String, Box<PExpr>, String, Box<PExpr>, Box<PExpr>),
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
    // Unary operators
    Neg,
    Not,
    Delay,  // From Patrick Bahr's Rattus
    Stable, // From Patrick Bahr's Rattus
    Adv,    // From Patrick Bahr's Rattus
    Unbox,  // From Patrick Bahr's Rattus
    Out,    // From Patrick Bahr's Rattus
    Into,   // From Patrick Bahr's Rattus
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    TEUnit,
    TEInt,
    TEFloat,
    TEString,
    TEBool,
    TETuple(Vec<Box<TypeExpr>>),
    TEList(Box<TypeExpr>),
    TEUser(String, Vec<Box<TypeExpr>>), // Structs, Enums, Generics, and Type Aliases with their generic arguments
    TEFunction(Box<TypeExpr>, Box<TypeExpr>),
    TEDelay(Box<TypeExpr>),  // From Patrick Bahr's Rattus
    TEStable(Box<TypeExpr>), // From Patrick Bahr's Rattus
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Underscore,
    Bool(bool),
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
    Var(String),
}
