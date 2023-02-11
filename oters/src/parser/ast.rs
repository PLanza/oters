pub type Program = Vec<SpPExpr>;

type GenericParams = Vec<(bool, String)>; // Each parameter is a pair of the parameter name and a boolean marking whether the parameter is stable or not

use super::span::*;
use std::collections::VecDeque;

// The parsed expressions of the grammar, need to translated into syntax::Exprs
#[derive(Debug, PartialEq, Clone)]
pub enum PExpr {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    BinOp(SpPExpr, Opcode, SpPExpr),
    UnOp(Opcode, SpPExpr),
    List(VecDeque<SpPExpr>),
    StructExpr(Vec<String>, String, Vec<(String, SpPExpr)>),
    Tuple(Vec<SpPExpr>),
    Fn(Vec<SpPattern>, SpPExpr),
    If(SpPExpr, SpPExpr, SpPExpr),
    Block(Vec<SpPExpr>),
    App(SpPExpr, SpPExpr),
    ProjStruct(SpPExpr, String),
    Variant(Vec<String>, String, Option<SpPExpr>), // Variant's type, name, and contents
    Match(SpPExpr, Vec<(SpPattern, SpPExpr)>),
    Var(Vec<String>, String),
    TypeDef(String, Vec<String>, SpTypeExpr), // Type Alias with Generic Parameters and defined type
    StructDef(String, Vec<String>, Vec<(String, SpTypeExpr)>), // Struct with generic parameters
    EnumDef(String, Vec<String>, Vec<(String, Option<SpTypeExpr>)>), // Enum with generic parameters
    Let(SpPattern, SpPExpr),
    LetAndWith(SpPattern, SpPExpr, SpPattern, SpPExpr, SpPExpr),
    Use(Vec<String>, bool), // Bool indicates whether it's a value or type being imported
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
    TETuple(Vec<SpTypeExpr>),
    TEList(SpTypeExpr),
    TEUser(Vec<String>, String, Vec<SpTypeExpr>), // Structs, Enums, Generics, and Type Aliases with their generic arguments
    TEFunction(SpTypeExpr, SpTypeExpr),
    TEDelay(SpTypeExpr),  // From Patrick Bahr's Rattus
    TEStable(SpTypeExpr), // From Patrick Bahr's Rattus
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Underscore,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Unit,
    Tuple(Vec<SpPattern>),
    List(Vec<SpPattern>),
    Variant(Vec<String>, String, Option<SpPattern>),
    Struct(Vec<String>, String, Vec<(String, Option<SpPattern>)>),
    Cons(SpPattern, SpPattern),
    Stream(SpPattern, SpPattern),
    Or(SpPattern, SpPattern),
    Var(String, bool),
}
