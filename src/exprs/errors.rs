use crate::parser::ast::PExpr;

use thiserror::Error;

impl PExpr {
    pub fn head_string(&self) -> String {
        let string = format!("{:?}", self);
        string.split_whitespace().next().unwrap().to_string()
    }
}

#[derive(Error, Debug)]
pub enum InvalidExprError {
    #[error("Cannot have the expression {0} as top level.")]
    InvalidTopLevelExpr(String),
    #[error("Cannot use operator {0} as a binary operator.")]
    InvalidBinaryOperator(String),
    #[error("Adv expressions must always be inside Delay expressions")]
    ImproperAdvExpr,
    #[error("{0} is not allowed to be recursive")]
    IllegalRecursiveExpr(String),
    #[error("Let expressions must be top-level or inside blocks")]
    IllegalLetExpr,
}

#[derive(Error, Debug)]
pub enum InvalidPatternError{
    #[error("The variable {0} is bound multiple times in pattern {1}")]
    SimultaneousPatternBinding(String, String),
    #[error("List patterns must contain elements of the same type")]
    InvalidListPattern,
    #[error("Stream patterns can only be bound to variables")]
    InvalidStreamPattern,
}
