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
}
