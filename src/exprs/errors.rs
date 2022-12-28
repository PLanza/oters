use super::Pattern;
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
    #[error("Adv expressions must always be inside Delay expressions")]
    ImproperAdvExpr,
    #[error("Let expressions must be top-level or inside blocks")]
    IllegalLetExpr,
    #[error("Locations can only be created by the interpreter")]
    IllegalLocation,
}

#[derive(Error, Debug)]
pub enum InvalidPatternError {
    #[error("The variable {0} is bound multiple times in pattern {1}")]
    SimultaneousPatternBinding(String, Pattern),
    #[error("List patterns must contain elements of the same type")]
    InvalidListPattern,
    #[error("Stream patterns can only be bound to variables")]
    InvalidStreamPattern,
}
