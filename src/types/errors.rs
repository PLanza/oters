use crate::parser::ast::Expr;

use thiserror::Error;

impl Expr {
    pub fn head_string(&self) -> String {
        let string = format!("{:?}", self);
        string.split_whitespace().next().unwrap().to_string()
    }
}

#[derive(Error, Debug)]
pub enum InvalidExprError {
    #[error("Cannot have the expression `{0}` as top level.")]
    InvalidTopLevelExpr(String),
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type {0} has not been declared")]
    UserTypeNotFound(String),
    #[error("Type Arguments applied to non-generic argument")]
    ImproperTypeArguments,
    #[error("Generic Type Variable {0} has not been declared")]
    GenericVariableNotFound(String),
    #[error("Expexted a Stable type, got {0} instead")]
    ImproperUnstableType(String),
}
