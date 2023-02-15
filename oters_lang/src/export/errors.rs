use crate::exprs::Expr;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ValueError {
    #[error("Cannot convert the expression {0} to a Value")]
    InvalidConversion(Expr),
    #[error("Expected Tuple got {0} instead")]
    ImproperArguments(Expr),
}
