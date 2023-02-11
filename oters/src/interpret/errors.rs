use thiserror::Error;

use crate::{exprs::Expr, parser::ast::Pattern};

#[derive(Error, Debug)]
pub enum InterpretError {
    #[error("Uncaught type error in expression {0}")]
    UncaughtTypeError(Expr),
    #[error("Unbound location in expression {0}")]
    UnboundLocationError(Expr),
    #[error("Unbound variable {0}")]
    UnboundVariableError(String),
    #[error("Field {0} missing in expression {1}")]
    StructFieldMissingError(String, Expr),
    #[error("Pattern {0} doesn't match {1}")]
    PatternMatchError(Pattern, Expr),
    #[error("No pattern matches expression {0}")]
    NoPatternMatchesError(Expr),
    #[error("Expression {0} cannot take a step forward")]
    ExpressionDoesNotStepError(Expr),
}
