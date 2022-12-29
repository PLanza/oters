use thiserror::Error;

#[derive(Error, Debug)]
pub enum InterpretError {
    #[error("Uncaught type error in expression {0}")]
    UncaughtTypeError(String),
    #[error("Unbound location in expression {0}")]
    UnboundLocationError(String),
    #[error("Unbound variable {0}")]
    UnboundVariableError(String),
    #[error("Field {0} missing in expression {1}")]
    StructFieldMissingError(String, String),
    #[error("Pattern {0} doesn't match {1}")]
    PatternMatchError(String, String),
    #[error("No pattern matches expression {0}")]
    NoPatternMatchesError(String),
    #[error("Expression {0} cannot take a step forward")]
    ExpressionDoesNotStepError(String),
}
