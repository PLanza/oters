use thiserror::Error;

#[derive(Error, Debug)]
pub enum InterpretError {
    #[error("Uncaught type error in expression {0}")]
    UncaughtTypeError(String),
    #[error("Unbound location in expression {0}")]
    UnboundLocationError(String),
    #[error("Unbound variable {0}")]
    UnboundVariableError(String),
    #[error("Field {1} missing in expression {1}")]
    StructFieldMissingError(String, String),
}
