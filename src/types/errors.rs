use thiserror::Error;

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
    #[error("Expected {0}, got {1} instead")]
    ImproperType(String, String),
}
