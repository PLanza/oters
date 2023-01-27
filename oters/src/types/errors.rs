use super::Type;
use crate::exprs::Expr;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type {0} has not been declared")]
    UserTypeNotFound(String),
    #[error("Type Arguments applied to non-generic argument")]
    ImproperTypeArguments,
    #[error("Fixed Point Variable {0} has not been declared")]
    FixedPointVariableNotFound(String),
    #[error("Generic Type Variable {0} has not been declared")]
    GenericVariableNotFound(String),
    #[error("Expexted a Stable type, got {0} instead")]
    ImproperUnstableType(Type),
    #[error("Expected {0}, got {1} instead")]
    ImproperType(Type, Type),
    #[error("The variable {0}, cannot be found in the current context")]
    UnboundVariableError(String),
    #[error("The variable {0}, cannot be accessed in the current context")]
    InvalidVariableAccess(String),
    #[error("The variable {0}, is not a struct")]
    NotAStruct(String),
    #[error("The struct {0} does not have a field {1}")]
    StructFieldDoesNotExist(String, String),
    #[error("The fields for struct {0} do not match")]
    StructFieldsDoNotMatch(String),
    #[error("Improper fields are applied to the enum variant {0}")]
    VariantFieldsDoNotMatch(String),
    #[error("The variable {0}, is not a enum")]
    NotAnEnum(String),
    #[error("Enums do not match")]
    EnumsDoNotMatch,
    #[error("Type {0} not stable")]
    ExpectedStableType(Type),
    #[error("Mutually recursive definition {0} must produce a Stream")]
    InvalidMutuallyRecursiveDefinition(Expr),
    #[error("Invalid path {0}")]
    InvalidPath(String),
}
