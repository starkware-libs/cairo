use crate::graph::{BlockId, Identifier, Type};

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(Identifier),
    MissingReference(Identifier),
    VariableOverride(Identifier),
    WrongNumberOfTypeArgs,
    UnsupportedTypeArg,
    UnsupportedTypeName(String),
    UnsupportedLibCallName(String),
    FunctionBlockOutOfBounds,
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionReturnTypeMismatch(BlockId),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    ExtensionArgumentsMismatch(String),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
