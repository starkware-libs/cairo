use crate::{
    extensions,
    graph::{BlockId, Identifier, Type},
    ref_value::RefValue,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    MissingReference(Identifier),
    VariableOverride(Identifier),
    UsedTempMemoryInvalidated(Identifier),
    Extension(extensions::Error, String),
    TypeInfo(extensions::Error, Type),
    FunctionBlockOutOfBounds,
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionReturnTypeMismatch(BlockId, Identifier),
    FunctionReturnLocationMismatch(BlockId, Identifier),
    FunctionReturnLocationNotEndOfTemp(BlockId, i64, usize),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionBlockIdentifierLocationMismatch(BlockId, Identifier, RefValue, RefValue),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
