use crate::{
    context::Context,
    edit_state::Error as EditError,
    extensions::Error as ExtError,
    graph::{ApChange, BlockId, Identifier, Type},
    ref_value::RefValue,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    UsedTempMemoryInvalidated(Identifier),
    Extension(ExtError, String),
    TypeInfo(ExtError, Type),
    EditState(BlockId, EditError),
    UnusedBlock(BlockId),
    FunctionBlockOutOfBounds,
    FunctionBlockContextMismatch(BlockId, Context, Context),
    FunctionBlockReturnTypesMismatch(BlockId, Vec<Type>, Vec<Type>),
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionReturnTypeMismatch(BlockId, Identifier),
    FunctionReturnApChangeMismatch(String, ApChange),
    FunctionReturnGasUsageMismatch(String, i64),
    FunctionReturnLocationMismatch(BlockId, Identifier),
    FunctionReturnLocationNotEndOfTemp(BlockId, i64, usize),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionBlockIdentifierLocationMismatch(BlockId, Identifier, RefValue, RefValue),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
