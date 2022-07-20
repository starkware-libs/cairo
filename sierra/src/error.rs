use crate::{
    edit_state::Error as EditError,
    extensions::Error as ExtError,
    graph::{BlockId, Identifier, Type},
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
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionReturnTypeMismatch(BlockId, Identifier),
    FunctionReturnApChangeMismatch(BlockId, usize),
    FunctionReturnLocationMismatch(BlockId, Identifier),
    FunctionReturnLocationNotEndOfTemp(BlockId, i64, usize),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionBlockIdentifierLocationMismatch(BlockId, Identifier, RefValue, RefValue),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
