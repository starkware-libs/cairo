use crate::{
    context::{Context, Effects, Error as CtxtError},
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
    MergeEffects(BlockId, CtxtError),
    UnusedBlock(BlockId),
    FunctionBlockOutOfBounds,
    FunctionBlockContextMismatch(BlockId, Context, Context),
    FunctionBlockEffectsMismatch(BlockId, Effects, Effects),
    FunctionBlockReturnTypesMismatch(BlockId, Vec<Type>, Vec<Type>),
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionRanOutOfResources(Vec<(Identifier, i64)>),
    FunctionReturnTypeMismatch(BlockId, Identifier),
    FunctionReturnApChangeMismatch(String, Option<usize>),
    FunctionReturnResourceUsageMismatch(String, Identifier, i64),
    FunctionReturnLocationMismatch(BlockId, Identifier),
    FunctionReturnLocationNotEndOfTemp(BlockId, i64, usize),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionBlockIdentifierLocationMismatch(BlockId, Identifier, RefValue, RefValue),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
