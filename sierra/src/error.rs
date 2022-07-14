use crate::{
    graph::{BlockId, Identifier, Type},
    mem_state::Location,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(Identifier),
    MissingReference(Identifier),
    VariableOverride(Identifier),
    IllegalApChangeValue,
    IllegalExtensionArgsLocation,
    LocalMemoryAlreadyAllocated,
    LocalMemoryCantBeAllocated,
    LocalMemoryNotAllocated,
    UsedTempMemoryInvalidated(Identifier),
    WrongNumberOfTypeArgs,
    LocationsNonCosecutive,
    UnsupportedTypeArg,
    UnsupportedTypeName(String),
    UnsupportedLibCallName(String),
    FunctionBlockOutOfBounds,
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionReturnTypeMismatch(BlockId, Identifier),
    FunctionReturnLocationMismatch(BlockId, Identifier),
    FunctionReturnLocationNotEndOfTemp(BlockId, i64, usize),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionBlockIdentifierLocationMismatch(BlockId, Identifier, Location, Location),
    ExtensionArgumentsMismatch(String),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
