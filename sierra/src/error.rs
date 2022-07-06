use crate::{
    graph::{BlockId, Identifier, Type},
    scope_state::ScopeState,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(Identifier, Type, Type),
    MissingReference(Identifier, Type),
    VariableOverride(Identifier),
    ArgumentSizeMismatch,
    ResultSizeMismatch,
    WrongNumberOfTypeArgs,
    UnsupportedTypeArg,
    UnsupportedLibCallName(String),
    FunctionBlockOutOfBounds,
    FunctionRemainingOwnedObjects(ScopeState),
    FunctionBlockMismatch(BlockId, ScopeState, ScopeState),
    FunctionInvocationMismatch(String),
    FunctionJumpMismatch(String),
}
