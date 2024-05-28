use std::fmt;

use anyhow::Result;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::debug_info::DebugInfo;
use crate::extensions::gas::{
    BuiltinCostWithdrawGasLibfunc, RedepositGasLibfunc, WithdrawGasLibfunc,
};
use crate::extensions::NamedLibfunc;
use crate::ids::{
    ConcreteLibfuncId, ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId, UserTypeId,
    VarId,
};

/// Version-tagged representation of Sierra program.
///
/// Always prefer using this struct as saved artifacts instead of inner ones.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum VersionedProgram {
    V1 {
        version: Version<1>,
        #[serde(flatten)]
        program: ProgramArtifact,
    },
}

impl VersionedProgram {
    pub fn v1(program: ProgramArtifact) -> Self {
        Self::V1 { program, version: Version::<1> }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Version<const V: u8>;

#[derive(Debug, Error)]
#[error("Unsupported Sierra program version")]
struct VersionError;

impl<const V: u8> Serialize for Version<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_u8(V)
    }
}

impl<'de, const V: u8> Deserialize<'de> for Version<V> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = u8::deserialize(deserializer)?;
        if value == V { Ok(Version::<V>) } else { Err(serde::de::Error::custom(VersionError)) }
    }
}

impl From<ProgramArtifact> for VersionedProgram {
    fn from(value: ProgramArtifact) -> Self {
        VersionedProgram::v1(value)
    }
}

impl VersionedProgram {
    pub fn into_v1(self) -> Result<ProgramArtifact> {
        match self {
            VersionedProgram::V1 { program, .. } => Ok(program),
        }
    }
}

impl fmt::Display for VersionedProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VersionedProgram::V1 { program, .. } => fmt::Display::fmt(program, f),
        }
    }
}

/// Sierra program in a form for storage on the filesystem and sharing externally.
///
/// Do not serialize this struct directly, use [`VersionedProgram`] instead.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ProgramArtifact {
    /// Sierra program itself.
    #[serde(flatten)]
    pub program: Program,
    /// Debug information for a Sierra program.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub debug_info: Option<DebugInfo>,
}

impl ProgramArtifact {
    /// Create a new [`ProgramArtifact`] without any extra information.
    pub fn stripped(program: Program) -> Self {
        Self { program, debug_info: None }
    }

    /// Add [`DebugInfo`] to the [`ProgramArtifact`], replacing existing one.
    pub fn with_debug_info(self, debug_info: DebugInfo) -> Self {
        Self { program: self.program, debug_info: Some(debug_info) }
    }
}

impl fmt::Display for ProgramArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.program, f)
    }
}

/// A full Sierra program.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Program {
    /// Declarations for all the used types.
    pub type_declarations: Vec<TypeDeclaration>,
    /// Declarations for all the used library functions.
    pub libfunc_declarations: Vec<LibfuncDeclaration>,
    /// The code of the program.
    pub statements: Vec<Statement>,
    /// Descriptions of the functions - signatures and entry points.
    pub funcs: Vec<Function>,
}
impl Program {
    pub fn get_statement(&self, id: &StatementIdx) -> Option<&Statement> {
        self.statements.get(id.0)
    }

    /// Create a new [`ProgramArtifact`] out of this [`Program`].
    pub fn into_artifact(self) -> VersionedProgram {
        ProgramArtifact::stripped(self).into()
    }
}

/// Declaration of a concrete type.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TypeDeclaration {
    /// The id of the declared concrete type.
    pub id: ConcreteTypeId,
    pub long_id: ConcreteTypeLongId,
    pub declared_type_info: Option<DeclaredTypeInfo>,
}

/// Declaration of a concrete type info.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct DeclaredTypeInfo {
    /// Can the type be stored by any of the store commands.
    pub storable: bool,
    /// Can the type be (trivially) dropped.
    pub droppable: bool,
    /// Can the type be (trivially) duplicated.
    pub duplicatable: bool,
    /// The size of an element of this type.
    pub zero_sized: bool,
}

/// A concrete type (the generic parent type and the generic arguments).
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ConcreteTypeLongId {
    /// The id of the used generic type.
    pub generic_id: GenericTypeId,
    /// The arguments for the generic type.
    pub generic_args: Vec<GenericArg>,
}

/// Declaration of a concrete library function.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct LibfuncDeclaration {
    /// The id of the declared concrete libfunc.
    pub id: ConcreteLibfuncId,
    pub long_id: ConcreteLibfuncLongId,
}

/// A concrete library function (the generic parent function and the generic arguments).
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ConcreteLibfuncLongId {
    /// The id of the used generic libfunc.
    pub generic_id: GenericLibfuncId,
    /// The arguments for the specialization.
    pub generic_args: Vec<GenericArg>,
}

/// Represents the signature of a function.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct FunctionSignature {
    /// The types of the parameters of the function.
    pub param_types: Vec<ConcreteTypeId>,
    /// The return types.
    pub ret_types: Vec<ConcreteTypeId>,
}

/// Represents a function (its name, signature and entry point).
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct GenFunction<StatementId> {
    /// The name of the function.
    pub id: FunctionId,
    /// The parameter types and return types.
    pub signature: FunctionSignature,
    /// The parameters of the function.
    // TODO(lior): Consider keeping here only the var ids, instead of the full Param (the types
    //   are stored in `signature`).
    pub params: Vec<Param>,
    /// The statement id where the function starts.
    pub entry_point: StatementId,
}

impl<StatementId> GenFunction<StatementId> {
    pub fn new(
        id: FunctionId,
        params: Vec<Param>,
        ret_types: Vec<ConcreteTypeId>,
        entry_point: StatementId,
    ) -> Self {
        let param_types: Vec<_> = params.iter().map(|Param { id: _, ty }| ty.clone()).collect();
        GenFunction {
            id,
            signature: FunctionSignature { param_types, ret_types },
            params,
            entry_point,
        }
    }
}

/// Descriptor of a variable.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Param {
    pub id: VarId,
    pub ty: ConcreteTypeId,
}

/// Represents the index of a Sierra statement in the Program::statements vector.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize, PartialOrd, Ord)]
pub struct StatementIdx(pub usize);
impl StatementIdx {
    pub fn next(&self, target: &BranchTarget) -> StatementIdx {
        match target {
            BranchTarget::Fallthrough => StatementIdx(self.0 + 1),
            BranchTarget::Statement(id) => *id,
        }
    }
}

/// Possible arguments for generic type.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum GenericArg {
    UserType(UserTypeId),
    Type(ConcreteTypeId),
    Value(BigInt),
    UserFunc(FunctionId),
    Libfunc(ConcreteLibfuncId),
}

/// A possible statement.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum GenStatement<StatementId> {
    Invocation(GenInvocation<StatementId>),
    Return(Vec<VarId>),
}
impl<StatementId> GenStatement<StatementId> {
    pub fn map<T>(self, f: impl Fn(StatementId) -> T) -> GenStatement<T> {
        match self {
            GenStatement::Invocation(invocation) => GenStatement::Invocation(GenInvocation {
                libfunc_id: invocation.libfunc_id,
                args: invocation.args.clone(),
                branches: invocation
                    .branches
                    .into_iter()
                    .map(|branch| GenBranchInfo {
                        target: branch.target.map(&f),
                        results: branch.results.clone(),
                    })
                    .collect(),
            }),
            GenStatement::Return(results) => GenStatement::Return(results.clone()),
        }
    }
}

/// An invocation statement.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct GenInvocation<StatementId> {
    /// The called libfunc.
    pub libfunc_id: ConcreteLibfuncId,
    /// The arguments consumed by the libfunc's invocation.
    pub args: Vec<VarId>,
    /// The possible branches to continue to after the invocation.
    /// The program would continue to exactly one of the branches.
    pub branches: Vec<GenBranchInfo<StatementId>>,
}

/// Describes the flow of a chosen libfunc's branch.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct GenBranchInfo<StatementId> {
    /// The target the branch continues the run through.
    pub target: GenBranchTarget<StatementId>,
    /// The resulting identifiers from the libfunc call.
    pub results: Vec<VarId>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum GenBranchTarget<StatementId> {
    /// Continues a run to the next statement.
    Fallthrough,
    /// Continues the run to provided statement.
    Statement(StatementId),
}
impl<StatementId> GenBranchTarget<StatementId> {
    pub fn map<T>(self, f: impl Fn(StatementId) -> T) -> GenBranchTarget<T> {
        match self {
            GenBranchTarget::Fallthrough => GenBranchTarget::Fallthrough,
            GenBranchTarget::Statement(id) => GenBranchTarget::Statement(f(id)),
        }
    }
}

pub type Function = GenFunction<StatementIdx>;
pub type Statement = GenStatement<StatementIdx>;
pub type Invocation = GenInvocation<StatementIdx>;
pub type BranchInfo = GenBranchInfo<StatementIdx>;
pub type BranchTarget = GenBranchTarget<StatementIdx>;

impl Program {
    /// Checks if this Sierra program needs a gas counter set up in order to be executed.
    ///
    /// This is determined by checking if the program uses any of gas-related libfuncs.
    pub fn requires_gas_counter(&self) -> bool {
        self.libfunc_declarations.iter().any(|decl| {
            matches!(
                decl.long_id.generic_id.0.as_str(),
                WithdrawGasLibfunc::STR_ID
                    | BuiltinCostWithdrawGasLibfunc::STR_ID
                    | RedepositGasLibfunc::STR_ID
            )
        })
    }
}
