use num_bigint::BigInt;

use crate::ids::{
    ConcreteLibFuncId, ConcreteTypeId, FunctionId, GenericLibFuncId, GenericTypeId, UserTypeId,
    VarId,
};

/// A full Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
    /// Declarations for all the used types.
    pub type_declarations: Vec<TypeDeclaration>,
    /// Declarations for all the used library functions.
    pub libfunc_declarations: Vec<LibFuncDeclaration>,
    /// The code of the program.
    pub statements: Vec<Statement>,
    /// Descriptions of the functions - signatures and entry points.
    pub funcs: Vec<Function>,
}
impl Program {
    pub fn get_statement(&self, id: &StatementIdx) -> Option<&Statement> {
        self.statements.get(id.0)
    }
}

/// Declaration of a concrete type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeDeclaration {
    /// The id of the declared concrete type.
    pub id: ConcreteTypeId,
    pub long_id: ConcreteTypeLongId,
}

/// A concrete type (the generic parent type and the generic arguments).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ConcreteTypeLongId {
    /// The id of the used generic type.
    pub generic_id: GenericTypeId,
    /// The arguments for the generic type.
    pub generic_args: Vec<GenericArg>,
}

/// Declaration of a concrete library function.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LibFuncDeclaration {
    /// The id of the declared concrete libfunc.
    pub id: ConcreteLibFuncId,
    pub long_id: ConcreteLibFuncLongId,
}

/// A concrete library function (the generic parent function and the generic arguments).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ConcreteLibFuncLongId {
    /// The id of the used generic libfunc.
    pub generic_id: GenericLibFuncId,
    /// The arguments for the specialization.
    pub generic_args: Vec<GenericArg>,
}

/// Represents the signature of a function.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionSignature {
    /// The types of the parameters of the function.
    pub param_types: Vec<ConcreteTypeId>,
    /// The return types.
    pub ret_types: Vec<ConcreteTypeId>,
}

/// Represents a function (its name, signature and entry point).
#[derive(Clone, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Param {
    pub id: VarId,
    pub ty: ConcreteTypeId,
}

/// Represents the index of a Sierra statement in the Program::statements vector.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum GenericArg {
    UserType(UserTypeId),
    Type(ConcreteTypeId),
    Value(BigInt),
    UserFunc(FunctionId),
    LibFunc(ConcreteLibFuncId),
}

/// A possible statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GenStatement<StatementId> {
    Invocation(GenInvocation<StatementId>),
    Return(Vec<VarId>),
}

/// An invocation statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GenInvocation<StatementId> {
    /// The called libfunc.
    pub libfunc_id: ConcreteLibFuncId,
    /// The arguments consumed by the libfunc's invocation.
    pub args: Vec<VarId>,
    /// The possible branches to continue to after the invocation.
    /// The program would continue to exactly one of the branches.
    pub branches: Vec<GenBranchInfo<StatementId>>,
}

/// Describes the flow of a chosen libfunc's branch.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GenBranchInfo<StatementId> {
    /// The target the branch continues the run through.
    pub target: GenBranchTarget<StatementId>,
    /// The resulting identifiers from the libfunc call.
    pub results: Vec<VarId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GenBranchTarget<StatementId> {
    /// Continues a run to the next statement.
    Fallthrough,
    /// Continues the run to provided statement.
    Statement(StatementId),
}

pub type Function = GenFunction<StatementIdx>;
pub type Statement = GenStatement<StatementIdx>;
pub type Invocation = GenInvocation<StatementIdx>;
pub type BranchInfo = GenBranchInfo<StatementIdx>;
pub type BranchTarget = GenBranchTarget<StatementIdx>;
