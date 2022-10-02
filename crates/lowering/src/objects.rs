use id_arena::Id;

pub type BlockId = Id<Block>;
pub type VariableId = Id<Variable>;

// Blocks can take (move) variable from outer scopes.
// New variable introduced in this block, and variable from params, must be consumed before the
// block ends, or returned. i.e. Handles drops.
// Variables can be reused only if they handle dup.
pub struct Block {
    pub params: Vec<VariableId>,
    pub statements: Vec<Statement>,
    pub rets: Vec<VariableId>,
}

pub struct Variable {
    pub id: VariableId,
    pub ty: semantic::TypeId,
}

pub enum Statement {
    // Values. TODO(spapini): Consts.
    Literal(StatementLiteral),

    // Flow control.
    Call(StatementCall),
    CallBlock(StatementCallBlock),
    Return(StatementReturn),

    // Externs.
    CallExtern(StatementCallExtern),
    MatchExtern(StatementMatchExtern),

    // Structs.
    StructConstruct,
    StructDestruct,

    // Enums.
    EnumConstruct,
    MatchEnum,

    // Tuples.
    TupleConstruct,
    TupleDestruct,
}

pub struct StatementLiteral {
    // TODO(spapini): Fix the type of `value`.
    pub value: usize,
    pub bind: VariableId,
}

pub struct StatementCall {
    // TODO(spapini): ConcreteFreeFunctionId once it exists.
    pub function: semantic::FunctionId,
    pub args: Vec<VariableId>,
    pub binds: Vec<VariableId>,
}

pub struct StatementCallBlock {
    pub block: BlockId,
    pub args: Vec<VariableId>,
    pub binds: Vec<VariableId>,
}

pub struct StatementCallExtern {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    pub function: semantic::FunctionId,
    pub args: Vec<VariableId>,
    pub binds: Vec<VariableId>,
}

// All live variables must be consumed or returned before the ret.
pub struct StatementReturn {
    pub rets: Vec<VariableId>,
}

pub struct StatementMatchExtern {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    pub function: semantic::FunctionId,
    pub args: Vec<VariableId>,
    // All blocks should have the same rets.
    pub arms: Vec<MatchArm>,
}

pub struct MatchArm {
    pub binds: Vec<VariableId>,
    pub block: BlockId,
}
