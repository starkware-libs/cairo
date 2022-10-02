//! Intermediate representation object after lowering from semantic.
//! This representation is SSA: each variable is defined before usage and assigned once.
//! It is also normal form: each function argument is a variable, not a complex expressions.

use id_arena::Id;

pub type BlockId = Id<Block>;
pub type VariableId = Id<Variable>;

/// A block of statements. Each block gets inputs and outputs, and is composed of
/// a linear sequence of statements.
/// A block may end with a `return`, which exits the current function.
/// A block is guaranteed to drop all living variables before the end.
pub struct Block {
    /// Input variables to the block, moved at the CallBlock call-site.
    pub inputs: Vec<VariableId>,
    /// Statements sequence running one after the other in the block, in a linear flow.
    pub statements: Vec<Statement>,
    /// Which variables are needed to be dropped at the end of this block. Note that these are
    /// not explicitly dropped by statements.
    pub drops: Vec<VariableId>,
    /// How this block ends, back to the call site, or exiting the entire function.
    pub end: BlockEnd,
}

/// Describes what happens to the program flow after a block ends.
pub enum BlockEnd {
    /// This block return to call-site, outputting variables to the call-site.
    Callsite(Vec<VariableId>),
    /// This block ends with a `return` statement, existing the function
    Return(Vec<VariableId>),
}

/// Information about a variable.
pub struct Variable {
    pub is_dup: bool,
    pub is_drop: bool,
    pub ty: semantic::TypeId,
}

/// Lowered statement.
pub enum Statement {
    // Values.
    // TODO(spapini): Consts.
    Literal(StatementLiteral),

    // Flow control.
    Call(StatementCall),
    CallBlock(StatementCallBlock),

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

/// A statement that binds a literal value to a variable.
pub struct StatementLiteral {
    // TODO(spapini): Fix the type of `value`.
    /// The value of the literal.
    pub value: usize,
    /// the variable to bind the value to.
    pub output: VariableId,
}

/// A statement that calls a user function.
pub struct StatementCall {
    // TODO(spapini): ConcreteFreeFunctionId once it exists.
    /// A user function to "call".
    pub function: semantic::FunctionId,
    /// Living variables in current scope to move to the function, as args.
    pub inputs: Vec<VariableId>,
    /// New variables to be introduced into the current scope from the function outputs.
    pub outputs: Vec<VariableId>,
}

/// A statement that jumps to another block. If that block ends with a BlockEnd::CallSite, the flow
/// returns to the statement following this one.
pub struct StatementCallBlock {
    /// A block to "call".
    pub block: BlockId,
    /// Living variables in current scope to move and be bound to the callee block inputs.
    pub inputs: Vec<VariableId>,
    /// New variables to be introduced into the current scope, moved from the callee block outputs.
    pub outputs: Vec<VariableId>,
}

/// A statement that calls an extern function with a single branch.
pub struct StatementCallExtern {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    /// A concrete external function to call.
    pub function: semantic::FunctionId,
    /// Living variables in current scope to move to the function, as args.
    pub inputs: Vec<VariableId>,
    /// New variables to be introduced into the current scope from the function outputs.
    pub outputs: Vec<VariableId>,
}

/// A statement that calls an extern function with branches, and "calls" a possibly different block
/// for each branch.
pub struct StatementMatchExtern {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    /// A concrete external function to call.
    pub function: semantic::FunctionId,
    /// Living variables in current scope to move to the function, as args.
    pub inputs: Vec<VariableId>,
    // All blocks should have the same rets.
    pub arms: Vec<MatchArm>,
}

pub struct MatchArm {
    /// New variables to be introduced into the current scope from the function branch outputs.
    /// These variables will move and be bound to the callee block inputs, in the same order.
    /// Note: to have something a bit more complex, like moving extra variables from a higher
    /// scope, or rearrange the variables, a intermediate block should be constructed with the
    /// proper calls.
    pub outputs: Vec<VariableId>,
    /// A block to "call".
    pub block: BlockId,
}
