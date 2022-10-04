//! Intermediate representation objects after lowering from semantic.
//! This representation is SSA (static single-assignment): each variable is defined before usage and
//! assigned once. It is also normal form: each function argument is a variable, rather than a
//! compound expression.

use id_arena::Id;

pub type BlockId = Id<Block>;
pub type VariableId = Id<Variable>;

/// A block of statements. Each block gets inputs and outputs, and is composed of
/// a linear sequence of statements.
///
/// A block may end with a `return`, which exits the current function.
///
/// A block contains the list of variables to be dropped at its end. Other than these variables and
/// the output variables, it is guaranteed that no other variable is alive.
pub struct Block {
    /// Input variables to the block.
    pub inputs: Vec<VariableId>,
    /// Statements sequence running one after the other in the block, in a linear flow.
    /// Note: Inner blocks might end with a `return`, which will exit the function in the middle.
    /// Note: Match is a possible statement, which means it has control flow logic inside, but
    /// after its execution is completed, the flow returns to the following statement of the block.
    pub statements: Vec<Statement>,
    /// Which variables are needed to be dropped at the end of this block. Note that these are
    /// not explicitly dropped by statements.
    pub drops: Vec<VariableId>,
    /// Describes how this block ends: returns to the caller or exits the function.
    pub end: BlockEnd,
}

/// Describes what happens to the program flow at the end of a block.
pub enum BlockEnd {
    /// This block returns to the call-site, outputting variables to the call-site.
    Callsite(Vec<VariableId>),
    /// This block ends with a `return` statement, exiting the function.
    Return(Vec<VariableId>),
    /// The last statement ended the flow (e.g., match will all arms ending in return),
    /// and the end of this block is unreachable.
    Unreachable,
}

/// Lowered variable representation.
pub struct Variable {
    /// Can the type be (trivially) dropped.
    pub droppable: bool,
    /// Can the type be (trivially) duplicated.
    pub duplicatable: bool,
    /// Semantic type of the variable.
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
    MatchExtern(StatementMatchExtern),

    // Structs.
    StructConstruct,
    StructDestruct,

    // Enums.
    EnumConstruct,
    MatchEnum,

    // Tuples.
    TupleConstruct,
    TupleDestruct(StatementTupleDestruct),
}
impl Statement {
    pub fn outputs(&self) -> Vec<VariableId> {
        match &self {
            Statement::Literal(stmt) => vec![stmt.output],
            Statement::Call(stmt) => stmt.outputs.clone(),
            Statement::CallBlock(stmt) => stmt.outputs.clone(),
            Statement::MatchExtern(stmt) => stmt.outputs.clone(),
            Statement::StructConstruct => todo!(),
            Statement::StructDestruct => todo!(),
            Statement::EnumConstruct => todo!(),
            Statement::MatchEnum => todo!(),
            Statement::TupleConstruct => todo!(),
            Statement::TupleDestruct(stmt) => stmt.outputs.clone(),
        }
    }
}

/// A statement that binds a literal value to a variable.
pub struct StatementLiteral {
    // TODO(spapini): Fix the type of `value`.
    /// The value of the literal.
    pub value: usize,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that calls a user function.
pub struct StatementCall {
    /// A function to "call".
    pub function: semantic::FunctionId,
    /// Living variables in current scope to move to the function, as arguments.
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

/// A statement that calls an extern function with branches, and "calls" a possibly different block
/// for each branch.
pub struct StatementMatchExtern {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    /// A concrete external function to call.
    pub function: semantic::FunctionId,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<VariableId>,
    // All blocks should have the same rets.
    pub arms: Vec<MatchArm>,
    /// New variables to be introduced into the current scope from the arm outputs.
    pub outputs: Vec<VariableId>,
}

pub struct MatchArm {
    /// New variables to be introduced into the current scope from the branch outputs.
    /// These variables will move and be bound to the callee block inputs, in the same order.
    /// Note: to have something a bit more complex, like moving extra variables from a higher
    /// scope, or rearrange the variables, an intermediate block should be constructed with the
    /// proper calls.
    pub arm_variables: Vec<VariableId>,
    /// A block to "call".
    pub block: BlockId,
}

/// A statement that destructs a tuple, introducing its elements as new variables.
pub struct StatementTupleDestruct {
    pub tys: Vec<semantic::TypeId>,
    pub input: VariableId,
    pub outputs: Vec<VariableId>,
}
