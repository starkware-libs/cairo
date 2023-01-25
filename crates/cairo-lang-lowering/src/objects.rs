//! Intermediate representation objects after lowering from semantic.
//! This representation is SSA (static single-assignment): each variable is defined before usage and
//! assigned once. It is also normal form: each function argument is a variable, rather than a
//! compound expression.

use std::ops::{Deref, DerefMut};

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::{ConcreteEnumId, ConcreteVariant};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::{Arena, Id};
use itertools::chain;
use num_bigint::BigInt;
pub mod blocks;
pub use blocks::BlockId;

use self::blocks::{FlatBlocks, StructuredBlocks};
use crate::diagnostic::LoweringDiagnostic;

pub type VariableId = Id<Variable>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct RefIndex(pub usize);

/// A lowered function code.
#[derive(Debug, PartialEq, Eq)]
pub struct StructuredLowered {
    /// Diagnostics produced while lowering.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    /// Block id for the start of the lowered function.
    pub root: Maybe<BlockId>,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: StructuredBlocks,
}

/// A lowered function code using flat blocks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FlatLowered {
    /// Diagnostics produced while lowering.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    /// Block id for the start of the lowered function.
    pub root: Maybe<BlockId>,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: FlatBlocks,
}

/// A block of statements. Each block gets inputs and outputs, and is composed of
/// a linear sequence of statements.
///
/// A block may end with a `return`, which exits the current function.
///
/// A block contains the list of variables to be dropped at its end. Other than these variables and
/// the output variables, it is guaranteed that no other variable is alive.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructuredBlock {
    /// The variable ids bound to the ref variables (including implicits) at the beginning of the
    /// block.
    pub initial_refs: Vec<VariableId>,
    /// Input variables to the block, including implicits.
    pub inputs: Vec<VariableId>,
    /// Statements sequence running one after the other in the block, in a linear flow.
    /// Note: Inner blocks might end with a `return`, which will exit the function in the middle.
    /// Note: Match is a possible statement, which means it has control flow logic inside, but
    /// after its execution is completed, the flow returns to the following statement of the block.
    pub statements: Vec<StructuredStatement>,
    /// Describes how this block ends: returns to the caller or exits the function.
    pub end: StructuredBlockEnd,
}

/// Remapping of lowered variable ids. Useful for convergence of branches.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct VarRemapping {
    /// Map from new_var to old_var (since new_var cannot appear twice, but old_var can).
    pub remapping: OrderedHashMap<VariableId, VariableId>,
}
impl Deref for VarRemapping {
    type Target = OrderedHashMap<VariableId, VariableId>;

    fn deref(&self) -> &Self::Target {
        &self.remapping
    }
}
impl DerefMut for VarRemapping {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.remapping
    }
}

/// Describes what happens to the program flow at the end of a [`StructuredBlock`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StructuredBlockEnd {
    /// This block returns to the call-site, remapping variables to the call-site.
    Callsite(VarRemapping),
    /// This block ends with a `return` statement, exiting the function.
    Return { refs: Vec<VariableId>, returns: Vec<VariableId> },
    /// This block ends with a `panic` statement, exiting the function.
    Panic { refs: Vec<VariableId>, data: VariableId },
    /// The last statement ended the flow (e.g., match will all arms ending in return),
    /// and the end of this block is unreachable.
    Unreachable,
}

/// A block of statements. Unlike [`StructuredBlock`], this has no reference information,
/// and no panic ending.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FlatBlock {
    /// Input variables to the block, including implicits.
    pub inputs: Vec<VariableId>,
    /// Statements sequence running one after the other in the block, in a linear flow.
    /// Note: Inner blocks might end with a `return`, which will exit the function in the middle.
    /// Note: Match is a possible statement, which means it has control flow logic inside, but
    /// after its execution is completed, the flow returns to the following statement of the block.
    pub statements: Vec<Statement>,
    /// Describes how this block ends: returns to the caller or exits the function.
    pub end: FlatBlockEnd,
}

/// Describes what happens to the program flow at the end of a [`FlatBlock`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatBlockEnd {
    /// This block returns to the call-site, outputting variables to the call-site.
    Callsite(VarRemapping),
    /// This block ends with a `return` statement, exiting the function.
    Return(Vec<VariableId>),
    /// The last statement ended the flow (e.g., match will all arms ending in return),
    /// and the end of this block is unreachable.
    Unreachable,
}

impl TryFrom<StructuredBlock> for FlatBlock {
    type Error = ();

    fn try_from(value: StructuredBlock) -> Result<Self, Self::Error> {
        Ok(FlatBlock {
            inputs: value.inputs,
            statements: value.statements.into_iter().map(|s| s.statement).collect(),
            end: value.end.try_into()?,
        })
    }
}

impl TryFrom<StructuredBlockEnd> for FlatBlockEnd {
    type Error = ();

    fn try_from(value: StructuredBlockEnd) -> Result<Self, Self::Error> {
        Ok(match value {
            StructuredBlockEnd::Callsite(vars) => FlatBlockEnd::Callsite(vars),
            StructuredBlockEnd::Return { refs, returns } => {
                FlatBlockEnd::Return(chain!(refs.iter(), returns.iter()).copied().collect())
            }
            StructuredBlockEnd::Panic { .. } => return Err(()),
            StructuredBlockEnd::Unreachable => FlatBlockEnd::Unreachable,
        })
    }
}

/// Lowered variable representation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    /// Can the type be (trivially) dropped.
    pub droppable: bool,
    /// Can the type be (trivially) duplicated.
    pub duplicatable: bool,
    /// Semantic type of the variable.
    pub ty: semantic::TypeId,
    /// Location of the variable.
    pub location: StableLocation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructuredStatement {
    pub statement: Statement,
    /// Updates to the variable ids bound to the ref variables (including implicits), from the last
    /// update until exactly after this statement.
    pub ref_updates: OrderedHashMap<RefIndex, VariableId>,
}

impl From<Statement> for StructuredStatement {
    fn from(statement: Statement) -> Self {
        StructuredStatement { statement, ref_updates: Default::default() }
    }
}

/// Lowered statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    // Values.
    // TODO(spapini): Consts.
    Literal(StatementLiteral),

    // Flow control.
    Call(StatementCall),
    CallBlock(StatementCallBlock),
    MatchExtern(StatementMatchExtern),

    // Structs (including tuples).
    StructConstruct(StatementStructConstruct),
    StructDestructure(StatementStructDestructure),

    // Enums.
    EnumConstruct(StatementEnumConstruct),
    MatchEnum(StatementMatchEnum),
}
impl Statement {
    pub fn inputs(&self) -> Vec<VariableId> {
        match &self {
            Statement::Literal(_stmt) => vec![],
            Statement::Call(stmt) => stmt.inputs.clone(),
            Statement::CallBlock(_) => vec![],
            Statement::MatchExtern(stmt) => stmt.inputs.clone(),
            Statement::StructConstruct(stmt) => stmt.inputs.clone(),
            Statement::StructDestructure(stmt) => vec![stmt.input],
            Statement::EnumConstruct(stmt) => vec![stmt.input],
            Statement::MatchEnum(stmt) => vec![stmt.input],
        }
    }
    pub fn outputs(&self) -> Vec<VariableId> {
        match &self {
            Statement::Literal(stmt) => vec![stmt.output],
            Statement::Call(stmt) => stmt.outputs.clone(),
            Statement::CallBlock(_) => vec![],
            Statement::MatchExtern(_) => vec![],
            Statement::StructConstruct(stmt) => vec![stmt.output],
            Statement::StructDestructure(stmt) => stmt.outputs.clone(),
            Statement::EnumConstruct(stmt) => vec![stmt.output],
            Statement::MatchEnum(_) => vec![],
        }
    }
}

/// A statement that binds a literal value to a variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementLiteral {
    /// The value of the literal.
    pub value: BigInt,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that calls a user function.
#[derive(Clone, Debug, PartialEq, Eq)]
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementCallBlock {
    /// A block to "call".
    pub block: BlockId,
}

/// A statement that calls an extern function with branches, and "calls" a possibly different block
/// for each branch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementMatchExtern {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    /// A concrete external function to call.
    pub function: semantic::FunctionId,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<VariableId>,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<(ConcreteVariant, BlockId)>,
}

/// A statement that construct a variant of an enum with a single argument, and binds it to a
/// variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementEnumConstruct {
    pub variant: ConcreteVariant,
    /// A living variable in current scope to wrap with the variant.
    pub input: VariableId,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that matches an enum, and "calls" a possibly different block for each branch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementMatchEnum {
    pub concrete_enum_id: ConcreteEnumId,
    /// A living variable in current scope to match on.
    pub input: VariableId,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<(ConcreteVariant, BlockId)>,
}

/// A statement that constructs a struct (tuple included) into a new variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementStructConstruct {
    pub inputs: Vec<VariableId>,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that destructures a struct (tuple included), introducing its elements as new
/// variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementStructDestructure {
    /// A living variable in current scope to destructure.
    pub input: VariableId,
    /// The variables to bind values to.
    pub outputs: Vec<VariableId>,
}
