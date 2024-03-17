//! Intermediate representation objects after lowering from semantic.
//! This representation is SSA (static single-assignment): each variable is defined before usage and
//! assigned once. It is also normal form: each function argument is a variable, rather than a
//! compound expression.

use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{DiagnosticNote, Diagnostics};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::{ConcreteEnumId, ConcreteVariant};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::{Arena, Id};

pub mod blocks;
pub use blocks::BlockId;
use semantic::expr::inference::{InferenceError, InferenceResult};
use semantic::items::constant::ConstValue;
use semantic::items::imp::ImplId;
use semantic::MatchArmSelector;

use self::blocks::FlatBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostic;
use crate::ids::{FunctionId, LocationId, Signature};

/// The Location struct represents the source location of a lowered object. It is used to store the
/// most relevant source location for a lowering object.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Location {
    /// The stable location of the object.
    pub stable_location: StableLocation,
    /// Additional notes about the origin of the object, for example if the object was
    /// auto-generated by the compiler.
    /// New notes are appended to the end of the vector.
    pub notes: Vec<DiagnosticNote>,
}
impl Location {
    pub fn new(stable_location: StableLocation) -> Self {
        Self { stable_location, notes: vec![] }
    }

    /// Creates a new Location with the given note as the last note.
    pub fn with_note(mut self, note: DiagnosticNote) -> Self {
        self.notes.push(note);
        self
    }

    /// Creates a new Location with the given note as the last note.
    pub fn maybe_with_note(mut self, note: Option<DiagnosticNote>) -> Self {
        let Some(note) = note else {
            return self;
        };
        self.notes.push(note);
        self
    }

    /// Creates a new Location with the a note from the given text and location.
    pub fn add_note_with_location(
        self,
        db: &dyn LoweringGroup,
        text: &str,
        location: LocationId,
    ) -> Self {
        self.with_note(DiagnosticNote::with_location(
            text.into(),
            location.get(db).stable_location.diagnostic_location(db.upcast()),
        ))
    }
}

impl DebugWithDb<dyn LoweringGroup> for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn LoweringGroup) -> std::fmt::Result {
        let files_db = db.upcast();
        self.stable_location.diagnostic_location(db.upcast()).fmt(f, files_db)?;

        for note in &self.notes {
            f.write_str("\nnote: ")?;
            note.fmt(f, files_db)?;
        }
        Ok(())
    }
}

pub type VariableId = Id<Variable>;

/// Represents a usage of a variable.
///
/// For example if we have:
///
/// fn foo(a: u32) {
///     1 + a
/// }
///
/// Then the right hand side of the tail expression `1 + a` is a VarUsage object with
/// the variable id of the variable `a` and the location:
///     1 + a
///         ^
/// Note that the location associated with the variable that was assigned to 'a' is
/// fn foo(a: u32)
///        ^
/// and it is different from the location in the VarUsage.
///
/// The tail expression `1 + a`  is also going to be assigned a variable and a VarUsage.
/// in that case, the location of both the variable and the usage will be the same.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarUsage {
    pub var_id: VariableId,
    pub location: LocationId,
}

/// A lowered function code using flat blocks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FlatLowered {
    /// Diagnostics produced while lowering.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    /// Function signature.
    pub signature: Signature,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: FlatBlocks,
    /// function parameters, including implicits.
    pub parameters: Vec<VariableId>,
}

/// Remapping of lowered variable ids. Useful for convergence of branches.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct VarRemapping {
    /// Map from new_var to old_var (since new_var cannot appear twice, but old_var can).
    pub remapping: OrderedHashMap<VariableId, VarUsage>,
}
impl Deref for VarRemapping {
    type Target = OrderedHashMap<VariableId, VarUsage>;

    fn deref(&self) -> &Self::Target {
        &self.remapping
    }
}
impl DerefMut for VarRemapping {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.remapping
    }
}

/// A block of statements. Unlike [`FlatBlock`], this has no reference information,
/// and no panic ending.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FlatBlock {
    /// Statements sequence running one after the other in the block, in a linear flow.
    /// Note: Inner blocks might end with a `return`, which will exit the function in the middle.
    /// Note: Match is a possible statement, which means it has control flow logic inside, but
    /// after its execution is completed, the flow returns to the following statement of the block.
    pub statements: Vec<Statement>,
    /// Describes how this block ends: returns to the caller or exits the function.
    pub end: FlatBlockEnd,
}
impl Default for FlatBlock {
    fn default() -> Self {
        Self { statements: Default::default(), end: FlatBlockEnd::NotSet }
    }
}
impl FlatBlock {
    pub fn is_set(&self) -> bool {
        !matches!(self.end, FlatBlockEnd::NotSet)
    }
}

/// Describes what happens to the program flow at the end of a [`FlatBlock`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlatBlockEnd {
    /// The block was created but still needs to be populated. Block must not be in this state in
    /// the end of the lowering phase.
    NotSet,
    /// This block ends with a `return` statement, exiting the function.
    Return(Vec<VarUsage>, LocationId),
    /// This block ends with a panic.
    Panic(VarUsage),
    /// This block ends with a jump to a different block.
    Goto(BlockId, VarRemapping),
    Match {
        info: MatchInfo,
    },
}

/// Lowered variable representation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    /// Can the type be (trivially) dropped.
    pub droppable: Result<ImplId, InferenceError>,
    /// Can the type be (trivially) copied.
    pub copyable: Result<ImplId, InferenceError>,
    /// A Destruct impl for the type, if found.
    pub destruct_impl: Result<ImplId, InferenceError>,
    /// A PanicDestruct impl for the type, if found.
    pub panic_destruct_impl: Result<ImplId, InferenceError>,
    /// Semantic type of the variable.
    pub ty: semantic::TypeId,
    /// Location of the variable.
    pub location: LocationId,
}

/// Lowered statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    // Values.
    Const(StatementConst),

    // Flow control.
    Call(StatementCall),

    // Structs (including tuples).
    StructConstruct(StatementStructConstruct),
    StructDestructure(StatementStructDestructure),

    // Enums.
    EnumConstruct(StatementEnumConstruct),

    Snapshot(StatementSnapshot),
    Desnap(StatementDesnap),
}
impl Statement {
    pub fn inputs(&self) -> &[VarUsage] {
        match &self {
            Statement::Const(_stmt) => &[],
            Statement::Call(stmt) => stmt.inputs.as_slice(),
            Statement::StructConstruct(stmt) => stmt.inputs.as_slice(),
            Statement::StructDestructure(stmt) => std::slice::from_ref(&stmt.input),
            Statement::EnumConstruct(stmt) => std::slice::from_ref(&stmt.input),
            Statement::Snapshot(stmt) => std::slice::from_ref(&stmt.input),
            Statement::Desnap(stmt) => std::slice::from_ref(&stmt.input),
        }
    }

    pub fn inputs_mut(&mut self) -> &mut [VarUsage] {
        match self {
            Statement::Const(_stmt) => &mut [],
            Statement::Call(stmt) => stmt.inputs.as_mut_slice(),
            Statement::StructConstruct(stmt) => stmt.inputs.as_mut_slice(),
            Statement::StructDestructure(stmt) => std::slice::from_mut(&mut stmt.input),
            Statement::EnumConstruct(stmt) => std::slice::from_mut(&mut stmt.input),
            Statement::Snapshot(stmt) => std::slice::from_mut(&mut stmt.input),
            Statement::Desnap(stmt) => std::slice::from_mut(&mut stmt.input),
        }
    }

    pub fn outputs(&self) -> &[VariableId] {
        match &self {
            Statement::Const(stmt) => std::slice::from_ref(&stmt.output),
            Statement::Call(stmt) => stmt.outputs.as_slice(),
            Statement::StructConstruct(stmt) => std::slice::from_ref(&stmt.output),
            Statement::StructDestructure(stmt) => stmt.outputs.as_slice(),
            Statement::EnumConstruct(stmt) => std::slice::from_ref(&stmt.output),
            Statement::Snapshot(stmt) => stmt.outputs.as_slice(),
            Statement::Desnap(stmt) => std::slice::from_ref(&stmt.output),
        }
    }
    pub fn location(&self) -> Option<LocationId> {
        // TODO(Gil): Add location to all statements.
        match &self {
            Statement::Const(_) => None,
            Statement::Call(stmt) => Some(stmt.location),
            Statement::StructConstruct(_) => None,
            Statement::StructDestructure(stmt) => Some(stmt.input.location),
            Statement::EnumConstruct(stmt) => Some(stmt.input.location),
            Statement::Snapshot(stmt) => Some(stmt.input.location),
            Statement::Desnap(stmt) => Some(stmt.input.location),
        }
    }
}

/// A statement that binds a const value to a variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementConst {
    /// The value of the const.
    pub value: ConstValue,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that calls a user function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementCall {
    /// A function to "call".
    pub function: FunctionId,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<VarUsage>,
    /// Is the last input a coupon for the function call. See
    /// [semantic::ExprFunctionCall::coupon_arg] for more information.
    pub with_coupon: bool,
    /// New variables to be introduced into the current scope from the function outputs.
    pub outputs: Vec<VariableId>,
    /// Location for the call.
    pub location: LocationId,
}

/// A statement that construct a variant of an enum with a single argument, and binds it to a
/// variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementEnumConstruct {
    pub variant: ConcreteVariant,
    /// A living variable in current scope to wrap with the variant.
    pub input: VarUsage,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that constructs a struct (tuple included) into a new variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementStructConstruct {
    pub inputs: Vec<VarUsage>,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// A statement that destructures a struct (tuple included), introducing its elements as new
/// variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementStructDestructure {
    /// A living variable in current scope to destructure.
    pub input: VarUsage,
    /// The variables to bind values to.
    pub outputs: Vec<VariableId>,
}

/// A statement that takes a snapshot of a variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementSnapshot {
    pub input: VarUsage,
    outputs: [VariableId; 2],
}
impl StatementSnapshot {
    pub fn new(input: VarUsage, output_original: VariableId, output_snapshot: VariableId) -> Self {
        Self { input, outputs: [output_original, output_snapshot] }
    }
    pub fn original(&self) -> VariableId {
        self.outputs[0]
    }
    pub fn snapshot(&self) -> VariableId {
        self.outputs[1]
    }
}

/// A statement that desnaps a variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StatementDesnap {
    pub input: VarUsage,
    /// The variable to bind the value to.
    pub output: VariableId,
}

/// An arm of a match statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchArm {
    /// The selector of the arm.
    pub arm_selector: MatchArmSelector,

    /// The block_id where the relevant arm is implemented.
    pub block_id: BlockId,

    /// The list of variable ids introduced in this arm.
    pub var_ids: Vec<VariableId>,
}

/// A statement that calls an extern function with branches, and "calls" a possibly different block
/// for each branch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchExternInfo {
    // TODO(spapini): ConcreteExternFunctionId once it exists.
    /// A concrete external function to call.
    pub function: FunctionId,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<VarUsage>,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<MatchArm>,
    /// Location for the call.
    pub location: LocationId,
}

/// A statement that matches an enum, and "calls" a possibly different block for each branch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchEnumInfo {
    pub concrete_enum_id: ConcreteEnumId,
    /// A living variable in current scope to match on.
    pub input: VarUsage,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<MatchArm>,
    /// Location for the match.
    pub location: LocationId,
}
/// A statement that matches an index enum for matching on felt252, and "calls" a possibly different
/// block for each branch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchEnumValue {
    pub num_of_arms: usize,

    /// A living variable in current scope to match on.
    pub input: VarUsage,
    /// Match arms. All blocks should have the same rets.
    pub arms: Vec<MatchArm>,
    /// Location for the match.
    pub location: LocationId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MatchInfo {
    Enum(MatchEnumInfo),
    Extern(MatchExternInfo),
    Value(MatchEnumValue),
}
impl MatchInfo {
    pub fn inputs(&self) -> &[VarUsage] {
        match self {
            MatchInfo::Enum(s) => std::slice::from_ref(&s.input),
            MatchInfo::Extern(s) => s.inputs.as_slice(),
            MatchInfo::Value(s) => std::slice::from_ref(&s.input),
        }
    }

    pub fn inputs_mut(&mut self) -> &mut [VarUsage] {
        match self {
            MatchInfo::Enum(s) => std::slice::from_mut(&mut s.input),
            MatchInfo::Extern(s) => s.inputs.as_mut_slice(),
            MatchInfo::Value(s) => std::slice::from_mut(&mut s.input),
        }
    }
    pub fn arms(&self) -> &[MatchArm] {
        match self {
            MatchInfo::Enum(s) => &s.arms,
            MatchInfo::Extern(s) => &s.arms,
            MatchInfo::Value(s) => &s.arms,
        }
    }
    pub fn location(&self) -> &LocationId {
        match self {
            MatchInfo::Enum(s) => &s.location,
            MatchInfo::Extern(s) => &s.location,
            MatchInfo::Value(s) => &s.location,
        }
    }
}

/// Used in graph algorithms, and describes how to construct the edges in function dependency graph.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DependencyType {
    /// A function depends on another function if it may call it.
    Call,
    /// A function depends on another function if its cost depends on the other function's cost.
    Cost,
}
