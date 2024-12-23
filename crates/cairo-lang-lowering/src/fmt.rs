use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{GenericParamId, NamedLanguageElementId, TopLevelLanguageElementId};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::constant::{ConstValue, ConstValueId};
use cairo_lang_semantic::items::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use cairo_lang_semantic::items::imp::{ImplId, ImplLongId};
use cairo_lang_semantic::{MatchArmSelector, TypeId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::GenericArg;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::Arena;
use itertools::Itertools;
use num_bigint::BigInt;
use serde::ser::{SerializeSeq, SerializeStruct, SerializeStructVariant};
use serde::{Deserialize, Serialize, Serializer, de};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use crate::db::LoweringGroup;
use crate::ids::{
    FunctionId, FunctionLongId, GeneratedFunction, GeneratedFunctionKey, LocationId, Signature,
};
use crate::objects::{
    BlockId, MatchExternInfo, Statement, StatementCall, StatementConst, StatementStructDestructure,
    VariableId,
};
use crate::{
    FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchEnumValue, MatchInfo,
    StatementDesnap, StatementEnumConstruct, StatementSnapshot, StatementStructConstruct,
    VarRemapping, VarUsage, Variable,
};

/// Holds all the information needed for formatting lowered representations.
/// Acts like a "db" for DebugWithDb.
pub struct LoweredFormatter<'db> {
    pub db: &'db dyn LoweringGroup,
    pub variables: &'db Arena<Variable>,
    pub include_usage_location: bool,
}
impl<'db> LoweredFormatter<'db> {
    pub fn new(db: &'db dyn LoweringGroup, variables: &'db Arena<Variable>) -> Self {
        Self { db, variables, include_usage_location: false }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for VarRemapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let mut remapping = self.iter().peekable();
        write!(f, "{{")?;
        while let Some((dst, src)) = remapping.next() {
            src.var_id.fmt(f, ctx)?;
            write!(f, " -> ")?;
            dst.fmt(f, ctx)?;
            if remapping.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}
impl DebugWithDb<LoweredFormatter<'_>> for FlatLowered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "Parameters:")?;
        let mut inputs = self.parameters.iter().peekable();
        while let Some(var) = inputs.next() {
            write!(f, " ")?;
            format_var_with_ty(*var, f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ",")?;
            }
        }
        writeln!(f)?;
        let mut blocks = self.blocks.iter();
        if let Some((root_block_id, root_block)) = blocks.next() {
            root_block_id.fmt(f, ctx)?;
            writeln!(f, " (root):")?;
            root_block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        for (block_id, block) in blocks {
            block_id.fmt(f, ctx)?;
            writeln!(f, ":")?;
            block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for FlatBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        writeln!(f, "Statements:")?;
        for stmt in &self.statements {
            write!(f, "  ")?;
            stmt.fmt(f, ctx)?;
            writeln!(f)?;
        }

        writeln!(f, "End:")?;
        self.end.fmt(f, ctx)?;
        writeln!(f)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for FlatBlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let outputs = match &self {
            FlatBlockEnd::Return(returns, _location) => {
                write!(f, "  Return(")?;
                returns.iter().map(|var_usage| var_usage.var_id).collect()
            }
            FlatBlockEnd::Panic(data) => {
                write!(f, "  Panic(")?;
                vec![data.var_id]
            }
            FlatBlockEnd::Goto(block_id, remapping) => {
                return write!(f, "  Goto({:?}, {:?})", block_id.debug(ctx), remapping.debug(ctx));
            }
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Match { info } => {
                return write!(f, "  Match({:?})", info.debug(ctx));
            }
        };
        let mut outputs = outputs.iter().peekable();
        while let Some(var) = outputs.next() {
            var.fmt(f, ctx)?;
            if outputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

fn format_var_with_ty(
    var_id: VariableId,
    f: &mut std::fmt::Formatter<'_>,
    ctx: &LoweredFormatter<'_>,
) -> std::fmt::Result {
    var_id.fmt(f, ctx)?;
    let var = &ctx.variables[var_id];
    write!(f, ": {}", var.ty.format(ctx.db.upcast()))
}

impl DebugWithDb<LoweredFormatter<'_>> for BlockId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _lowered: &LoweredFormatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "blk{:?}", self.0)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for VarUsage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "v{:?}", self.var_id.index(),)?;
        if ctx.include_usage_location {
            write!(
                f,
                "{{`{}`}}",
                self.location
                    .lookup_intern(ctx.db)
                    .stable_location
                    .syntax_node(ctx.db.upcast())
                    .get_text_without_trivia(ctx.db.upcast())
                    .lines()
                    .map(|s| s.trim())
                    .join(" ")
            )?;
        }
        Ok(())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for VariableId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _lowered: &LoweredFormatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "v{:?}", self.index())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut outputs = self.outputs().iter().peekable();
        while let Some(var) = outputs.next() {
            format_var_with_ty(*var, f, ctx)?;
            if outputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ") <- ")?;
        match self {
            Statement::Const(stmt) => stmt.fmt(f, ctx),
            Statement::Call(stmt) => stmt.fmt(f, ctx),
            Statement::StructConstruct(stmt) => stmt.fmt(f, ctx),
            Statement::StructDestructure(stmt) => stmt.fmt(f, ctx),
            Statement::EnumConstruct(stmt) => stmt.fmt(f, ctx),
            Statement::Snapshot(stmt) => stmt.fmt(f, ctx),
            Statement::Desnap(stmt) => stmt.fmt(f, ctx),
        }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        match self {
            MatchInfo::Extern(s) => s.fmt(f, ctx),
            MatchInfo::Enum(s) => s.fmt(f, ctx),
            MatchInfo::Value(s) => s.fmt(f, ctx),
        }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        self.value.fmt(f, ctx.db)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(", self.function.lookup_intern(ctx.db).debug(ctx.db))?;
        for (i, var) in self.inputs.iter().enumerate() {
            let is_last = i == self.inputs.len() - 1;
            if is_last && self.with_coupon {
                write!(f, "__coupon__: ")?;
            }
            var.fmt(f, ctx)?;
            if !is_last {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchExternInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match {:?}(", self.function.lookup_intern(ctx.db).debug(ctx.db))?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            var.fmt(f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
            writeln!(f)?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "    {:?}", self.arm_selector.debug(ctx.db))?;

        if !self.var_ids.is_empty() {
            write!(f, "(")?;
            let mut var_ids = self.var_ids.iter().peekable();
            while let Some(var_id) = var_ids.next() {
                var_id.fmt(f, ctx)?;
                if var_ids.peek().is_some() {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
        }

        write!(f, " => {:?},", self.block_id.debug(ctx))
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchEnumInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match_enum(")?;
        self.input.fmt(f, ctx)?;
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
            writeln!(f)?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchEnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match_enum.(")?;
        self.input.fmt(f, ctx)?;
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
            writeln!(f)?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementEnumConstruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let enum_name =
            self.variant.concrete_enum_id.enum_id(ctx.db.upcast()).name(ctx.db.upcast());
        let variant_name = self.variant.id.name(ctx.db.upcast());
        write!(f, "{enum_name}::{variant_name}(",)?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementStructConstruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "struct_construct(")?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            var.fmt(f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementStructDestructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "struct_destructure(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementSnapshot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "snapshot(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementDesnap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "desnap(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

#[derive(Serialize, Deserialize)]
pub struct FlatLoweredSerializeable {
    /// Function signature.
    signature: SignatureSerializeable,
    /// Arena of allocated lowered variables.
    variables: Vec<VariableSerializeable>,
    /// Arena of allocated lowered blocks.
    blocks: Vec<FlatBlockSerializeable>,
    /// function parameters, including implicits.
    parameters: Vec<usize>,
}
impl FlatLoweredSerializeable {
    pub fn new(flat_lowered: FlatLowered, db: &dyn LoweringGroup) -> Self {
        Self {
            signature: SignatureSerializeable::new(flat_lowered.signature, db),
            variables: flat_lowered
                .variables
                .into_iter()
                .map(|var| VariableSerializeable::new(var.1, db))
                .collect(),
            blocks: flat_lowered
                .blocks
                .into_iter()
                .map(|block: (BlockId, &FlatBlock)| {
                    FlatBlockSerializeable::new(block.1.clone(), db)
                })
                .collect(),
            parameters: flat_lowered.parameters.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SignatureSerializeable {
    /// Function parameters.
    pub params: Vec<ExprVarMemberPathSerializeable>,
    /// Extra return values.
    pub extra_rets: Vec<ExprVarMemberPathSerializeable>,
    /// Return type.
    pub return_type: TypeSerializeable,
    /// Implicit parameters.
    pub implicits: Vec<TypeSerializeable>,
    /// Whether the function is panicable.
    pub panicable: bool,
}
impl SignatureSerializeable {
    fn new(signature: Signature, db: &dyn LoweringGroup) -> Self {
        Self {
            params: signature
                .params
                .into_iter()
                .map(|var| ExprVarMemberPathSerializeable::new(var, db.upcast()))
                .collect(),
            extra_rets: signature
                .extra_rets
                .into_iter()
                .map(|var| ExprVarMemberPathSerializeable::new(var, db.upcast()))
                .collect(),

            return_type: TypeSerializeable::new(signature.return_type, db.upcast()),
            implicits: signature
                .implicits
                .into_iter()
                .map(|ty| TypeSerializeable::new(ty, db.upcast()))
                .collect(),
            panicable: signature.panicable,
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ExprVarMemberPathSerializeable {
    Var(ExprVarSerializeable),
    Member {
        parent: Box<ExprVarMemberPathSerializeable>,
        member_id: String,
        concrete_struct_id: ConcreteStructSerializeable,
        ty: TypeSerializeable,
    },
}
impl ExprVarMemberPathSerializeable {
    fn new(expr_var_member_path: semantic::ExprVarMemberPath, db: &dyn SemanticGroup) -> Self {
        match expr_var_member_path {
            semantic::ExprVarMemberPath::Var(var) => {
                ExprVarMemberPathSerializeable::Var(ExprVarSerializeable::new(var, db))
            }
            semantic::ExprVarMemberPath::Member {
                parent,
                member_id,
                stable_ptr: _,
                concrete_struct_id,
                ty,
            } => ExprVarMemberPathSerializeable::Member {
                parent: Box::new(ExprVarMemberPathSerializeable::new(*parent, db)),
                member_id: member_id.full_path(db.upcast()),
                concrete_struct_id: ConcreteStructSerializeable::new(concrete_struct_id, db),
                ty: TypeSerializeable::new(ty, db),
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ExprVarSerializeable {
    /// Variable type.
    pub ty: TypeSerializeable,
}
impl ExprVarSerializeable {
    fn new(expr_var: semantic::ExprVar, db: &dyn SemanticGroup) -> Self {
        Self { ty: TypeSerializeable::new(expr_var.ty, db) }
    }
}

#[derive(Serialize, Deserialize)]
struct VariableSerializeable {
    pub droppable: Option<ImplSerializeable>,
    /// Can the type be (trivially) copied.
    pub copyable: Option<ImplSerializeable>,
    /// A Destruct impl for the type, if found.
    pub destruct_impl: Option<ImplSerializeable>,
    /// A PanicDestruct impl for the type, if found.
    pub panic_destruct_impl: Option<ImplSerializeable>,
    /// Semantic type of the variable.
    pub ty: TypeSerializeable,
}
impl VariableSerializeable {
    fn new(variable: Variable, db: &dyn LoweringGroup) -> Self {
        Self {
            droppable: variable
                .droppable
                .map(|impl_id| ImplSerializeable::new(impl_id, db.upcast()))
                .ok(),
            copyable: variable
                .copyable
                .map(|impl_id| ImplSerializeable::new(impl_id, db.upcast()))
                .ok(),
            destruct_impl: variable
                .destruct_impl
                .map(|impl_id| ImplSerializeable::new(impl_id, db.upcast()))
                .ok(),
            panic_destruct_impl: variable
                .panic_destruct_impl
                .map(|impl_id| ImplSerializeable::new(impl_id, db.upcast()))
                .ok(),
            ty: TypeSerializeable::new(variable.ty, db.upcast()),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct FlatBlockSerializeable {
    /// Statements in the block.
    pub statements: Vec<StatementSerializeable>,
    /// Block end.
    pub end: FlatBlockEndSerializeable,
}
impl FlatBlockSerializeable {
    fn new(flat_block: FlatBlock, db: &dyn LoweringGroup) -> Self {
        Self {
            statements: flat_block
                .statements
                .into_iter()
                .map(|stmt| StatementSerializeable::new(stmt, db))
                .collect(),
            end: FlatBlockEndSerializeable::new(flat_block.end, db),
        }
    }
}
#[derive(Serialize, Deserialize)]
enum FlatBlockEndSerializeable {
    /// The block was created but still needs to be populated. Block must not be in this state in
    /// the end of the lowering phase.
    NotSet,
    /// This block ends with a `return` statement, exiting the function.
    Return(Vec<usize>),
    /// This block ends with a panic.
    Panic(usize),
    /// This block ends with a jump to a different block.
    Goto(usize, VarRemappingSerializeable),
    Match {
        info: MatchInfoSerializeable,
    },
}
impl FlatBlockEndSerializeable {
    fn new(flat_block_end: FlatBlockEnd, db: &dyn LoweringGroup) -> Self {
        match flat_block_end {
            FlatBlockEnd::Return(returns, _location) => FlatBlockEndSerializeable::Return(
                returns.iter().map(|var| var.var_id.index()).collect(),
            ),
            FlatBlockEnd::Panic(data) => FlatBlockEndSerializeable::Panic(data.var_id.index()),
            FlatBlockEnd::Goto(block_id, remapping) => FlatBlockEndSerializeable::Goto(
                block_id.0,
                VarRemappingSerializeable::new(remapping),
            ),
            FlatBlockEnd::NotSet => FlatBlockEndSerializeable::NotSet,
            FlatBlockEnd::Match { info } => {
                FlatBlockEndSerializeable::Match { info: MatchInfoSerializeable::new(info, db) }
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct VarRemappingSerializeable {
    /// Map from new_var to old_var (since new_var cannot appear twice, but old_var can).
    pub remapping: OrderedHashMap<usize, usize>,
}
impl VarRemappingSerializeable {
    fn new(var_remapping: VarRemapping) -> Self {
        Self {
            remapping: var_remapping
                .iter()
                .map(|(dst, src)| (dst.index(), src.var_id.index()))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum MatchInfoSerializeable {
    Enum(MatchEnumInfoSerializeable),
    Extern(MatchExternInfoSerializeable),
    Value(MatchEnumValueSerializeable),
}
impl MatchInfoSerializeable {
    fn new(match_info: MatchInfo, db: &dyn LoweringGroup) -> Self {
        match match_info {
            MatchInfo::Enum(info) => {
                MatchInfoSerializeable::Enum(MatchEnumInfoSerializeable::new(info, db))
            }
            MatchInfo::Extern(info) => {
                MatchInfoSerializeable::Extern(MatchExternInfoSerializeable::new(info, db))
            }
            MatchInfo::Value(info) => {
                MatchInfoSerializeable::Value(MatchEnumValueSerializeable::new(info, db))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchEnumInfoSerializeable {
    pub concrete_enum_id: ConcreteEnumSerializeable,
    /// A living variable in current scope to match on.
    pub input: usize,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<MatchArmSerializeable>,
}
impl MatchEnumInfoSerializeable {
    fn new(match_enum_info: MatchEnumInfo, db: &dyn LoweringGroup) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumSerializeable::new(
                match_enum_info.concrete_enum_id,
                db.upcast(),
            ),
            input: match_enum_info.input.var_id.index(),
            arms: match_enum_info
                .arms
                .into_iter()
                .map(|arm| MatchArmSerializeable::new(arm, db))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchExternInfoSerializeable {
    /// A concrete external function to call.
    pub function: FunctionSerializeable,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<usize>,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<MatchArmSerializeable>,
}

impl MatchExternInfoSerializeable {
    fn new(match_extern_info: MatchExternInfo, db: &dyn LoweringGroup) -> Self {
        Self {
            function: FunctionSerializeable::new(match_extern_info.function, db),
            inputs: match_extern_info.inputs.iter().map(|var| var.var_id.index()).collect(),
            arms: match_extern_info
                .arms
                .into_iter()
                .map(|arm| MatchArmSerializeable::new(arm, db))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchEnumValueSerializeable {
    pub num_of_arms: usize,

    /// A living variable in current scope to match on.
    pub input: usize,
    /// Match arms. All blocks should have the same rets.
    pub arms: Vec<MatchArmSerializeable>,
}

impl MatchEnumValueSerializeable {
    fn new(match_enum_value: MatchEnumValue, db: &dyn LoweringGroup) -> Self {
        Self {
            num_of_arms: match_enum_value.num_of_arms,
            input: match_enum_value.input.var_id.index(),
            arms: match_enum_value
                .arms
                .into_iter()
                .map(|arm| MatchArmSerializeable::new(arm, db))
                .collect(),
        }
    }
}
/// An arm of a match statement.
#[derive(Serialize, Deserialize)]
pub struct MatchArmSerializeable {
    /// The selector of the arm.
    pub arm_selector: MatchArmSelectorSerializeable,

    /// The block_id where the relevant arm is implemented.
    pub block_id: usize,

    /// The list of variable ids introduced in this arm.
    pub var_ids: Vec<usize>,
}

impl MatchArmSerializeable {
    fn new(match_arm: MatchArm, db: &dyn LoweringGroup) -> Self {
        Self {
            arm_selector: MatchArmSelectorSerializeable::new(match_arm.arm_selector, db.upcast()),
            block_id: match_arm.block_id.0,
            var_ids: match_arm.var_ids.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum MatchArmSelectorSerializeable {
    VariantId(ConcreteVariantSerializeable),
    Value(usize),
}

impl MatchArmSelectorSerializeable {
    fn new(match_arm_selector: MatchArmSelector, db: &dyn SemanticGroup) -> Self {
        match match_arm_selector {
            MatchArmSelector::VariantId(variant_id) => MatchArmSelectorSerializeable::VariantId(
                ConcreteVariantSerializeable::new(variant_id, db),
            ),
            MatchArmSelector::Value(value) => MatchArmSelectorSerializeable::Value(value.value),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum StatementSerializeable {
    // Values.
    Const(StatementConstSerializeable),

    // Flow control.
    Call(StatementCallSerializeable),

    // Structs (including tuples).
    StructConstruct(StatementStructConstructSerializeable),
    StructDestructure(StatementStructDestructureSerializeable),

    // Enums.
    EnumConstruct(StatementEnumConstructSerializeable),

    Snapshot(StatementSnapshotSerializeable),
    Desnap(StatementDesnapSerializeable),
}

impl StatementSerializeable {
    fn new(stmt: Statement, db: &dyn LoweringGroup) -> Self {
        match stmt {
            Statement::Const(stmt) => {
                StatementSerializeable::Const(StatementConstSerializeable::new(stmt, db))
            }
            Statement::Call(stmt) => {
                StatementSerializeable::Call(StatementCallSerializeable::new(stmt, db))
            }
            Statement::StructConstruct(stmt) => StatementSerializeable::StructConstruct(
                StatementStructConstructSerializeable::new(stmt, db),
            ),
            Statement::StructDestructure(stmt) => StatementSerializeable::StructDestructure(
                StatementStructDestructureSerializeable::new(stmt, db),
            ),
            Statement::EnumConstruct(stmt) => StatementSerializeable::EnumConstruct(
                StatementEnumConstructSerializeable::new(stmt, db),
            ),
            Statement::Snapshot(stmt) => {
                StatementSerializeable::Snapshot(StatementSnapshotSerializeable::new(stmt, db))
            }
            Statement::Desnap(stmt) => {
                StatementSerializeable::Desnap(StatementDesnapSerializeable::new(stmt, db))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementConstSerializeable {
    /// The value of the const.
    pub value: ConstValueSerializeable,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementConstSerializeable {
    fn new(stmt: StatementConst, db: &dyn LoweringGroup) -> Self {
        Self {
            value: ConstValueSerializeable::new(stmt.value, db.upcast()),
            output: stmt.output.index(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ConstValueSerializeable {
    Int(BigInt, TypeSerializeable),
    Struct(Vec<ConstValueSerializeable>, TypeSerializeable),
    Enum(ConcreteVariantSerializeable, Box<ConstValueSerializeable>),
    NonZero(Box<ConstValueSerializeable>),
    Boxed(Box<ConstValueSerializeable>),
    Generic(GenericParamSerializeable),
}
impl ConstValueSerializeable {
    fn new(const_value_id: ConstValue, db: &dyn SemanticGroup) -> Self {
        match const_value_id {
            ConstValue::Int(value, ty) => {
                ConstValueSerializeable::Int(value, TypeSerializeable::new(ty, db))
            }
            ConstValue::Struct(values, ty) => ConstValueSerializeable::Struct(
                values.into_iter().map(|v| ConstValueSerializeable::new(v, db)).collect(),
                TypeSerializeable::new(ty, db),
            ),
            ConstValue::Enum(variant, value) => ConstValueSerializeable::Enum(
                ConcreteVariantSerializeable::new(variant, db),
                Box::new(ConstValueSerializeable::new(*value, db)),
            ),
            ConstValue::NonZero(value) => {
                ConstValueSerializeable::NonZero(Box::new(ConstValueSerializeable::new(*value, db)))
            }
            ConstValue::Boxed(value) => {
                ConstValueSerializeable::Boxed(Box::new(ConstValueSerializeable::new(*value, db)))
            }
            ConstValue::Generic(generic_param) => {
                ConstValueSerializeable::Generic(GenericParamSerializeable::new(generic_param, db))
            }
            _ => todo!(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ConstStatementSerializeable {
    /// Value of the constant.
    pub value: i32,
}

#[derive(Serialize, Deserialize)]
struct StatementCallSerializeable {
    /// A function to "call".
    pub function: FunctionSerializeable,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<usize>,
    /// Is the last input a coupon for the function call. See
    /// [semantic::ExprFunctionCall::coupon_arg] for more information.
    pub with_coupon: bool,
    /// New variables to be introduced into the current scope from the function outputs.
    pub outputs: Vec<usize>,
}
impl StatementCallSerializeable {
    fn new(stmt: StatementCall, db: &dyn LoweringGroup) -> Self {
        Self {
            function: FunctionSerializeable::new(stmt.function, db),
            inputs: stmt.inputs.iter().map(|var| var.var_id.index()).collect(),
            with_coupon: stmt.with_coupon,
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum FunctionSerializeable {
    /// An original function from the user code.
    Semantic(SemanticFunctionSerializeable),
    /// A function generated by the compiler.
    Generated(GeneratedFunctionSerializeable),
}
impl FunctionSerializeable {
    fn new(function: FunctionId, db: &dyn LoweringGroup) -> Self {
        match function.lookup_intern(db) {
            FunctionLongId::Semantic(id) => {
                FunctionSerializeable::Semantic(SemanticFunctionSerializeable::new(id, db.upcast()))
            }
            FunctionLongId::Generated(id) => {
                FunctionSerializeable::Generated(GeneratedFunctionSerializeable::new(id, db))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticFunctionSerializeable {
    generic_funtion: GenericFunctionSerializeable,

    generic_args: Vec<GenericArgumentSerializeable>,
}
impl SemanticFunctionSerializeable {
    fn new(function_id: semantic::FunctionId, db: &dyn SemanticGroup) -> Self {
        let long_id = function_id.lookup_intern(db).function;
        Self {
            generic_funtion: GenericFunctionSerializeable::new(long_id.generic_function, db),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializeable::new(arg, db))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum GenericFunctionSerializeable {
    /// A generic free function.
    Free(String),
    /// A generic extern function.
    Extern(String),
    /// A generic function of an impl.
    Impl(ImplSerializeable, String),
}
impl GenericFunctionSerializeable {
    fn new(generic_function: GenericFunctionId, db: &dyn SemanticGroup) -> Self {
        match generic_function {
            GenericFunctionId::Free(id) => {
                GenericFunctionSerializeable::Free(id.full_path(db.upcast()))
            }
            GenericFunctionId::Extern(id) => {
                GenericFunctionSerializeable::Extern(id.full_path(db.upcast()))
            }
            GenericFunctionId::Impl(id) => GenericFunctionSerializeable::Impl(
                ImplSerializeable::new(id.impl_id, db),
                id.function.full_path(db.upcast()),
            ),
            GenericFunctionId::Trait(id_) => {
                panic!("Trait functions are not supported in serialization")
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct GeneratedFunctionSerializeable {
    pub parent: SemanticConcreteFunctionWithBodySerializeable,
    pub key: GeneratedFunctionKeySerializeable,
}
impl GeneratedFunctionSerializeable {
    fn new(function: GeneratedFunction, db: &dyn LoweringGroup) -> Self {
        Self {
            parent: SemanticConcreteFunctionWithBodySerializeable::new(
                function.parent,
                db.upcast(),
            ),
            key: GeneratedFunctionKeySerializeable::new(function.key, db),
        }
    }
}
#[derive(Serialize, Deserialize)]
struct SemanticConcreteFunctionWithBodySerializeable {
    pub generic_function: GenericFunctionWithBodySerializeable,
    pub generic_args: Vec<GenericArgumentSerializeable>,
}
impl SemanticConcreteFunctionWithBodySerializeable {
    fn new(function_id: semantic::ConcreteFunctionWithBodyId, db: &dyn SemanticGroup) -> Self {
        Self {
            generic_function: GenericFunctionWithBodySerializeable::new(
                function_id.generic_function(db),
                db,
            ),
            generic_args: function_id
                .lookup_intern(db)
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializeable::new(arg, db))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum GenericFunctionWithBodySerializeable {
    Free(String),
    Impl(ConcreteImplSerializeable, String),
}

impl GenericFunctionWithBodySerializeable {
    fn new(generic_function: GenericFunctionWithBodyId, db: &dyn SemanticGroup) -> Self {
        match generic_function {
            GenericFunctionWithBodyId::Free(id) => {
                GenericFunctionWithBodySerializeable::Free(id.full_path(db.upcast()))
            }
            GenericFunctionWithBodyId::Impl(id) => GenericFunctionWithBodySerializeable::Impl(
                ConcreteImplSerializeable::new(id.concrete_impl_id, db),
                id.function_body.name(db.upcast()).into(),
            ),
            GenericFunctionWithBodyId::Trait(id) => {
                unreachable!("Trait functions are not supported in serialization")
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum GeneratedFunctionKeySerializeable {
    Loop(usize),
    TraitFunc(SemanticFunctionSerializeable),
}

impl GeneratedFunctionKeySerializeable {
    fn new(key: GeneratedFunctionKey, db: &dyn LoweringGroup) -> Self {
        match key {
            GeneratedFunctionKey::Loop(id) => GeneratedFunctionKeySerializeable::Loop(id.index()),
            GeneratedFunctionKey::TraitFunc(id, _) => GeneratedFunctionKeySerializeable::TraitFunc(
                SemanticFunctionSerializeable::new(id, db.upcast()),
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementStructConstructSerializeable {
    pub inputs: Vec<usize>,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementStructConstructSerializeable {
    fn new(stmt: StatementStructConstruct, db: &dyn LoweringGroup) -> Self {
        Self {
            inputs: stmt.inputs.iter().map(|var| var.var_id.index()).collect(),
            output: stmt.output.index(),
        }
    }
}
#[derive(Serialize, Deserialize)]
struct StatementStructDestructureSerializeable {
    /// A living variable in current scope to destructure.
    pub input: usize,
    /// The variables to bind values to.
    pub outputs: Vec<usize>,
}
impl StatementStructDestructureSerializeable {
    fn new(stmt: StatementStructDestructure, db: &dyn LoweringGroup) -> Self {
        Self {
            input: stmt.input.var_id.index(),
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementEnumConstructSerializeable {
    pub variant: ConcreteVariantSerializeable,
    /// A living variable in current scope to wrap with the variant.
    pub input: usize,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementEnumConstructSerializeable {
    fn new(stmt: StatementEnumConstruct, db: &dyn LoweringGroup) -> Self {
        Self {
            variant: ConcreteVariantSerializeable::new(stmt.variant, db.upcast()),
            input: stmt.input.var_id.index(),
            output: stmt.output.index(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementSnapshotSerializeable {
    pub input: usize,
    outputs: [usize; 2],
}
impl StatementSnapshotSerializeable {
    fn new(stmt: StatementSnapshot, _db: &dyn LoweringGroup) -> Self {
        Self { input: stmt.input.var_id.index(), outputs: stmt.outputs.map(|var| var.index()) }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementDesnapSerializeable {
    pub input: usize,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementDesnapSerializeable {
    fn new(stmt: StatementDesnap, db: &dyn LoweringGroup) -> Self {
        Self { input: stmt.input.var_id.index(), output: stmt.output.index() }
    }
}

#[derive(Serialize, Deserialize)]
enum GenericArgumentSerializeable {
    Type(TypeSerializeable),
    Value(ConstValueSerializeable),
    Impl(ImplSerializeable),
    NegImpl,
}

impl GenericArgumentSerializeable {
    fn new(
        generic_argument_id: semantic::GenericArgumentId,
        db: &dyn semantic::db::SemanticGroup,
    ) -> Self {
        match generic_argument_id {
            semantic::GenericArgumentId::Type(type_id) => {
                GenericArgumentSerializeable::Type(TypeSerializeable::new(type_id, db))
            }
            semantic::GenericArgumentId::Constant(const_value_id) => {
                GenericArgumentSerializeable::Value(ConstValueSerializeable::new(
                    const_value_id.lookup_intern(db),
                    db,
                ))
            }
            semantic::GenericArgumentId::Impl(impl_id) => {
                GenericArgumentSerializeable::Impl(ImplSerializeable::new(impl_id, db))
            }
            semantic::GenericArgumentId::NegImpl => GenericArgumentSerializeable::NegImpl,
        }
    }
}

#[derive(Serialize, Deserialize)]
enum TypeSerializeable {
    Concrete(ConcreteTypeSerializeable),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeSerializeable>),
    Snapshot(Box<TypeSerializeable>),
    GenericParameter(GenericParamSerializeable),
}

impl TypeSerializeable {
    fn new(type_id: TypeId, db: &dyn SemanticGroup) -> Self {
        match type_id.lookup_intern(db) {
            semantic::TypeLongId::Concrete(concrete_type_id) => {
                TypeSerializeable::Concrete(ConcreteTypeSerializeable::new(concrete_type_id, db))
            }
            semantic::TypeLongId::Tuple(vec) => TypeSerializeable::Tuple(
                vec.into_iter().map(|ty| TypeSerializeable::new(ty, db)).collect(),
            ),
            semantic::TypeLongId::Snapshot(type_id) => {
                TypeSerializeable::Snapshot(Box::new(TypeSerializeable::new(type_id, db)))
            }
            semantic::TypeLongId::GenericParameter(generic_param_id) => {
                TypeSerializeable::GenericParameter(GenericParamSerializeable::new(
                    generic_param_id,
                    db,
                ))
            }
            _ => todo!(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ConcreteTypeSerializeable {
    Struct(ConcreteStructSerializeable),
    Enum(ConcreteEnumSerializeable),
    Extern(String, Vec<GenericArgumentSerializeable>),
}

impl ConcreteTypeSerializeable {
    fn new(concrete_type_id: semantic::ConcreteTypeId, db: &dyn SemanticGroup) -> Self {
        match concrete_type_id {
            semantic::ConcreteTypeId::Struct(id) => {
                ConcreteTypeSerializeable::Struct(ConcreteStructSerializeable::new(id, db))
            }
            semantic::ConcreteTypeId::Enum(id) => {
                ConcreteTypeSerializeable::Enum(ConcreteEnumSerializeable::new(id, db))
            }
            semantic::ConcreteTypeId::Extern(_) => ConcreteTypeSerializeable::Extern(
                concrete_type_id.generic_type(db.upcast()).full_path(db.upcast()),
                concrete_type_id
                    .generic_args(db.upcast())
                    .into_iter()
                    .map(|arg| GenericArgumentSerializeable::new(arg, db))
                    .collect(),
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ImplSerializeable {
    Concrete(ConcreteImplSerializeable),
    GenericParameter(GenericParamSerializeable),
}
impl ImplSerializeable {
    fn new(impl_id: ImplId, db: &dyn SemanticGroup) -> Self {
        match impl_id.lookup_intern(db) {
            ImplLongId::Concrete(concrete_impl) => {
                ImplSerializeable::Concrete(ConcreteImplSerializeable::new(concrete_impl, db))
            }
            ImplLongId::GenericParameter(generic_param_id) => ImplSerializeable::GenericParameter(
                GenericParamSerializeable::new(generic_param_id, db),
            ),
            _ => todo!(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ConcreteImplSerializeable {
    pub impl_id: String,
    pub generic_args: Vec<GenericArgumentSerializeable>,
}
impl ConcreteImplSerializeable {
    fn new(concrete_impl: semantic::ConcreteImplId, db: &dyn SemanticGroup) -> Self {
        let long_id = concrete_impl.lookup_intern(db);
        Self {
            impl_id: long_id.impl_def_id.full_path(db.upcast()),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializeable::new(arg, db))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct GenericParamSerializeable {
    pub name: String,
}
impl GenericParamSerializeable {
    fn new(generic_param_id: GenericParamId, db: &dyn SemanticGroup) -> Self {
        Self { name: generic_param_id.name(db.upcast()).unwrap_or_default().to_string() }
    }
}

#[derive(Serialize, Deserialize)]
struct ConcreteVariantSerializeable {
    pub concrete_enum_id: ConcreteEnumSerializeable,
    pub id: String,
    pub ty: TypeSerializeable,
    /// The index of the variant from within the variant list.
    pub idx: usize,
}
impl ConcreteVariantSerializeable {
    fn new(concrete_variant: semantic::ConcreteVariant, db: &dyn SemanticGroup) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumSerializeable::new(concrete_variant.concrete_enum_id, db),
            id: concrete_variant.id.full_path(db.upcast()),
            ty: TypeSerializeable::new(concrete_variant.ty, db),
            idx: concrete_variant.idx,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ConcreteEnumSerializeable {
    pub enum_id: String,
    pub generic_args: Vec<GenericArgumentSerializeable>,
}

impl ConcreteEnumSerializeable {
    fn new(concrete_enum: semantic::ConcreteEnumId, db: &dyn semantic::db::SemanticGroup) -> Self {
        let long_id = concrete_enum.lookup_intern(db);
        Self {
            enum_id: long_id.enum_id.full_path(db.upcast()),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializeable::new(arg, db))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ConcreteStructSerializeable {
    pub struct_id: String,
    pub generic_args: Vec<GenericArgumentSerializeable>,
}
impl ConcreteStructSerializeable {
    fn new(
        concrete_struct: semantic::ConcreteStructId,
        db: &dyn semantic::db::SemanticGroup,
    ) -> Self {
        let long_id = concrete_struct.lookup_intern(db);
        Self {
            struct_id: long_id.struct_id.full_path(db.upcast()),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializeable::new(arg, db))
                .collect(),
        }
    }
}

// struct ObjectWithDb<'db, T> {
//     object: &'db T,
//     db: &'db dyn LoweringGroup,
// }

// impl<'db> Serialize for ObjectWithDb<'db, FlatLowered> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         let mut state = serializer.serialize_struct("FlatLowered", 4)?;
//         state.serialize_field("signature", &ObjectWithDb {
//             object: &self.object.signature,
//             db: self.db,
//         })?;
//         state.serialize_field("variables", &ObjectWithDb {
//             object: &self.object.variables,
//             db: self.db,
//         })?;
//         // ser.serialize_field("blocks", &self.blocks)?;
//         state.end()
//     }
// }
// impl<'db> Serialize for ObjectWithDb<'db, Signature> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         let mut state = serializer.serialize_struct("Signature", 6)?;
//         state.serialize_field(
//             "params",
//             &self
//                 .object
//                 .params
//                 .iter()
//                 .map(|var| ObjectWithDb { object: var, db: self.db })
//                 .collect_vec(),
//         )?;
//         state.serialize_field(
//             "extra_rets",
//             &self
//                 .object
//                 .extra_rets
//                 .iter()
//                 .map(|var| ObjectWithDb { object: var, db: self.db })
//                 .collect_vec(),
//         )?;
//         state.serialize_field("return_type", &ObjectWithDb {
//             object: &self.object.return_type,
//             db: self.db,
//         })?;

//         state.serialize_field(
//             "implicits",
//             &self
//                 .object
//                 .implicits
//                 .iter()
//                 .map(|var| ObjectWithDb { object: var, db: self.db })
//                 .collect_vec(),
//         )?;

//         state.serialize_field("panicable", &self.object.panicable)?;
//         // should not be read
//         state.serialize_field("location", &ObjectWithDb {
//             object: &self.object.location,
//             db: self.db,
//         })?;

//         state.end()
//     }
// }

// impl<'db> Serialize for ObjectWithDb<'db, semantic::ExprVarMemberPath> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         match self.object {
//             semantic::ExprVarMemberPath::Var(expr_var) => {
//                 serializer.serialize_newtype_variant("ExprVarMemberPath", 0, "Var", &ObjectWithDb
// {                     object: expr_var,
//                     db: self.db,
//                 })
//             }
//             semantic::ExprVarMemberPath::Member {
//                 parent,
//                 member_id,

//                 stable_ptr,
//                 concrete_struct_id,
//                 // Type of the member.
//                 ty,
//             } => {
//                 let mut state =
//                     serializer.serialize_struct_variant("ExprVarMemberPath", 1, "Member", 5)?;
//                 state.serialize_field("parent", &ObjectWithDb {
//                     object: parent.as_ref(),
//                     db: self.db,
//                 })?;
//                 // should not be read
//                 state.serialize_field("member_id", "member_id")?;
//                 // should not be read
//                 state.serialize_field("stable_ptr", "stable_ptr")?;

//                 state.serialize_field("concrete_struct_id", &ObjectWithDb {
//                     object: concrete_struct_id,
//                     db: self.db,
//                 })?;
//                 state.serialize_field("ty", &ObjectWithDb { object: ty, db: self.db })?;
//                 state.end()
//             }
//         }
//     }
// }

// impl<'db> Serialize for ObjectWithDb<'db, semantic::ExprVar> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         let mut state = serializer.serialize_struct("ExprVar", 3)?;
//         state.serialize_field("var", &ObjectWithDb { object: &self.object.var, db: self.db })?;
//         state.serialize_field("ty", &ObjectWithDb { object: &self.object.ty, db: self.db })?;
//         // should not be read
//         state.serialize_field("stable_ptr", "stable_ptr")?;
//         state.end()
//     }
// }

// impl<'db> Serialize for ObjectWithDb<'db, Arena<Variable>> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         let mut state = serializer.serialize_struct("Arena<Variable>", 2)?;
//         state.serialize_field("arena_id", &self.object.)?;
//         state.serialize_field(
//             "extra_rets",
//             &self
//                 .object
//                 .extra_rets
//                 .iter()
//                 .map(|var| ObjectWithDb { object: var, db: self.db })
//                 .collect_vec(),
//         )?;
//         state.serialize_field("return_type", &ObjectWithDb {
//             object: &self.object.return_type,
//             db: self.db,
//         })?;

//         state.serialize_field(
//             "implicits",
//             &self
//                 .object
//                 .implicits
//                 .iter()
//                 .map(|var| ObjectWithDb { object: var, db: self.db })
//                 .collect_vec(),
//         )?;

//         state.serialize_field("panicable", &self.object.panicable)?;
//         // should not be read
//         state.serialize_field("location", &ObjectWithDb {
//             object: &self.object.location,
//             db: self.db,
//         })?;

//         state.end()
//     }
// }

// impl<'db> Serialize for ObjectWithDb<'db, semantic::TypeId> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         serializer.serialize_str(self.object.format(self.db.upcast()).as_str())
//     }
// }
// impl<'db> Serialize for ObjectWithDb<'db, semantic::ConcreteStructId> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         serializer.serialize_str(format!("{:?}", self.object.debug(self.db)).as_str())
//     }
// }

// impl<'db> Serialize for ObjectWithDb<'db, LocationId> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         serializer.serialize_str(
//             format!("{:?}", self.object.lookup_intern(self.db).debug(self.db.upcast())).as_str(),
//         )
//     }
// }

// impl<'db> Serialize for ObjectWithDb<'db, defs::ids::VarId> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         // should not be read;
//         serializer.serialize_str("VarId")
//     }
// }
