use std::default;
use std::mem::take;
use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    GenericParamId, GenericParamLongId, LanguageElementId, LocalVarId, ModuleFileId, ModuleId,
    NamedLanguageElementId, StatementItemId, TopLevelLanguageElementId, TraitConstantId,
    TraitTypeId,
};
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use cairo_lang_semantic::items::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use cairo_lang_semantic::items::imp::{ImplId, ImplLongId};
use cairo_lang_semantic::resolve::Resolver;
use cairo_lang_semantic::types::ImplTypeId;
use cairo_lang_semantic::{MatchArmSelector, TypeId, TypeLongId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::GenericArg;
use cairo_lang_syntax::node::green::{GreenNode, GreenNodeDetails};
use cairo_lang_syntax::node::ids::{GreenId, SyntaxStablePtrId};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::Arena;
use itertools::Itertools;
use num_bigint::BigInt;
use serde::ser::{SerializeSeq, SerializeStruct, SerializeStructVariant};
use serde::{Deserialize, Serialize, Serializer, de};
use smol_str::SmolStr;
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

struct DesSerielizationContext<'db> {
    resolver: Resolver<'db>,
    db: &'db dyn LoweringGroup,
}

pub struct SerielizationContext<'db> {
    db: &'db dyn LoweringGroup,
    data: SerielizationData,
    semantic_ctx: SemanticSerielizationContext<'db>,
}
impl Deref for SerielizationContext<'_> {
    type Target = SerielizationData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SerielizationContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
impl<'db> SerielizationContext<'db> {
    pub fn new(db: &'db dyn LoweringGroup) -> Self {
        Self {
            db,
            data: SerielizationData::default(),
            semantic_ctx: SemanticSerielizationContext {
                db: db.upcast(),
                data: SemanticSerielizationData::default(),
            },
        }
    }
}

#[derive(Default)]
pub struct SerielizationData {
    function_ids: OrderedHashMap<FunctionId, FunctionIdSerializable>,
    lookups: SerielizationLookups,
}
impl Deref for SerielizationData {
    type Target = SerielizationLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SerielizationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct SerielizationLookups {
    function_ids_lookup: Vec<FunctionSerializable>,
}

pub struct SemanticSerielizationContext<'db> {
    db: &'db dyn SemanticGroup,
    data: SemanticSerielizationData,
}
impl Deref for SemanticSerielizationContext<'_> {
    type Target = SemanticSerielizationData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SemanticSerielizationContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Default)]
pub struct SemanticSerielizationData {
    function_ids: OrderedHashMap<semantic::FunctionId, SemanticFunctionIdSerializable>,

    type_ids: OrderedHashMap<TypeId, TypeIdSerializable>,

    impl_ids: OrderedHashMap<ImplId, ImplIdSerializable>,

    green_ids: OrderedHashMap<GreenId, GreenIdSerializable>,

    syntax_stable_ptr_ids: OrderedHashMap<SyntaxStablePtrId, SyntaxStablePtrIdSerializable>,
    lookups: SemanticSerielizationLookups,
}

impl Deref for SemanticSerielizationData {
    type Target = SemanticSerielizationLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SemanticSerielizationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct SemanticSerielizationLookups {
    function_ids_lookup: Vec<SemanticFunctionSerializable>,
    type_ids_lookup: Vec<TypeSerializable>,
    impl_ids_lookup: Vec<ImplSerializable>,
    green_ids_lookup: Vec<GreenNodeSerializable>,
    syntax_stable_ptr_ids_lookup: Vec<SyntaxStablePtrSerializable>,
}

#[derive(Serialize, Deserialize)]
pub struct FlatLoweredSerializable {
    /// Function signature.
    signature: SignatureSerializable,
    /// Arena of allocated lowered variables.
    variables: Vec<VariableSerializable>,
    /// Arena of allocated lowered blocks.
    blocks: Vec<FlatBlockSerializable>,
    /// function parameters, including implicits.
    parameters: Vec<usize>,
}
impl FlatLoweredSerializable {
    pub fn new(flat_lowered: FlatLowered, ctx: &mut SerielizationContext) -> Self {
        Self {
            signature: SignatureSerializable::new(flat_lowered.signature, ctx),
            variables: flat_lowered
                .variables
                .into_iter()
                .map(|var| VariableSerializable::new(var.1, ctx))
                .collect(),
            blocks: flat_lowered
                .blocks
                .into_iter()
                .map(|block: (BlockId, &FlatBlock)| {
                    FlatBlockSerializable::new(block.1.clone(), ctx)
                })
                .collect(),
            parameters: flat_lowered.parameters.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SignatureSerializable {
    /// Function parameters.
    pub params: Vec<ExprVarMemberPathSerializable>,
    /// Extra return values.
    pub extra_rets: Vec<ExprVarMemberPathSerializable>,
    /// Return type.
    pub return_type: TypeIdSerializable,
    /// Implicit parameters.
    pub implicits: Vec<TypeIdSerializable>,
    /// Whether the function is panicable.
    pub panicable: bool,
}
impl SignatureSerializable {
    fn new(signature: Signature, ctx: &mut SerielizationContext) -> Self {
        Self {
            params: signature
                .params
                .into_iter()
                .map(|var| ExprVarMemberPathSerializable::new(var, &mut ctx.semantic_ctx))
                .collect(),
            extra_rets: signature
                .extra_rets
                .into_iter()
                .map(|var| ExprVarMemberPathSerializable::new(var, &mut ctx.semantic_ctx))
                .collect(),

            return_type: TypeIdSerializable::new(signature.return_type, &mut ctx.semantic_ctx),
            implicits: signature
                .implicits
                .into_iter()
                .map(|ty| TypeIdSerializable::new(ty, &mut ctx.semantic_ctx))
                .collect(),
            panicable: signature.panicable,
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ExprVarMemberPathSerializable {
    Var(ExprVarSerializable),
    Member {
        parent: Box<ExprVarMemberPathSerializable>,
        member_id: (PathSerializable, String),
        concrete_struct_id: ConcreteStructSerializable,
        ty: TypeIdSerializable,
    },
}
impl ExprVarMemberPathSerializable {
    fn new(
        expr_var_member_path: semantic::ExprVarMemberPath,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        match expr_var_member_path {
            semantic::ExprVarMemberPath::Var(var) => {
                ExprVarMemberPathSerializable::Var(ExprVarSerializable::new(var, ctx))
            }
            semantic::ExprVarMemberPath::Member {
                parent,
                member_id,
                stable_ptr: _,
                concrete_struct_id,
                ty,
            } => ExprVarMemberPathSerializable::Member {
                parent: Box::new(ExprVarMemberPathSerializable::new(*parent, ctx)),
                member_id: (
                    PathSerializable::new(member_id.struct_id(ctx.db.upcast()), ctx),
                    member_id.name(ctx.db.upcast()).to_string(),
                ),
                concrete_struct_id: ConcreteStructSerializable::new(concrete_struct_id, ctx),
                ty: TypeIdSerializable::new(ty, ctx),
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ExprVarSerializable {
    var: SemanticVarIdSerializable,
    /// Variable type.
    pub ty: TypeIdSerializable,
    stable_ptr: SyntaxStablePtrIdSerializable,
}
impl ExprVarSerializable {
    fn new(expr_var: semantic::ExprVar, ctx: &mut SemanticSerielizationContext) -> Self {
        Self {
            ty: TypeIdSerializable::new(expr_var.ty, ctx),
            var: SemanticVarIdSerializable::new(expr_var.var, ctx),
            stable_ptr: SyntaxStablePtrIdSerializable::new(
                expr_var.stable_ptr.0,
                expr_var.var.module_file_id(ctx.db.upcast()),
                ctx,
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum SemanticVarIdSerializable {
    Param(SemanticParamIdSerializable),
    Local(SemanticLocalVarIdSerializable),
    Item(SemanticStatementItemIdSerializable),
}
impl SemanticVarIdSerializable {
    fn new(var_id: semantic::VarId, ctx: &mut SemanticSerielizationContext) -> Self {
        match var_id {
            semantic::VarId::Param(id) => {
                SemanticVarIdSerializable::Param(SemanticParamIdSerializable::new(id, ctx))
            }
            semantic::VarId::Local(id) => {
                SemanticVarIdSerializable::Local(SemanticLocalVarIdSerializable::new(id, ctx))
            }
            semantic::VarId::Item(id) => {
                SemanticVarIdSerializable::Item(SemanticStatementItemIdSerializable::new(id, ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticParamIdSerializable {
    pub language_element: LanguageElementSerializable,
}
impl SemanticParamIdSerializable {
    fn new(param_id: semantic::ParamId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(param_id, ctx) }
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticLocalVarIdSerializable {
    pub language_element: LanguageElementSerializable,
}
impl SemanticLocalVarIdSerializable {
    fn new(local_var_id: LocalVarId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(local_var_id, ctx) }
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticStatementItemIdSerializable {
    pub language_element: LanguageElementSerializable,
}

impl SemanticStatementItemIdSerializable {
    fn new(statement_item_id: StatementItemId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(statement_item_id, ctx) }
    }
}

#[derive(Serialize, Deserialize)]
struct VariableSerializable {
    pub droppable: Option<ImplIdSerializable>,
    /// Can the type be (trivially) copied.
    pub copyable: Option<ImplIdSerializable>,
    /// A Destruct impl for the type, if found.
    pub destruct_impl: Option<ImplIdSerializable>,
    /// A PanicDestruct impl for the type, if found.
    pub panic_destruct_impl: Option<ImplIdSerializable>,
    /// Semantic type of the variable.
    pub ty: TypeIdSerializable,
}
impl VariableSerializable {
    fn new(variable: Variable, ctx: &mut SerielizationContext) -> Self {
        Self {
            droppable: variable
                .droppable
                .map(|impl_id| ImplIdSerializable::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            copyable: variable
                .copyable
                .map(|impl_id| ImplIdSerializable::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            destruct_impl: variable
                .destruct_impl
                .map(|impl_id| ImplIdSerializable::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            panic_destruct_impl: variable
                .panic_destruct_impl
                .map(|impl_id| ImplIdSerializable::new(impl_id, &mut ctx.semantic_ctx))
                .ok(),
            ty: TypeIdSerializable::new(variable.ty, &mut ctx.semantic_ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct FlatBlockSerializable {
    /// Statements in the block.
    pub statements: Vec<StatementSerializable>,
    /// Block end.
    pub end: FlatBlockEndSerializable,
}
impl FlatBlockSerializable {
    fn new(flat_block: FlatBlock, ctx: &mut SerielizationContext) -> Self {
        Self {
            statements: flat_block
                .statements
                .into_iter()
                .map(|stmt| StatementSerializable::new(stmt, ctx))
                .collect(),
            end: FlatBlockEndSerializable::new(flat_block.end, ctx),
        }
    }
}
#[derive(Serialize, Deserialize)]
enum FlatBlockEndSerializable {
    /// The block was created but still needs to be populated. Block must not be in this state in
    /// the end of the lowering phase.
    NotSet,
    /// This block ends with a `return` statement, exiting the function.
    Return(Vec<usize>),
    /// This block ends with a panic.
    Panic(usize),
    /// This block ends with a jump to a different block.
    Goto(usize, VarRemappingSerializable),
    Match {
        info: MatchInfoSerializable,
    },
}
impl FlatBlockEndSerializable {
    fn new(flat_block_end: FlatBlockEnd, ctx: &mut SerielizationContext) -> Self {
        match flat_block_end {
            FlatBlockEnd::Return(returns, _location) => FlatBlockEndSerializable::Return(
                returns.iter().map(|var| var.var_id.index()).collect(),
            ),
            FlatBlockEnd::Panic(data) => FlatBlockEndSerializable::Panic(data.var_id.index()),
            FlatBlockEnd::Goto(block_id, remapping) => {
                FlatBlockEndSerializable::Goto(block_id.0, VarRemappingSerializable::new(remapping))
            }
            FlatBlockEnd::NotSet => FlatBlockEndSerializable::NotSet,
            FlatBlockEnd::Match { info } => {
                FlatBlockEndSerializable::Match { info: MatchInfoSerializable::new(info, ctx) }
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct VarRemappingSerializable {
    /// Map from new_var to old_var (since new_var cannot appear twice, but old_var can).
    pub remapping: OrderedHashMap<usize, usize>,
}
impl VarRemappingSerializable {
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
enum MatchInfoSerializable {
    Enum(MatchEnumInfoSerializable),
    Extern(MatchExternInfoSerializable),
    Value(MatchEnumValueSerializable),
}
impl MatchInfoSerializable {
    fn new(match_info: MatchInfo, ctx: &mut SerielizationContext) -> Self {
        match match_info {
            MatchInfo::Enum(info) => {
                MatchInfoSerializable::Enum(MatchEnumInfoSerializable::new(info, ctx))
            }
            MatchInfo::Extern(info) => {
                MatchInfoSerializable::Extern(MatchExternInfoSerializable::new(info, ctx))
            }
            MatchInfo::Value(info) => {
                MatchInfoSerializable::Value(MatchEnumValueSerializable::new(info, ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchEnumInfoSerializable {
    pub concrete_enum_id: ConcreteEnumSerializable,
    /// A living variable in current scope to match on.
    pub input: usize,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<MatchArmSerializable>,
}
impl MatchEnumInfoSerializable {
    fn new(match_enum_info: MatchEnumInfo, ctx: &mut SerielizationContext) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumSerializable::new(
                match_enum_info.concrete_enum_id,
                &mut ctx.semantic_ctx,
            ),
            input: match_enum_info.input.var_id.index(),
            arms: match_enum_info
                .arms
                .into_iter()
                .map(|arm| MatchArmSerializable::new(arm, ctx))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchExternInfoSerializable {
    /// A concrete external function to call.
    pub function: FunctionIdSerializable,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<usize>,
    /// Match arms. All blocks should have the same rets.
    /// Order must be identical to the order in the definition of the enum.
    pub arms: Vec<MatchArmSerializable>,
}

impl MatchExternInfoSerializable {
    fn new(match_extern_info: MatchExternInfo, ctx: &mut SerielizationContext) -> Self {
        Self {
            function: FunctionIdSerializable::new(match_extern_info.function, ctx),
            inputs: match_extern_info.inputs.iter().map(|var| var.var_id.index()).collect(),
            arms: match_extern_info
                .arms
                .into_iter()
                .map(|arm| MatchArmSerializable::new(arm, ctx))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct MatchEnumValueSerializable {
    pub num_of_arms: usize,

    /// A living variable in current scope to match on.
    pub input: usize,
    /// Match arms. All blocks should have the same rets.
    pub arms: Vec<MatchArmSerializable>,
}

impl MatchEnumValueSerializable {
    fn new(match_enum_value: MatchEnumValue, ctx: &mut SerielizationContext) -> Self {
        Self {
            num_of_arms: match_enum_value.num_of_arms,
            input: match_enum_value.input.var_id.index(),
            arms: match_enum_value
                .arms
                .into_iter()
                .map(|arm| MatchArmSerializable::new(arm, ctx))
                .collect(),
        }
    }
}
/// An arm of a match statement.
#[derive(Serialize, Deserialize)]
pub struct MatchArmSerializable {
    /// The selector of the arm.
    arm_selector: MatchArmSelectorSerializable,

    /// The block_id where the relevant arm is implemented.
    pub block_id: usize,

    /// The list of variable ids introduced in this arm.
    pub var_ids: Vec<usize>,
}

impl MatchArmSerializable {
    fn new(match_arm: MatchArm, ctx: &mut SerielizationContext) -> Self {
        Self {
            arm_selector: MatchArmSelectorSerializable::new(
                match_arm.arm_selector,
                &mut ctx.semantic_ctx,
            ),
            block_id: match_arm.block_id.0,
            var_ids: match_arm.var_ids.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum MatchArmSelectorSerializable {
    VariantId(ConcreteVariantSerializable),
    Value(usize),
}

impl MatchArmSelectorSerializable {
    fn new(match_arm_selector: MatchArmSelector, ctx: &mut SemanticSerielizationContext) -> Self {
        match match_arm_selector {
            MatchArmSelector::VariantId(variant_id) => MatchArmSelectorSerializable::VariantId(
                ConcreteVariantSerializable::new(variant_id, ctx),
            ),
            MatchArmSelector::Value(value) => MatchArmSelectorSerializable::Value(value.value),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum StatementSerializable {
    // Values.
    Const(StatementConstSerializable),

    // Flow control.
    Call(StatementCallSerializable),

    // Structs (including tuples).
    StructConstruct(StatementStructConstructSerializable),
    StructDestructure(StatementStructDestructureSerializable),

    // Enums.
    EnumConstruct(StatementEnumConstructSerializable),

    Snapshot(StatementSnapshotSerializable),
    Desnap(StatementDesnapSerializable),
}

impl StatementSerializable {
    fn new(stmt: Statement, ctx: &mut SerielizationContext) -> Self {
        match stmt {
            Statement::Const(stmt) => {
                StatementSerializable::Const(StatementConstSerializable::new(stmt, ctx))
            }
            Statement::Call(stmt) => {
                StatementSerializable::Call(StatementCallSerializable::new(stmt, ctx))
            }
            Statement::StructConstruct(stmt) => StatementSerializable::StructConstruct(
                StatementStructConstructSerializable::new(stmt, ctx),
            ),
            Statement::StructDestructure(stmt) => StatementSerializable::StructDestructure(
                StatementStructDestructureSerializable::new(stmt, ctx),
            ),
            Statement::EnumConstruct(stmt) => StatementSerializable::EnumConstruct(
                StatementEnumConstructSerializable::new(stmt, ctx),
            ),
            Statement::Snapshot(stmt) => {
                StatementSerializable::Snapshot(StatementSnapshotSerializable::new(stmt, ctx))
            }
            Statement::Desnap(stmt) => {
                StatementSerializable::Desnap(StatementDesnapSerializable::new(stmt, ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementConstSerializable {
    /// The value of the const.
    pub value: ConstValueSerializable,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementConstSerializable {
    fn new(stmt: StatementConst, ctx: &mut SerielizationContext) -> Self {
        Self {
            value: ConstValueSerializable::new(stmt.value, &mut ctx.semantic_ctx),
            output: stmt.output.index(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum ConstValueSerializable {
    Int(BigInt, TypeIdSerializable),
    Struct(Vec<ConstValueSerializable>, TypeIdSerializable),
    Enum(ConcreteVariantSerializable, Box<ConstValueSerializable>),
    NonZero(Box<ConstValueSerializable>),
    Boxed(Box<ConstValueSerializable>),
    Generic(GenericParamSerializable),
    ImplConstant(ImplConstantSerializable),
}
impl ConstValueSerializable {
    fn new(const_value_id: ConstValue, ctx: &mut SemanticSerielizationContext) -> Self {
        match const_value_id {
            ConstValue::Int(value, ty) => {
                ConstValueSerializable::Int(value, TypeIdSerializable::new(ty, ctx))
            }
            ConstValue::Struct(values, ty) => ConstValueSerializable::Struct(
                values.into_iter().map(|v| ConstValueSerializable::new(v, ctx)).collect(),
                TypeIdSerializable::new(ty, ctx),
            ),
            ConstValue::Enum(variant, value) => ConstValueSerializable::Enum(
                ConcreteVariantSerializable::new(variant, ctx),
                Box::new(ConstValueSerializable::new(*value, ctx)),
            ),
            ConstValue::NonZero(value) => {
                ConstValueSerializable::NonZero(Box::new(ConstValueSerializable::new(*value, ctx)))
            }
            ConstValue::Boxed(value) => {
                ConstValueSerializable::Boxed(Box::new(ConstValueSerializable::new(*value, ctx)))
            }
            ConstValue::Generic(generic_param) => {
                ConstValueSerializable::Generic(GenericParamSerializable::new(generic_param, ctx))
            }
            ConstValue::ImplConstant(impl_constant_id) => ConstValueSerializable::ImplConstant(
                ImplConstantSerializable::new(impl_constant_id, ctx),
            ),
            _ => {
                println!("{:?}", const_value_id.debug(ctx.db.elongate()));
                todo!()
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ImplConstantSerializable {
    impl_id: ImplIdSerializable,
    trait_constant: TraitConstantSerializable,
}
impl ImplConstantSerializable {
    fn new(impl_constant_id: ImplConstantId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self {
            impl_id: ImplIdSerializable::new(impl_constant_id.impl_id(), ctx),
            trait_constant: TraitConstantSerializable::new(
                impl_constant_id.trait_constant_id(),
                ctx,
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct TraitConstantSerializable {
    language_element: PathSerializable,
}
impl TraitConstantSerializable {
    fn new(trait_constant_id: TraitConstantId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { language_element: PathSerializable::new(trait_constant_id, ctx) }
    }
}

#[derive(Serialize, Deserialize)]
struct ConstStatementSerializable {
    /// Value of the constant.
    pub value: i32,
}

#[derive(Serialize, Deserialize)]
struct StatementCallSerializable {
    /// A function to "call".
    pub function: FunctionIdSerializable,
    /// Living variables in current scope to move to the function, as arguments.
    pub inputs: Vec<usize>,
    /// Is the last input a coupon for the function call. See
    /// [semantic::ExprFunctionCall::coupon_arg] for more information.
    pub with_coupon: bool,
    /// New variables to be introduced into the current scope from the function outputs.
    pub outputs: Vec<usize>,
}
impl StatementCallSerializable {
    fn new(stmt: StatementCall, ctx: &mut SerielizationContext) -> Self {
        Self {
            function: FunctionIdSerializable::new(stmt.function, ctx),
            inputs: stmt.inputs.iter().map(|var| var.var_id.index()).collect(),
            with_coupon: stmt.with_coupon,
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum FunctionSerializable {
    /// An original function from the user code.
    Semantic(SemanticFunctionIdSerializable),
    /// A function generated by the compiler.
    Generated(GeneratedFunctionSerializable),
}
impl FunctionSerializable {
    fn new(function: FunctionLongId, ctx: &mut SerielizationContext) -> Self {
        match function {
            FunctionLongId::Semantic(id) => FunctionSerializable::Semantic(
                SemanticFunctionIdSerializable::new(id, &mut ctx.semantic_ctx),
            ),
            FunctionLongId::Generated(id) => {
                FunctionSerializable::Generated(GeneratedFunctionSerializable::new(id, ctx))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
struct FunctionIdSerializable(usize);
impl FunctionIdSerializable {
    fn new(function_id: FunctionId, ctx: &mut SerielizationContext) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = FunctionSerializable::new(function_id.lookup_intern(ctx.db), ctx);
        let id = FunctionIdSerializable(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticFunctionSerializable {
    generic_function: GenericFunctionSerializable,

    generic_args: Vec<GenericArgumentSerializable>,
}
impl SemanticFunctionSerializable {
    fn new(function_id: semantic::FunctionLongId, ctx: &mut SemanticSerielizationContext) -> Self {
        let function = function_id.function;
        Self {
            generic_function: GenericFunctionSerializable::new(function.generic_function, ctx),
            generic_args: function
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone)]
struct SemanticFunctionIdSerializable(usize);
impl SemanticFunctionIdSerializable {
    fn new(function_id: semantic::FunctionId, ctx: &mut SemanticSerielizationContext) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = SemanticFunctionSerializable::new(function_id.lookup_intern(ctx.db), ctx);
        let id = SemanticFunctionIdSerializable(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
}

#[derive(Serialize, Deserialize)]
enum GenericFunctionSerializable {
    /// A generic free function.
    Free(PathSerializable),
    /// A generic extern function.
    Extern(PathSerializable),
    /// A generic function of an impl.
    Impl(ImplIdSerializable, String),
}
impl GenericFunctionSerializable {
    fn new(generic_function: GenericFunctionId, ctx: &mut SemanticSerielizationContext) -> Self {
        match generic_function {
            GenericFunctionId::Free(id) => {
                GenericFunctionSerializable::Free(PathSerializable::new(id, ctx))
            }
            GenericFunctionId::Extern(id) => {
                GenericFunctionSerializable::Extern(PathSerializable::new(id, ctx))
            }
            GenericFunctionId::Impl(id) => GenericFunctionSerializable::Impl(
                ImplIdSerializable::new(id.impl_id, ctx),
                id.function.name(ctx.db.upcast()).to_string(),
            ),
            GenericFunctionId::Trait(_id_) => {
                panic!("Trait functions are not supported in serialization")
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct GeneratedFunctionSerializable {
    parent: SemanticConcreteFunctionWithBodySerializable,
    key: GeneratedFunctionKeySerializable,
}
impl GeneratedFunctionSerializable {
    fn new(function: GeneratedFunction, ctx: &mut SerielizationContext) -> Self {
        Self {
            parent: SemanticConcreteFunctionWithBodySerializable::new(
                function.parent,
                &mut ctx.semantic_ctx,
            ),
            key: GeneratedFunctionKeySerializable::new(function.key, ctx),
        }
    }
}
#[derive(Serialize, Deserialize)]
struct SemanticConcreteFunctionWithBodySerializable {
    pub generic_function: GenericFunctionWithBodySerializable,
    pub generic_args: Vec<GenericArgumentSerializable>,
}
impl SemanticConcreteFunctionWithBodySerializable {
    fn new(
        function_id: semantic::ConcreteFunctionWithBodyId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        Self {
            generic_function: GenericFunctionWithBodySerializable::new(
                function_id.generic_function(ctx.db),
                ctx,
            ),
            generic_args: function_id
                .lookup_intern(ctx.db)
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum GenericFunctionWithBodySerializable {
    Free(PathSerializable),
    Impl(ConcreteImplSerializable, String),
}

impl GenericFunctionWithBodySerializable {
    fn new(
        generic_function: GenericFunctionWithBodyId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        match generic_function {
            GenericFunctionWithBodyId::Free(id) => {
                GenericFunctionWithBodySerializable::Free(PathSerializable::new(id, ctx))
            }
            GenericFunctionWithBodyId::Impl(id) => GenericFunctionWithBodySerializable::Impl(
                ConcreteImplSerializable::new(id.concrete_impl_id, ctx),
                id.function_body.name(ctx.db.upcast()).into(),
            ),
            GenericFunctionWithBodyId::Trait(_id) => {
                unreachable!("Trait functions are not supported in serialization")
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
enum GeneratedFunctionKeySerializable {
    Loop(usize),
    TraitFunc(SemanticFunctionIdSerializable),
}

impl GeneratedFunctionKeySerializable {
    fn new(key: GeneratedFunctionKey, ctx: &mut SerielizationContext) -> Self {
        match key {
            GeneratedFunctionKey::Loop(id) => GeneratedFunctionKeySerializable::Loop(id.index()),
            GeneratedFunctionKey::TraitFunc(id, _) => GeneratedFunctionKeySerializable::TraitFunc(
                SemanticFunctionIdSerializable::new(id, &mut ctx.semantic_ctx),
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementStructConstructSerializable {
    pub inputs: Vec<usize>,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementStructConstructSerializable {
    fn new(stmt: StatementStructConstruct, _ctx: &mut SerielizationContext) -> Self {
        Self {
            inputs: stmt.inputs.iter().map(|var| var.var_id.index()).collect(),
            output: stmt.output.index(),
        }
    }
}
#[derive(Serialize, Deserialize)]
struct StatementStructDestructureSerializable {
    /// A living variable in current scope to destructure.
    pub input: usize,
    /// The variables to bind values to.
    pub outputs: Vec<usize>,
}
impl StatementStructDestructureSerializable {
    fn new(stmt: StatementStructDestructure, _ctx: &mut SerielizationContext) -> Self {
        Self {
            input: stmt.input.var_id.index(),
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementEnumConstructSerializable {
    pub variant: ConcreteVariantSerializable,
    /// A living variable in current scope to wrap with the variant.
    pub input: usize,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementEnumConstructSerializable {
    fn new(stmt: StatementEnumConstruct, ctx: &mut SerielizationContext) -> Self {
        Self {
            variant: ConcreteVariantSerializable::new(stmt.variant, &mut ctx.semantic_ctx),
            input: stmt.input.var_id.index(),
            output: stmt.output.index(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementSnapshotSerializable {
    pub input: usize,
    outputs: [usize; 2],
}
impl StatementSnapshotSerializable {
    fn new(stmt: StatementSnapshot, _ctx: &mut SerielizationContext) -> Self {
        Self { input: stmt.input.var_id.index(), outputs: stmt.outputs.map(|var| var.index()) }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementDesnapSerializable {
    pub input: usize,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementDesnapSerializable {
    fn new(stmt: StatementDesnap, _ctx: &mut SerielizationContext) -> Self {
        Self { input: stmt.input.var_id.index(), output: stmt.output.index() }
    }
}

#[derive(Serialize, Deserialize)]
enum GenericArgumentSerializable {
    Type(TypeIdSerializable),
    Value(ConstValueSerializable),
    Impl(ImplIdSerializable),
    NegImpl,
}

impl GenericArgumentSerializable {
    fn new(
        generic_argument_id: semantic::GenericArgumentId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        match generic_argument_id {
            semantic::GenericArgumentId::Type(type_id) => {
                GenericArgumentSerializable::Type(TypeIdSerializable::new(type_id, ctx))
            }
            semantic::GenericArgumentId::Constant(const_value_id) => {
                GenericArgumentSerializable::Value(ConstValueSerializable::new(
                    const_value_id.lookup_intern(ctx.db), // todo intern
                    ctx,
                ))
            }
            semantic::GenericArgumentId::Impl(impl_id) => {
                GenericArgumentSerializable::Impl(ImplIdSerializable::new(impl_id, ctx))
            }
            semantic::GenericArgumentId::NegImpl => GenericArgumentSerializable::NegImpl,
        }
    }
}

#[derive(Serialize, Deserialize)]
enum TypeSerializable {
    Concrete(ConcreteTypeSerializable),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeIdSerializable>),
    Snapshot(Box<TypeIdSerializable>),
    GenericParameter(GenericParamSerializable),
    ImplType(ImplTypeSerializable),
    FixedSizeArray(TypeIdSerializable, ConstValueSerializable),
}

impl TypeSerializable {
    fn new(type_id: TypeLongId, ctx: &mut SemanticSerielizationContext) -> Self {
        match type_id {
            semantic::TypeLongId::Concrete(concrete_type_id) => {
                TypeSerializable::Concrete(ConcreteTypeSerializable::new(concrete_type_id, ctx))
            }
            semantic::TypeLongId::Tuple(vec) => TypeSerializable::Tuple(
                vec.into_iter().map(|ty| TypeIdSerializable::new(ty, ctx)).collect(),
            ),
            semantic::TypeLongId::Snapshot(type_id) => {
                TypeSerializable::Snapshot(Box::new(TypeIdSerializable::new(type_id, ctx)))
            }
            semantic::TypeLongId::GenericParameter(generic_param_id) => {
                TypeSerializable::GenericParameter(GenericParamSerializable::new(
                    generic_param_id,
                    ctx,
                ))
            }
            semantic::TypeLongId::ImplType(impl_type_id) => {
                TypeSerializable::ImplType(ImplTypeSerializable::new(impl_type_id, ctx))
            }
            semantic::TypeLongId::FixedSizeArray { type_id, size } => {
                TypeSerializable::FixedSizeArray(
                    TypeIdSerializable::new(type_id, ctx),
                    ConstValueSerializable::new(size.lookup_intern(ctx.db), ctx),
                )
            }
            _ => {
                println!("failed to serielize type {:?}", type_id.debug(ctx.db.elongate()));
                todo!()
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
struct TypeIdSerializable(usize);

impl TypeIdSerializable {
    fn new(ty: TypeId, ctx: &mut SemanticSerielizationContext) -> Self {
        if let Some(id) = ctx.type_ids.get(&ty) {
            return *id;
        }
        let ty_long = TypeSerializable::new(ty.lookup_intern(ctx.db), ctx);
        let id = TypeIdSerializable(ctx.type_ids_lookup.len());
        ctx.type_ids_lookup.push(ty_long);
        ctx.type_ids.insert(ty, id);
        id
    }
}

#[derive(Serialize, Deserialize)]
enum ConcreteTypeSerializable {
    Struct(ConcreteStructSerializable),
    Enum(ConcreteEnumSerializable),
    Extern(PathSerializable, Vec<GenericArgumentSerializable>),
}

impl ConcreteTypeSerializable {
    fn new(
        concrete_type_id: semantic::ConcreteTypeId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        match concrete_type_id {
            semantic::ConcreteTypeId::Struct(id) => {
                ConcreteTypeSerializable::Struct(ConcreteStructSerializable::new(id, ctx))
            }
            semantic::ConcreteTypeId::Enum(id) => {
                ConcreteTypeSerializable::Enum(ConcreteEnumSerializable::new(id, ctx))
            }
            semantic::ConcreteTypeId::Extern(_) => ConcreteTypeSerializable::Extern(
                PathSerializable::new(concrete_type_id.generic_type(ctx.db), ctx),
                concrete_type_id
                    .generic_args(ctx.db)
                    .into_iter()
                    .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                    .collect(),
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ImplTypeSerializable {
    impl_id: ImplIdSerializable,
    trait_type: TraitTypeSerializable,
}
impl ImplTypeSerializable {
    fn new(impl_type_id: ImplTypeId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self {
            impl_id: ImplIdSerializable::new(impl_type_id.impl_id(), ctx),
            trait_type: TraitTypeSerializable::new(impl_type_id.ty(), ctx),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct TraitTypeSerializable {
    language_element: PathSerializable,
}
impl TraitTypeSerializable {
    fn new(trait_type_id: TraitTypeId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { language_element: PathSerializable::new(trait_type_id, ctx) }
    }
}

#[derive(Serialize, Deserialize)]
enum ImplSerializable {
    Concrete(ConcreteImplSerializable),
    GenericParameter(GenericParamSerializable),
}
impl ImplSerializable {
    fn new(impl_id: ImplLongId, ctx: &mut SemanticSerielizationContext) -> Self {
        match impl_id {
            ImplLongId::Concrete(concrete_impl) => {
                ImplSerializable::Concrete(ConcreteImplSerializable::new(concrete_impl, ctx))
            }
            ImplLongId::GenericParameter(generic_param_id) => ImplSerializable::GenericParameter(
                GenericParamSerializable::new(generic_param_id, ctx),
            ),
            _ => todo!(),
        }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone)]
struct ImplIdSerializable(usize);

impl ImplIdSerializable {
    fn new(impl_id: ImplId, ctx: &mut SemanticSerielizationContext) -> Self {
        if let Some(id) = ctx.impl_ids.get(&impl_id) {
            return *id;
        }
        let imp = ImplSerializable::new(impl_id.lookup_intern(ctx.db), ctx);
        let id = ImplIdSerializable(ctx.impl_ids_lookup.len());
        ctx.impl_ids_lookup.push(imp);
        ctx.impl_ids.insert(impl_id, id);
        id
    }
}

#[derive(Serialize, Deserialize)]
struct ConcreteImplSerializable {
    pub impl_def_id: PathSerializable,
    pub generic_args: Vec<GenericArgumentSerializable>,
}
impl ConcreteImplSerializable {
    fn new(
        concrete_impl: semantic::ConcreteImplId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        let long_id = concrete_impl.lookup_intern(ctx.db);
        Self {
            impl_def_id: PathSerializable::new(long_id.impl_def_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct GenericParamSerializable {
    pub language_element: LanguageElementSerializable,
}
impl GenericParamSerializable {
    fn new(generic_param_id: GenericParamId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(generic_param_id, ctx) }
    }
}

#[derive(Serialize, Deserialize)]
struct ConcreteVariantSerializable {
    pub concrete_enum_id: ConcreteEnumSerializable,
    pub id: String,
    pub ty: TypeIdSerializable,
    /// The index of the variant from within the variant list.
    pub idx: usize,
}
impl ConcreteVariantSerializable {
    fn new(
        concrete_variant: semantic::ConcreteVariant,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumSerializable::new(concrete_variant.concrete_enum_id, ctx),
            id: concrete_variant.id.name(ctx.db.upcast()).to_string(),
            ty: TypeIdSerializable::new(concrete_variant.ty, ctx),
            idx: concrete_variant.idx,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ConcreteEnumSerializable {
    enum_id: PathSerializable,
    generic_args: Vec<GenericArgumentSerializable>,
}

impl ConcreteEnumSerializable {
    fn new(
        concrete_enum: semantic::ConcreteEnumId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        let long_id = concrete_enum.lookup_intern(ctx.db);
        Self {
            enum_id: PathSerializable::new(long_id.enum_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ConcreteStructSerializable {
    struct_id: PathSerializable,
    generic_args: Vec<GenericArgumentSerializable>,
}
impl ConcreteStructSerializable {
    fn new(
        concrete_struct: semantic::ConcreteStructId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        let long_id = concrete_struct.lookup_intern(ctx.db);
        Self {
            struct_id: PathSerializable::new(long_id.struct_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct ModuleFileSerializable {
    module: String,
    file_index: usize,
}
impl ModuleFileSerializable {
    fn new(module_file_id: ModuleFileId, ctx: &mut SemanticSerielizationContext) -> Self {
        Self { module: module_file_id.0.full_path(ctx.db.upcast()), file_index: module_file_id.1.0 }
    }
}

#[derive(Serialize, Deserialize)]
struct PathSerializable {
    pub path: Vec<String>,
}

impl PathSerializable {
    fn new<T: NamedLanguageElementId>(
        language_element: T,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        let mut path = vec![language_element.name(ctx.db.upcast()).to_string()];

        fn push_parents<T: NamedLanguageElementId>(
            language_element: T,
            ctx: &mut SemanticSerielizationContext,
            path: &mut Vec<String>,
        ) {
            let parent = language_element.parent_module(ctx.db.upcast());
            match parent {
                ModuleId::CrateRoot(crate_id) => {
                    path.push(crate_id.name(ctx.db.upcast()).to_string());
                }
                ModuleId::Submodule(submodule_id) => {
                    path.push(submodule_id.name(ctx.db.upcast()).to_string());
                    push_parents(submodule_id, ctx, path);
                }
            }
        }

        push_parents(language_element, ctx, &mut path);
        Self { path }
    }
}

#[derive(Serialize, Deserialize)]
struct LanguageElementSerializable {
    module_file_id: ModuleFileSerializable,
    stable_ptr: SyntaxStablePtrIdSerializable,
}
impl LanguageElementSerializable {
    fn new<T: LanguageElementId>(
        language_element: T,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        Self {
            module_file_id: ModuleFileSerializable::new(
                language_element.module_file_id(ctx.db.upcast()),
                ctx,
            ),
            stable_ptr: SyntaxStablePtrIdSerializable::new(
                language_element.untyped_stable_ptr(ctx.db.upcast()),
                language_element.module_file_id(ctx.db.upcast()),
                ctx,
            ),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum SyntaxStablePtrSerializable {
    /// The root node of the tree.
    /// use the module_id for serialization.
    Root(String, GreenIdSerializable),
    /// A child node.
    Child {
        /// The parent of the node.
        parent: SyntaxStablePtrIdSerializable,
        /// The SyntaxKind of the node.
        kind: SyntaxKind,
        /// A list of field values for this node, to index by.
        /// Which fields are used is determined by each SyntaxKind.
        /// For example, a function item might use the name of the function.
        key_fields: Vec<GreenIdSerializable>,
        /// Chronological index among all nodes with the same (parent, kind, key_fields).
        index: usize,
    },
}

impl SyntaxStablePtrSerializable {
    fn new(
        syntax_stable_ptr: SyntaxStablePtr,
        module_file_id: ModuleFileId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        match syntax_stable_ptr {
            SyntaxStablePtr::Root(root, green_id) => {
                assert!(Ok(root) == module_file_id.file_id(ctx.db.upcast()));
                SyntaxStablePtrSerializable::Root(
                    module_file_id.0.full_path(ctx.db.upcast()),
                    GreenIdSerializable::new(green_id, ctx),
                )
            }
            SyntaxStablePtr::Child { parent, kind, key_fields, index } => {
                SyntaxStablePtrSerializable::Child {
                    parent: SyntaxStablePtrIdSerializable::new(parent, module_file_id, ctx),
                    kind,
                    key_fields: key_fields
                        .into_iter()
                        .map(|field| GreenIdSerializable::new(field, ctx))
                        .collect(),
                    index,
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
struct SyntaxStablePtrIdSerializable(usize);
impl SyntaxStablePtrIdSerializable {
    fn new(
        syntax_stable_ptr_id: SyntaxStablePtrId,
        module_file_id: ModuleFileId,
        ctx: &mut SemanticSerielizationContext,
    ) -> Self {
        if let Some(id) = ctx.syntax_stable_ptr_ids.get(&syntax_stable_ptr_id) {
            return *id;
        }
        let stable_ptr = SyntaxStablePtrSerializable::new(
            syntax_stable_ptr_id.lookup_intern(ctx.db),
            module_file_id,
            ctx,
        );
        let id = SyntaxStablePtrIdSerializable(ctx.syntax_stable_ptr_ids_lookup.len());
        ctx.syntax_stable_ptr_ids_lookup.push(stable_ptr);
        ctx.syntax_stable_ptr_ids.insert(syntax_stable_ptr_id, id);
        id
    }
}

#[derive(Serialize, Deserialize)]
enum GreenNodeDetailsSerializable {
    Token(SmolStr),
    Node { children: Vec<GreenIdSerializable>, width: TextWidth },
}

impl GreenNodeDetailsSerializable {
    fn new(
        green_node_details: &GreenNodeDetails,
        ctx: &mut SemanticSerielizationContext,
    ) -> GreenNodeDetailsSerializable {
        match green_node_details {
            GreenNodeDetails::Token(token) => GreenNodeDetailsSerializable::Token(token.clone()),
            GreenNodeDetails::Node { children, width } => GreenNodeDetailsSerializable::Node {
                children: children
                    .into_iter()
                    .map(|child| GreenIdSerializable::new(*child, ctx))
                    .collect(),
                width: *width,
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct GreenNodeSerializable {
    pub kind: SyntaxKind,
    details: GreenNodeDetailsSerializable,
}
impl GreenNodeSerializable {
    fn new(green_node: &GreenNode, ctx: &mut SemanticSerielizationContext) -> Self {
        Self {
            kind: green_node.kind,
            details: GreenNodeDetailsSerializable::new(&green_node.details, ctx),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
struct GreenIdSerializable(usize);

impl GreenIdSerializable {
    fn new(green_id: GreenId, ctx: &mut SemanticSerielizationContext) -> Self {
        if let Some(id) = ctx.green_ids.get(&green_id) {
            return *id;
        }
        let green_node = GreenNodeSerializable::new(green_id.lookup_intern(ctx.db).as_ref(), ctx);
        let id = GreenIdSerializable(ctx.green_ids_lookup.len());
        ctx.green_ids_lookup.push(green_node);
        ctx.green_ids.insert(green_id, id);
        id
    }
}
