use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    EnumLongId, ExternFunctionLongId, ExternTypeLongId, FileIndex, FreeFunctionLongId,
    GenericParamId, GenericParamLongId, ImplDefId, ImplDefLongId, ImplItemId, LanguageElementId,
    LocalVarId, LocalVarLongId, MemberLongId, ModuleFileId, ModuleId, NamedLanguageElementId,
    ParamLongId, StatementConstLongId, StatementItemId, StatementUseLongId, StructLongId,
    SubmoduleId, SubmoduleLongId, TraitConstantId, TraitConstantLongId, TraitTypeId,
    TraitTypeLongId,
};
use cairo_lang_diagnostics::skip_diagnostic;
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::constant::{ConstValue, ImplConstantId};
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionId, GenericFunctionWithBodyId, ImplFunctionBodyId,
    ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use cairo_lang_semantic::items::imp::{ImplId, ImplLongId};
use cairo_lang_semantic::types::{
    ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId, ImplTypeId,
};
use cairo_lang_semantic::{
    ConcreteFunction, ConcreteImplLongId, MatchArmSelector, TypeId, TypeLongId, ValueSelectorArm,
};
use cairo_lang_syntax::node::ast::{
    ExprPtr, FunctionWithBodyPtr, GenericParamPtr, ItemConstantPtr, ItemEnumPtr,
    ItemExternFunctionPtr, ItemExternTypePtr, ItemImplPtr, ItemModulePtr, ItemStructPtr, MemberPtr,
    ParamPtr, TerminalIdentifierPtr, TraitItemConstantPtr, TraitItemTypePtr, UsePathLeafPtr,
};
use cairo_lang_syntax::node::green::{GreenNode, GreenNodeDetails};
use cairo_lang_syntax::node::ids::{GreenId, SyntaxStablePtrId};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use id_arena::Arena;
use itertools::Itertools;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::blocks::FlatBlocksBuilder;
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

struct DesSerializationContext<'db> {
    variables_id: Vec<VariableId>,
    db: &'db dyn LoweringGroup,

    default_location: LocationId,

    data: DeserializationData,

    semantic_ctx: SemanticDeserializationContext<'db>,
}

impl Deref for DesSerializationContext<'_> {
    type Target = DeserializationData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for DesSerializationContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
struct DeserializationData {
    function_ids: OrderedHashMap<FunctionIdSerializable, FunctionId>,
    lookups: SerializationLookups,
}
impl Deref for DeserializationData {
    type Target = SerializationLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for DeserializationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

pub struct SerializationContext<'db> {
    db: &'db dyn LoweringGroup,
    data: SerializationData,
    semantic_ctx: SemanticSerializationContext<'db>,
}
impl Deref for SerializationContext<'_> {
    type Target = SerializationData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SerializationContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
impl<'db> SerializationContext<'db> {
    pub fn new(db: &'db dyn LoweringGroup) -> Self {
        Self {
            db,
            data: SerializationData::default(),
            semantic_ctx: SemanticSerializationContext {
                db: db.upcast(),
                data: SemanticSerializationData::default(),
            },
        }
    }
}

#[derive(Default)]
pub struct SerializationData {
    function_ids: OrderedHashMap<FunctionId, FunctionIdSerializable>,
    lookups: SerializationLookups,
}
impl Deref for SerializationData {
    type Target = SerializationLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SerializationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct SerializationLookups {
    function_ids_lookup: Vec<FunctionSerializable>,
}

struct SemanticDeserializationContext<'db> {
    db: &'db dyn SemanticGroup,
    data: SemanticDeserializationData,
}

impl Deref for SemanticDeserializationContext<'_> {
    type Target = SemanticDeserializationData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SemanticDeserializationContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

struct SemanticDeserializationData {
    function_ids: OrderedHashMap<SemanticFunctionIdSerializable, semantic::FunctionId>,
    type_ids: OrderedHashMap<TypeIdSerializable, TypeId>,
    impl_ids: OrderedHashMap<ImplIdSerializable, ImplId>,
    green_ids: OrderedHashMap<GreenIdSerializable, GreenId>,
    syntax_stable_ptr_ids: OrderedHashMap<SyntaxStablePtrIdSerializable, SyntaxStablePtrId>,
    crate_ids: OrderedHashMap<CrateIdSerializable, CrateId>,
    submodule_ids: OrderedHashMap<SubmoduleIdSerializable, SubmoduleId>,
    lookups: SemanticSerializationLookups,
}

impl Deref for SemanticDeserializationData {
    type Target = SemanticSerializationLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SemanticDeserializationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

pub struct SemanticSerializationContext<'db> {
    db: &'db dyn SemanticGroup,
    data: SemanticSerializationData,
}
impl Deref for SemanticSerializationContext<'_> {
    type Target = SemanticSerializationData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for SemanticSerializationContext<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Default)]
pub struct SemanticSerializationData {
    function_ids: OrderedHashMap<semantic::FunctionId, SemanticFunctionIdSerializable>,

    type_ids: OrderedHashMap<TypeId, TypeIdSerializable>,

    impl_ids: OrderedHashMap<ImplId, ImplIdSerializable>,

    green_ids: OrderedHashMap<GreenId, GreenIdSerializable>,
    crate_ids: OrderedHashMap<CrateId, CrateIdSerializable>,
    submodule_ids: OrderedHashMap<SubmoduleId, SubmoduleIdSerializable>,

    syntax_stable_ptr_ids: OrderedHashMap<SyntaxStablePtrId, SyntaxStablePtrIdSerializable>,
    lookups: SemanticSerializationLookups,
}

impl Deref for SemanticSerializationData {
    type Target = SemanticSerializationLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}
impl DerefMut for SemanticSerializationData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lookups
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct SemanticSerializationLookups {
    function_ids_lookup: Vec<SemanticFunctionSerializable>,
    type_ids_lookup: Vec<TypeSerializable>,
    impl_ids_lookup: Vec<ImplSerializable>,
    green_ids_lookup: Vec<GreenNodeSerializable>,
    crate_ids_lookup: Vec<CrateSerializable>,
    syntax_stable_ptr_ids_lookup: Vec<SyntaxStablePtrSerializable>,
    submodule_ids_lookup: Vec<SubmoduleSerializable>,
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
    pub fn new(flat_lowered: FlatLowered, ctx: &mut SerializationContext) -> Self {
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
    pub fn embed(self, ctx: &mut DesSerializationContext) -> Result<FlatLowered, ()> {
        ctx.variables_id.clear();
        let mut variables = Arena::new();
        for var in self.variables {
            let id = variables.alloc(var.embed(ctx)?);
            ctx.variables_id.push(id);
        }

        let mut blocks = FlatBlocksBuilder::new();
        for block in self.blocks {
            blocks.alloc(block.embed(ctx)?);
        }
        Ok(FlatLowered {
            diagnostics: Default::default(),
            signature: self.signature.embed(ctx)?,
            variables,
            blocks: blocks.build().unwrap(),
            parameters: self
                .parameters
                .into_iter()
                .map(|var_id| ctx.variables_id[var_id])
                .collect(),
        })
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
    fn new(signature: Signature, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<Signature, ()> {
        Ok(Signature {
            params: self
                .params
                .into_iter()
                .map(|var| var.embed(&mut ctx.semantic_ctx))
                .collect::<Result<_, _>>()?,
            extra_rets: self
                .extra_rets
                .into_iter()
                .map(|var| var.embed(&mut ctx.semantic_ctx))
                .collect::<Result<_, _>>()?,
            return_type: self.return_type.embed(&mut ctx.semantic_ctx)?,
            implicits: self
                .implicits
                .into_iter()
                .map(|ty| ty.embed(&mut ctx.semantic_ctx))
                .collect::<Result<_, _>>()?,
            panicable: self.panicable,
            location: ctx.default_location,
        })
    }
}

#[derive(Serialize, Deserialize)]
enum ExprVarMemberPathSerializable {
    Var(ExprVarSerializable),
    Member {
        parent: Box<ExprVarMemberPathSerializable>,
        member_id: LanguageElementSerializable,
        concrete_struct_id: ConcreteStructSerializable,
        stable_ptr: SyntaxStablePtrIdSerializable,
        ty: TypeIdSerializable,
    },
}
impl ExprVarMemberPathSerializable {
    fn new(
        expr_var_member_path: semantic::ExprVarMemberPath,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        match expr_var_member_path {
            semantic::ExprVarMemberPath::Var(var) => {
                ExprVarMemberPathSerializable::Var(ExprVarSerializable::new(var, ctx))
            }
            semantic::ExprVarMemberPath::Member {
                parent,
                member_id,
                concrete_struct_id,
                stable_ptr,
                ty,
            } => {
                let module_file_id = parent.base_var().module_file_id(ctx.db.upcast()); // assume the module_file_is is the same as the parent's
                ExprVarMemberPathSerializable::Member {
                    parent: Box::new(ExprVarMemberPathSerializable::new(*parent, ctx)),
                    member_id: LanguageElementSerializable::new(member_id, ctx),
                    concrete_struct_id: ConcreteStructSerializable::new(concrete_struct_id, ctx),
                    stable_ptr: SyntaxStablePtrIdSerializable::new(
                        stable_ptr.0,
                        module_file_id,
                        ctx,
                    ),
                    ty: TypeIdSerializable::new(ty, ctx),
                }
            }
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ExprVarMemberPath, ()> {
        Ok(match self {
            ExprVarMemberPathSerializable::Var(var) => {
                semantic::ExprVarMemberPath::Var(var.embed(ctx)?)
            }
            ExprVarMemberPathSerializable::Member {
                parent,
                member_id,
                concrete_struct_id,
                stable_ptr,
                ty,
            } => {
                let parent = Box::new(parent.embed(ctx)?);
                let (moddule_file_id, member_stable_ptr) = member_id.embed(ctx)?;
                let member_id =
                    MemberLongId(moddule_file_id, MemberPtr(member_stable_ptr)).intern(ctx.db);
                semantic::ExprVarMemberPath::Member {
                    parent,
                    member_id,
                    concrete_struct_id: concrete_struct_id.embed(ctx)?,
                    stable_ptr: ExprPtr(stable_ptr.embed(ctx)),
                    ty: ty.embed(ctx)?,
                }
            }
        })
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
    fn new(expr_var: semantic::ExprVar, ctx: &mut SemanticSerializationContext) -> Self {
        Self {
            var: SemanticVarIdSerializable::new(expr_var.var, ctx),
            ty: TypeIdSerializable::new(expr_var.ty, ctx),
            stable_ptr: SyntaxStablePtrIdSerializable::new(
                expr_var.stable_ptr.0,
                expr_var.var.module_file_id(ctx.db.upcast()),
                ctx,
            ),
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<semantic::ExprVar, ()> {
        Ok(semantic::ExprVar {
            var: self.var.embed(ctx)?,
            ty: self.ty.embed(ctx)?,
            stable_ptr: ExprPtr(self.stable_ptr.embed(ctx)),
        })
    }
}

#[derive(Serialize, Deserialize)]
enum SemanticVarIdSerializable {
    Param(SemanticParamIdSerializable),
    Local(SemanticLocalVarIdSerializable),
    Item(SemanticStatementItemIdSerializable),
}
impl SemanticVarIdSerializable {
    fn new(var_id: semantic::VarId, ctx: &mut SemanticSerializationContext) -> Self {
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
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<semantic::VarId, ()> {
        Ok(match self {
            SemanticVarIdSerializable::Param(id) => semantic::VarId::Param(id.embed(ctx)?),
            SemanticVarIdSerializable::Local(id) => semantic::VarId::Local(id.embed(ctx)?),
            SemanticVarIdSerializable::Item(id) => semantic::VarId::Item(id.embed(ctx)?),
        })
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticParamIdSerializable {
    pub language_element: LanguageElementSerializable,
}
impl SemanticParamIdSerializable {
    fn new(param_id: semantic::ParamId, ctx: &mut SemanticSerializationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(param_id, ctx) }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<semantic::ParamId, ()> {
        let (module_id, stable_ptr) = self.language_element.embed(ctx)?;
        Ok(ParamLongId(module_id, ParamPtr(stable_ptr)).intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize)]
struct SemanticLocalVarIdSerializable {
    pub language_element: LanguageElementSerializable,
}
impl SemanticLocalVarIdSerializable {
    fn new(local_var_id: LocalVarId, ctx: &mut SemanticSerializationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(local_var_id, ctx) }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<LocalVarId, ()> {
        let (module_id, stable_ptr) = self.language_element.embed(ctx)?;
        Ok(LocalVarLongId(module_id, TerminalIdentifierPtr(stable_ptr)).intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize)]
enum SemanticStatementItemIdSerializable {
    Constant(LanguageElementSerializable),
    Use(LanguageElementSerializable),
}

impl SemanticStatementItemIdSerializable {
    fn new(item_id: StatementItemId, ctx: &mut SemanticSerializationContext) -> Self {
        match item_id {
            StatementItemId::Constant(id) => SemanticStatementItemIdSerializable::Constant(
                LanguageElementSerializable::new(id, ctx),
            ),
            StatementItemId::Use(id) => {
                SemanticStatementItemIdSerializable::Use(LanguageElementSerializable::new(id, ctx))
            }
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<StatementItemId, ()> {
        Ok(match self {
            SemanticStatementItemIdSerializable::Constant(id) => {
                let (module_id, stable_ptr) = id.embed(ctx)?;
                StatementItemId::Constant(
                    StatementConstLongId(module_id, ItemConstantPtr(stable_ptr)).intern(ctx.db),
                )
            }
            SemanticStatementItemIdSerializable::Use(id) => {
                let (module_id, stable_ptr) = id.embed(ctx)?;
                StatementItemId::Use(
                    StatementUseLongId(module_id, UsePathLeafPtr(stable_ptr)).intern(ctx.db),
                )
            }
        })
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
    fn new(variable: Variable, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<Variable, ()> {
        Ok(Variable {
            droppable: self
                .droppable
                .and_then(|impl_id| impl_id.embed(&mut ctx.semantic_ctx).ok())
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            copyable: self
                .copyable
                .and_then(|impl_id| impl_id.embed(&mut ctx.semantic_ctx).ok())
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            destruct_impl: self
                .destruct_impl
                .and_then(|impl_id| impl_id.embed(&mut ctx.semantic_ctx).ok())
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            panic_destruct_impl: self
                .panic_destruct_impl
                .and_then(|impl_id| impl_id.embed(&mut ctx.semantic_ctx).ok())
                .ok_or(InferenceError::Reported(skip_diagnostic())),
            ty: self.ty.embed(&mut ctx.semantic_ctx)?,
            location: ctx.default_location,
        })
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
    fn new(flat_block: FlatBlock, ctx: &mut SerializationContext) -> Self {
        Self {
            statements: flat_block
                .statements
                .into_iter()
                .map(|stmt| StatementSerializable::new(stmt, ctx))
                .collect(),
            end: FlatBlockEndSerializable::new(flat_block.end, ctx),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<FlatBlock, ()> {
        Ok(FlatBlock {
            statements: self
                .statements
                .into_iter()
                .map(|stmt| stmt.embed(ctx))
                .collect::<Result<_, _>>()?,
            end: self.end.embed(ctx)?,
        })
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
    fn new(flat_block_end: FlatBlockEnd, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<FlatBlockEnd, ()> {
        Ok(match self {
            FlatBlockEndSerializable::Return(returns) => FlatBlockEnd::Return(
                returns
                    .into_iter()
                    .map(|var_id| VarUsage {
                        var_id: ctx.variables_id[var_id],
                        location: ctx.default_location,
                    })
                    .collect(),
                ctx.default_location,
            ),
            FlatBlockEndSerializable::Panic(var_id) => FlatBlockEnd::Panic(VarUsage {
                var_id: ctx.variables_id[var_id],
                location: ctx.default_location,
            }),
            FlatBlockEndSerializable::Goto(block_id, remapping) => {
                FlatBlockEnd::Goto(BlockId(block_id), remapping.embed(ctx))
            }
            FlatBlockEndSerializable::NotSet => FlatBlockEnd::NotSet,
            FlatBlockEndSerializable::Match { info } => {
                FlatBlockEnd::Match { info: info.embed(ctx)? }
            }
        })
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
    fn embed(self, ctx: &DesSerializationContext) -> VarRemapping {
        let mut remapping = OrderedHashMap::default();
        for (dst, src) in self.remapping {
            remapping.insert(ctx.variables_id[dst], VarUsage {
                var_id: ctx.variables_id[src],
                location: ctx.default_location,
            });
        }
        VarRemapping { remapping }
    }
}

#[derive(Serialize, Deserialize)]
enum MatchInfoSerializable {
    Enum(MatchEnumInfoSerializable),
    Extern(MatchExternInfoSerializable),
    Value(MatchEnumValueSerializable),
}
impl MatchInfoSerializable {
    fn new(match_info: MatchInfo, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<MatchInfo, ()> {
        Ok(match self {
            MatchInfoSerializable::Enum(info) => MatchInfo::Enum(info.embed(ctx)?),
            MatchInfoSerializable::Extern(info) => MatchInfo::Extern(info.embed(ctx).unwrap()),
            MatchInfoSerializable::Value(info) => MatchInfo::Value(info.embed(ctx)),
        })
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
    fn new(match_enum_info: MatchEnumInfo, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<MatchEnumInfo, ()> {
        Ok(MatchEnumInfo {
            concrete_enum_id: self.concrete_enum_id.embed(&mut ctx.semantic_ctx)?,
            input: VarUsage {
                var_id: ctx.variables_id[self.input],
                location: ctx.default_location,
            },
            arms: self.arms.into_iter().map(|arm| arm.embed(ctx)).collect(),
            location: ctx.default_location,
        })
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
    fn new(match_extern_info: MatchExternInfo, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<MatchExternInfo, ()> {
        Ok(MatchExternInfo {
            function: self.function.embed(ctx)?,
            inputs: self
                .inputs
                .into_iter()
                .map(|var_id| VarUsage {
                    var_id: ctx.variables_id[var_id],
                    location: ctx.default_location,
                })
                .collect(),
            arms: self.arms.into_iter().map(|arm| arm.embed(ctx)).collect(),
            location: ctx.default_location,
        })
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
    fn new(match_enum_value: MatchEnumValue, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> MatchEnumValue {
        MatchEnumValue {
            num_of_arms: self.num_of_arms,
            input: VarUsage {
                var_id: ctx.variables_id[self.input],
                location: ctx.default_location,
            },
            arms: self.arms.into_iter().map(|arm| arm.embed(ctx)).collect(),
            location: ctx.default_location,
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
    fn new(match_arm: MatchArm, ctx: &mut SerializationContext) -> Self {
        Self {
            arm_selector: MatchArmSelectorSerializable::new(
                match_arm.arm_selector,
                &mut ctx.semantic_ctx,
            ),
            block_id: match_arm.block_id.0,
            var_ids: match_arm.var_ids.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> MatchArm {
        MatchArm {
            arm_selector: self.arm_selector.embed(ctx).unwrap(),
            block_id: BlockId(self.block_id),
            var_ids: self.var_ids.into_iter().map(|var_id| ctx.variables_id[var_id]).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum MatchArmSelectorSerializable {
    VariantId(ConcreteVariantSerializable),
    Value(usize),
}

impl MatchArmSelectorSerializable {
    fn new(match_arm_selector: MatchArmSelector, ctx: &mut SemanticSerializationContext) -> Self {
        match match_arm_selector {
            MatchArmSelector::VariantId(variant_id) => MatchArmSelectorSerializable::VariantId(
                ConcreteVariantSerializable::new(variant_id, ctx),
            ),
            MatchArmSelector::Value(value) => MatchArmSelectorSerializable::Value(value.value),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<MatchArmSelector, ()> {
        Ok(match self {
            MatchArmSelectorSerializable::VariantId(variant_id) => {
                MatchArmSelector::VariantId(variant_id.embed(&mut ctx.semantic_ctx)?)
            }
            MatchArmSelectorSerializable::Value(value) => {
                MatchArmSelector::Value(ValueSelectorArm { value })
            }
        })
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
    fn new(stmt: Statement, ctx: &mut SerializationContext) -> Self {
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
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<Statement, ()> {
        match self {
            StatementSerializable::Const(stmt) => Ok(Statement::Const(stmt.embed(ctx)?)),
            StatementSerializable::Call(stmt) => Ok(Statement::Call(stmt.embed(ctx)?)),
            StatementSerializable::StructConstruct(stmt) => {
                Ok(Statement::StructConstruct(stmt.embed(ctx)))
            }
            StatementSerializable::StructDestructure(stmt) => {
                Ok(Statement::StructDestructure(stmt.embed(ctx)))
            }
            StatementSerializable::EnumConstruct(stmt) => {
                Ok(Statement::EnumConstruct(stmt.embed(ctx)?))
            }
            StatementSerializable::Snapshot(stmt) => Ok(Statement::Snapshot(stmt.embed(ctx))),
            StatementSerializable::Desnap(stmt) => Ok(Statement::Desnap(stmt.embed(ctx))),
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
    fn new(stmt: StatementConst, ctx: &mut SerializationContext) -> Self {
        Self {
            value: ConstValueSerializable::new(stmt.value, &mut ctx.semantic_ctx),
            output: stmt.output.index(),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<StatementConst, ()> {
        Ok(StatementConst {
            value: self.value.embed(&mut ctx.semantic_ctx)?,
            output: ctx.variables_id[self.output],
        })
    }
}

#[derive(Serialize, Deserialize, Clone)]
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
    fn new(const_value_id: ConstValue, ctx: &mut SemanticSerializationContext) -> Self {
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
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ConstValue, ()> {
        match self {
            ConstValueSerializable::Int(value, ty) => Ok(ConstValue::Int(value, ty.embed(ctx)?)),
            ConstValueSerializable::Struct(values, ty) => Ok(ConstValue::Struct(
                values.into_iter().map(|v| v.embed(ctx)).collect::<Result<_, _>>()?,
                ty.embed(ctx)?,
            )),
            ConstValueSerializable::Enum(variant, value) => {
                Ok(ConstValue::Enum(variant.embed(ctx)?, Box::new(value.embed(ctx)?)))
            }
            ConstValueSerializable::NonZero(value) => {
                Ok(ConstValue::NonZero(Box::new(value.embed(ctx)?)))
            }
            ConstValueSerializable::Boxed(value) => {
                Ok(ConstValue::Boxed(Box::new(value.embed(ctx)?)))
            }
            ConstValueSerializable::Generic(generic_param) => {
                Ok(ConstValue::Generic(generic_param.embed(ctx)?))
            }
            ConstValueSerializable::ImplConstant(impl_constant_id) => {
                Ok(ConstValue::ImplConstant(impl_constant_id.embed(ctx)?))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ImplConstantSerializable {
    impl_id: ImplIdSerializable,
    trait_constant: TraitConstantSerializable,
}
impl ImplConstantSerializable {
    fn new(impl_constant_id: ImplConstantId, ctx: &mut SemanticSerializationContext) -> Self {
        Self {
            impl_id: ImplIdSerializable::new(impl_constant_id.impl_id(), ctx),
            trait_constant: TraitConstantSerializable::new(
                impl_constant_id.trait_constant_id(),
                ctx,
            ),
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ImplConstantId, ()> {
        Ok(ImplConstantId::new(self.impl_id.embed(ctx)?, self.trait_constant.embed(ctx)?, ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct TraitConstantSerializable {
    language_element: LanguageElementSerializable,
}
impl TraitConstantSerializable {
    fn new(trait_constant_id: TraitConstantId, ctx: &mut SemanticSerializationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(trait_constant_id, ctx) }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<TraitConstantId, ()> {
        let (module_id, stable_ptr) = self.language_element.embed(ctx)?;
        Ok(TraitConstantLongId(module_id, TraitItemConstantPtr(stable_ptr)).intern(ctx.db))
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
    fn new(stmt: StatementCall, ctx: &mut SerializationContext) -> Self {
        Self {
            function: FunctionIdSerializable::new(stmt.function, ctx),
            inputs: stmt.inputs.iter().map(|var| var.var_id.index()).collect(),
            with_coupon: stmt.with_coupon,
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<StatementCall, ()> {
        Ok(StatementCall {
            function: self.function.embed(ctx)?,
            inputs: self
                .inputs
                .into_iter()
                .map(|var_id| VarUsage {
                    var_id: ctx.variables_id[var_id],
                    location: ctx.default_location,
                })
                .collect(),
            with_coupon: self.with_coupon,
            outputs: self.outputs.into_iter().map(|var_id| ctx.variables_id[var_id]).collect(),
            location: ctx.default_location,
        })
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum FunctionSerializable {
    /// An original function from the user code.
    Semantic(SemanticFunctionIdSerializable),
    /// A function generated by the compiler.
    Generated(GeneratedFunctionSerializable),
}
impl FunctionSerializable {
    fn new(function: FunctionLongId, ctx: &mut SerializationContext) -> Self {
        match function {
            FunctionLongId::Semantic(id) => FunctionSerializable::Semantic(
                SemanticFunctionIdSerializable::new(id, &mut ctx.semantic_ctx),
            ),
            FunctionLongId::Generated(id) => {
                FunctionSerializable::Generated(GeneratedFunctionSerializable::new(id, ctx))
            }
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<FunctionId, ()> {
        Ok(match self {
            FunctionSerializable::Semantic(id) => {
                FunctionLongId::Semantic(id.embed(&mut ctx.semantic_ctx)?)
            }
            FunctionSerializable::Generated(id) => FunctionLongId::Generated(id.embed(ctx)?),
        }
        .intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct FunctionIdSerializable(usize);
impl FunctionIdSerializable {
    fn new(function_id: FunctionId, ctx: &mut SerializationContext) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = FunctionSerializable::new(function_id.lookup_intern(ctx.db), ctx);
        let id = FunctionIdSerializable(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<FunctionId, ()> {
        if let Some(function_id) = ctx.function_ids.get(&self) {
            return Ok(*function_id);
        }

        let function = ctx.function_ids_lookup[self.0].clone();
        let function_id = function.embed(ctx)?;
        ctx.function_ids.insert(self, function_id);
        Ok(function_id)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct SemanticFunctionSerializable {
    generic_function: GenericFunctionSerializable,

    generic_args: Vec<GenericArgumentSerializable>,
}
impl SemanticFunctionSerializable {
    fn new(function_id: semantic::FunctionLongId, ctx: &mut SemanticSerializationContext) -> Self {
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
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::FunctionLongId, ()> {
        let long_id = semantic::FunctionLongId {
            function: ConcreteFunction {
                generic_function: self.generic_function.embed(ctx).map_err(|_| ())?,
                generic_args: self
                    .generic_args
                    .into_iter()
                    .map(|arg| arg.embed(ctx))
                    .collect::<Result<_, _>>()
                    .map_err(|_| ())?,
            },
        };
        Ok(long_id)
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct SemanticFunctionIdSerializable(usize);
impl SemanticFunctionIdSerializable {
    fn new(function_id: semantic::FunctionId, ctx: &mut SemanticSerializationContext) -> Self {
        if let Some(id) = ctx.function_ids.get(&function_id) {
            return *id;
        }
        let function = SemanticFunctionSerializable::new(function_id.lookup_intern(ctx.db), ctx);
        let id = SemanticFunctionIdSerializable(ctx.function_ids_lookup.len());
        ctx.function_ids_lookup.push(function);
        ctx.function_ids.insert(function_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<semantic::FunctionId, ()> {
        if let Some(function_id) = ctx.function_ids.get(&self) {
            return Ok(*function_id);
        }

        let function = ctx.function_ids_lookup[self.0].clone();
        let function_id = function.embed(ctx)?.intern(ctx.db);
        ctx.function_ids.insert(self, function_id);
        Ok(function_id)
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericFunctionSerializable {
    /// A generic free function.
    Free(LanguageElementSerializable),
    /// A generic extern function.
    Extern(LanguageElementSerializable),
    /// A generic function of an impl.
    Impl(ImplIdSerializable, String),
}
impl GenericFunctionSerializable {
    fn new(generic_function: GenericFunctionId, ctx: &mut SemanticSerializationContext) -> Self {
        match generic_function {
            GenericFunctionId::Free(id) => {
                GenericFunctionSerializable::Free(LanguageElementSerializable::new(id, ctx))
            }
            GenericFunctionId::Extern(id) => {
                GenericFunctionSerializable::Extern(LanguageElementSerializable::new(id, ctx))
            }
            GenericFunctionId::Impl(id) => GenericFunctionSerializable::Impl(
                ImplIdSerializable::new(id.impl_id, ctx),
                id.function.name(ctx.db.upcast()).into(),
            ),
            _ => {
                unreachable!("Trait functions are not supported in serialization")
            }
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<GenericFunctionId, ()> {
        match self {
            GenericFunctionSerializable::Free(id) => {
                let (module_id, stable_ptr) = id.embed(ctx)?;
                let id =
                    FreeFunctionLongId(module_id, FunctionWithBodyPtr(stable_ptr)).intern(ctx.db);
                Ok(GenericFunctionId::Free(id))
            }
            GenericFunctionSerializable::Extern(id) => {
                let (module_id, stable_ptr) = id.embed(ctx)?;
                let id = ExternFunctionLongId(module_id, ItemExternFunctionPtr(stable_ptr))
                    .intern(ctx.db);
                Ok(GenericFunctionId::Extern(id))
            }
            GenericFunctionSerializable::Impl(id, name) => {
                let impl_id = id.embed(ctx)?;
                let Ok(Some(trait_function_id)) = ctx.db.trait_function_by_name(
                    impl_id
                        .concrete_trait(ctx.db)
                        .map(|concrete_trait| concrete_trait.trait_id(ctx.db))
                        .map_err(|_| ())?,
                    name.into(),
                ) else {
                    return Err(());
                };
                Ok(GenericFunctionId::Impl(ImplGenericFunctionId {
                    impl_id,
                    function: trait_function_id,
                }))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct GeneratedFunctionSerializable {
    parent: SemanticConcreteFunctionWithBodySerializable,
    key: GeneratedFunctionKeySerializable,
}
impl GeneratedFunctionSerializable {
    fn new(function: GeneratedFunction, ctx: &mut SerializationContext) -> Self {
        Self {
            parent: SemanticConcreteFunctionWithBodySerializable::new(
                function.parent,
                &mut ctx.semantic_ctx,
            ),
            key: GeneratedFunctionKeySerializable::new(function.key, ctx),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<GeneratedFunction, ()> {
        Ok(GeneratedFunction {
            parent: self.parent.embed(&mut ctx.semantic_ctx)?,
            key: self.key.embed(ctx)?,
        })
    }
}
#[derive(Serialize, Deserialize, Clone)]
struct SemanticConcreteFunctionWithBodySerializable {
    pub generic_function: GenericFunctionWithBodySerializable,
    pub generic_args: Vec<GenericArgumentSerializable>,
}
impl SemanticConcreteFunctionWithBodySerializable {
    fn new(
        function_id: semantic::ConcreteFunctionWithBodyId,
        ctx: &mut SemanticSerializationContext,
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
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteFunctionWithBodyId, ()> {
        let generic_function = self.generic_function.embed(ctx)?;
        let generic_args =
            self.generic_args.into_iter().map(|arg| arg.embed(ctx)).collect::<Result<_, _>>()?;
        Ok(ConcreteFunctionWithBody { generic_function, generic_args }.intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericFunctionWithBodySerializable {
    Free(LanguageElementSerializable),
    Impl(ConcreteImplSerializable, String),
}

impl GenericFunctionWithBodySerializable {
    fn new(
        generic_function: GenericFunctionWithBodyId,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        match generic_function {
            GenericFunctionWithBodyId::Free(id) => {
                GenericFunctionWithBodySerializable::Free(LanguageElementSerializable::new(id, ctx))
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
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<GenericFunctionWithBodyId, ()> {
        match self {
            GenericFunctionWithBodySerializable::Free(id) => {
                let (module_id, stable_ptr) = id.embed(ctx)?;
                let id =
                    FreeFunctionLongId(module_id, FunctionWithBodyPtr(stable_ptr)).intern(ctx.db);
                Ok(GenericFunctionWithBodyId::Free(id))
            }
            GenericFunctionWithBodySerializable::Impl(id, name) => {
                // todo handle trait functions
                let impl_id = id.embed(ctx)?;
                let Some(ImplItemId::Function(function_id)) = ctx
                    .db
                    .impl_item_by_name(impl_id.impl_def_id(ctx.db), name.into())
                    .map_err(|_| ())?
                else {
                    return Err(());
                };
                Ok(GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                    concrete_impl_id: impl_id,
                    function_body: ImplFunctionBodyId::Impl(function_id),
                }))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GeneratedFunctionKeySerializable {
    Loop(usize),
    TraitFunc(SemanticFunctionIdSerializable),
}

impl GeneratedFunctionKeySerializable {
    fn new(key: GeneratedFunctionKey, ctx: &mut SerializationContext) -> Self {
        match key {
            GeneratedFunctionKey::Loop(id) => GeneratedFunctionKeySerializable::Loop(id.index()),
            GeneratedFunctionKey::TraitFunc(id, _) => GeneratedFunctionKeySerializable::TraitFunc(
                SemanticFunctionIdSerializable::new(id, &mut ctx.semantic_ctx),
            ),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<GeneratedFunctionKey, ()> {
        let expr_arena: Arena<semantic::Expr> = Default::default();

        Ok(match self {
            GeneratedFunctionKeySerializable::Loop(_id) => {
                GeneratedFunctionKey::Loop(expr_arena.next_id())
            }
            GeneratedFunctionKeySerializable::TraitFunc(id) => GeneratedFunctionKey::TraitFunc(
                id.embed(&mut ctx.semantic_ctx)?,
                ctx.default_location.lookup_intern(ctx.db).stable_location,
            ),
        })
    }
}

#[derive(Serialize, Deserialize)]
struct StatementStructConstructSerializable {
    pub inputs: Vec<usize>,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementStructConstructSerializable {
    fn new(stmt: StatementStructConstruct, _ctx: &mut SerializationContext) -> Self {
        Self {
            inputs: stmt.inputs.iter().map(|var| var.var_id.index()).collect(),
            output: stmt.output.index(),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> StatementStructConstruct {
        StatementStructConstruct {
            inputs: self
                .inputs
                .into_iter()
                .map(|var_id| VarUsage {
                    var_id: ctx.variables_id[var_id],
                    location: ctx.default_location,
                })
                .collect(),
            output: ctx.variables_id[self.output],
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
    fn new(stmt: StatementStructDestructure, _ctx: &mut SerializationContext) -> Self {
        Self {
            input: stmt.input.var_id.index(),
            outputs: stmt.outputs.iter().map(|var| var.index()).collect(),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> StatementStructDestructure {
        StatementStructDestructure {
            input: VarUsage {
                var_id: ctx.variables_id[self.input],
                location: ctx.default_location,
            },
            outputs: self.outputs.into_iter().map(|var_id| ctx.variables_id[var_id]).collect(),
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
    fn new(stmt: StatementEnumConstruct, ctx: &mut SerializationContext) -> Self {
        Self {
            variant: ConcreteVariantSerializable::new(stmt.variant, &mut ctx.semantic_ctx),
            input: stmt.input.var_id.index(),
            output: stmt.output.index(),
        }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> Result<StatementEnumConstruct, ()> {
        Ok(StatementEnumConstruct {
            variant: self.variant.embed(&mut ctx.semantic_ctx)?,
            input: VarUsage {
                var_id: ctx.variables_id[self.input],
                location: ctx.default_location,
            },
            output: ctx.variables_id[self.output],
        })
    }
}

#[derive(Serialize, Deserialize)]
struct StatementSnapshotSerializable {
    pub input: usize,
    outputs: [usize; 2],
}
impl StatementSnapshotSerializable {
    fn new(stmt: StatementSnapshot, _ctx: &mut SerializationContext) -> Self {
        Self { input: stmt.input.var_id.index(), outputs: stmt.outputs.map(|var| var.index()) }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> StatementSnapshot {
        StatementSnapshot {
            input: VarUsage {
                var_id: ctx.variables_id[self.input],
                location: ctx.default_location,
            },
            outputs: [ctx.variables_id[self.outputs[0]], ctx.variables_id[self.outputs[1]]],
        }
    }
}

#[derive(Serialize, Deserialize)]
struct StatementDesnapSerializable {
    pub input: usize,
    /// The variable to bind the value to.
    pub output: usize,
}
impl StatementDesnapSerializable {
    fn new(stmt: StatementDesnap, _ctx: &mut SerializationContext) -> Self {
        Self { input: stmt.input.var_id.index(), output: stmt.output.index() }
    }
    fn embed(self, ctx: &mut DesSerializationContext) -> StatementDesnap {
        StatementDesnap {
            input: VarUsage {
                var_id: ctx.variables_id[self.input],
                location: ctx.default_location,
            },
            output: ctx.variables_id[self.output],
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GenericArgumentSerializable {
    Type(TypeIdSerializable),
    Value(ConstValueSerializable),
    Impl(ImplIdSerializable),
    NegImpl,
}

impl GenericArgumentSerializable {
    fn new(
        generic_argument_id: semantic::GenericArgumentId,
        ctx: &mut SemanticSerializationContext,
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
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::GenericArgumentId, ()> {
        Ok(match self {
            GenericArgumentSerializable::Type(ty) => {
                semantic::GenericArgumentId::Type(ty.embed(ctx)?)
            }
            GenericArgumentSerializable::Value(value) => {
                semantic::GenericArgumentId::Constant(value.embed(ctx)?.intern(ctx.db))
            }
            GenericArgumentSerializable::Impl(imp) => {
                semantic::GenericArgumentId::Impl(imp.embed(ctx)?)
            }
            GenericArgumentSerializable::NegImpl => semantic::GenericArgumentId::NegImpl,
        })
    }
}

#[derive(Serialize, Deserialize, Clone)]
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
    fn new(type_id: TypeLongId, ctx: &mut SemanticSerializationContext) -> Self {
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
                println!("failed to serialize type {:?}", type_id.debug(ctx.db.elongate()));
                todo!()
            }
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<TypeLongId, ()> {
        Ok(match self {
            TypeSerializable::Concrete(concrete_type) => {
                TypeLongId::Concrete(concrete_type.embed(ctx)?)
            }
            TypeSerializable::Tuple(vec) => TypeLongId::Tuple(
                vec.into_iter().map(|ty| ty.embed(ctx)).collect::<Result<_, _>>()?,
            ),
            TypeSerializable::Snapshot(type_id) => TypeLongId::Snapshot(type_id.embed(ctx)?),
            TypeSerializable::GenericParameter(generic_param) => {
                TypeLongId::GenericParameter(generic_param.embed(ctx)?)
            }
            TypeSerializable::ImplType(impl_type) => TypeLongId::ImplType(impl_type.embed(ctx)?),
            TypeSerializable::FixedSizeArray(type_id, size) => TypeLongId::FixedSizeArray {
                type_id: type_id.embed(ctx)?,
                size: size.embed(ctx)?.intern(ctx.db),
            },
        })
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct TypeIdSerializable(usize);

impl TypeIdSerializable {
    fn new(ty: TypeId, ctx: &mut SemanticSerializationContext) -> Self {
        if let Some(id) = ctx.type_ids.get(&ty) {
            return *id;
        }
        let ty_long = TypeSerializable::new(ty.lookup_intern(ctx.db), ctx);
        let id = TypeIdSerializable(ctx.type_ids_lookup.len());
        ctx.type_ids_lookup.push(ty_long);
        ctx.type_ids.insert(ty, id);
        id
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<TypeId, ()> {
        if let Some(type_id) = ctx.type_ids.get(&self) {
            return Ok(*type_id);
        }

        let ty = ctx.type_ids_lookup[self.0].clone();
        let ty = ty.embed(ctx)?.intern(ctx.db);
        ctx.type_ids.insert(self, ty);
        Ok(ty)
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ConcreteTypeSerializable {
    Struct(ConcreteStructSerializable),
    Enum(ConcreteEnumSerializable),
    Extern(ConcreteExternTypeSerializable),
}

impl ConcreteTypeSerializable {
    fn new(
        concrete_type_id: semantic::ConcreteTypeId,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        match concrete_type_id {
            semantic::ConcreteTypeId::Struct(id) => {
                ConcreteTypeSerializable::Struct(ConcreteStructSerializable::new(id, ctx))
            }
            semantic::ConcreteTypeId::Enum(id) => {
                ConcreteTypeSerializable::Enum(ConcreteEnumSerializable::new(id, ctx))
            }
            semantic::ConcreteTypeId::Extern(id) => {
                ConcreteTypeSerializable::Extern(ConcreteExternTypeSerializable::new(id, ctx))
            }
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteTypeId, ()> {
        Ok(match self {
            ConcreteTypeSerializable::Struct(s) => semantic::ConcreteTypeId::Struct(s.embed(ctx)?),
            ConcreteTypeSerializable::Enum(e) => semantic::ConcreteTypeId::Enum(e.embed(ctx)?),
            ConcreteTypeSerializable::Extern(e) => semantic::ConcreteTypeId::Extern(e.embed(ctx)?),
        })
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ImplTypeSerializable {
    impl_id: ImplIdSerializable,
    trait_type: TraitTypeSerializable,
}
impl ImplTypeSerializable {
    fn new(impl_type_id: ImplTypeId, ctx: &mut SemanticSerializationContext) -> Self {
        Self {
            impl_id: ImplIdSerializable::new(impl_type_id.impl_id(), ctx),
            trait_type: TraitTypeSerializable::new(impl_type_id.ty(), ctx),
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ImplTypeId, ()> {
        let impl_id = self.impl_id.embed(ctx)?;
        let ty = self.trait_type.embed(ctx)?;
        Ok(ImplTypeId::new(impl_id, ty, ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct TraitTypeSerializable {
    language_element: LanguageElementSerializable,
}
impl TraitTypeSerializable {
    fn new(trait_type_id: TraitTypeId, ctx: &mut SemanticSerializationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(trait_type_id, ctx) }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<TraitTypeId, ()> {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx)?;
        Ok(TraitTypeLongId(module_file_id, TraitItemTypePtr(stable_ptr)).intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ImplSerializable {
    Concrete(ConcreteImplSerializable),
    GenericParameter(GenericParamSerializable),
}
impl ImplSerializable {
    fn new(impl_id: ImplLongId, ctx: &mut SemanticSerializationContext) -> Self {
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
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ImplLongId, ()> {
        Ok(match self {
            ImplSerializable::Concrete(concrete_impl) => {
                ImplLongId::Concrete(concrete_impl.embed(ctx)?)
            }
            ImplSerializable::GenericParameter(generic_param) => {
                ImplLongId::GenericParameter(generic_param.embed(ctx)?)
            }
        })
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct ImplIdSerializable(usize);

impl ImplIdSerializable {
    fn new(impl_id: ImplId, ctx: &mut SemanticSerializationContext) -> Self {
        if let Some(id) = ctx.impl_ids.get(&impl_id) {
            return *id;
        }
        let imp = ImplSerializable::new(impl_id.lookup_intern(ctx.db), ctx);
        let id = ImplIdSerializable(ctx.impl_ids_lookup.len());
        ctx.impl_ids_lookup.push(imp);
        ctx.impl_ids.insert(impl_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ImplId, ()> {
        if let Some(impl_id) = ctx.impl_ids.get(&self) {
            return Ok(*impl_id);
        }

        let imp = ctx.impl_ids_lookup[self.0].clone();
        let imp = imp.embed(ctx)?.intern(ctx.db);
        ctx.impl_ids.insert(self, imp);
        Ok(imp)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteImplSerializable {
    pub impl_def_id: ImplDefIdSerializable,
    pub generic_args: Vec<GenericArgumentSerializable>,
}
impl ConcreteImplSerializable {
    fn new(
        concrete_impl: semantic::ConcreteImplId,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        let long_id = concrete_impl.lookup_intern(ctx.db);
        Self {
            impl_def_id: ImplDefIdSerializable::new(long_id.impl_def_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteImplId, ()> {
        let impl_def_id = self.impl_def_id.embed(ctx)?;
        let long_id = ConcreteImplLongId {
            impl_def_id,
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.embed(ctx))
                .collect::<Result<_, _>>()?,
        };
        Ok(long_id.intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ImplDefIdSerializable {
    language_element: LanguageElementSerializable,
}
impl ImplDefIdSerializable {
    fn new(impl_def_id: ImplDefId, ctx: &mut SemanticSerializationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(impl_def_id, ctx) }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ImplDefId, ()> {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx)?;
        Ok(ImplDefLongId(module_file_id, ItemImplPtr(stable_ptr)).intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct GenericParamSerializable {
    pub language_element: LanguageElementSerializable,
}
impl GenericParamSerializable {
    fn new(generic_param_id: GenericParamId, ctx: &mut SemanticSerializationContext) -> Self {
        Self { language_element: LanguageElementSerializable::new(generic_param_id, ctx) }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<GenericParamId, ()> {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx)?;
        Ok(GenericParamLongId(module_file_id, GenericParamPtr(stable_ptr)).intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
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
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        Self {
            concrete_enum_id: ConcreteEnumSerializable::new(concrete_variant.concrete_enum_id, ctx),
            id: concrete_variant.id.name(ctx.db.upcast()).to_string(),
            ty: TypeIdSerializable::new(concrete_variant.ty, ctx),
            idx: concrete_variant.idx,
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteVariant, ()> {
        let concrete_enum_id = self.concrete_enum_id.embed(ctx)?;
        let ty = self.ty.embed(ctx)?;
        let id = *ctx
            .db
            .enum_variants(concrete_enum_id.enum_id(ctx.db))
            .unwrap()
            .get(self.id.as_str())
            .unwrap();
        Ok(semantic::ConcreteVariant { concrete_enum_id, id, ty, idx: self.idx })
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ConcreteEnumSerializable {
    enum_id: LanguageElementSerializable,
    generic_args: Vec<GenericArgumentSerializable>,
}

impl ConcreteEnumSerializable {
    fn new(
        concrete_enum: semantic::ConcreteEnumId,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        let long_id = concrete_enum.lookup_intern(ctx.db);
        Self {
            enum_id: LanguageElementSerializable::new(long_id.enum_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteEnumId, ()> {
        let (module_file_id, stable_ptr) = self.enum_id.embed(ctx)?;

        let long_id = ConcreteEnumLongId {
            enum_id: EnumLongId(module_file_id, ItemEnumPtr(stable_ptr)).intern(ctx.db),
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.embed(ctx))
                .collect::<Result<_, _>>()?,
        };
        Ok(long_id.intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ConcreteStructSerializable {
    struct_id: LanguageElementSerializable,
    generic_args: Vec<GenericArgumentSerializable>,
}
impl ConcreteStructSerializable {
    fn new(
        concrete_struct: semantic::ConcreteStructId,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        let long_id = concrete_struct.lookup_intern(ctx.db);
        Self {
            struct_id: LanguageElementSerializable::new(long_id.struct_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteStructId, ()> {
        let (module_file_id, stable_ptr) = self.struct_id.embed(ctx)?;

        let long_id = ConcreteStructLongId {
            struct_id: StructLongId(module_file_id, ItemStructPtr(stable_ptr)).intern(ctx.db),
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.embed(ctx))
                .collect::<Result<_, _>>()?,
        };
        Ok(long_id.intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ConcreteExternTypeSerializable {
    pub language_element: LanguageElementSerializable,
    pub generic_args: Vec<GenericArgumentSerializable>,
}
impl ConcreteExternTypeSerializable {
    fn new(
        concrete_extern_type: semantic::ConcreteExternTypeId,
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        let long_id = concrete_extern_type.lookup_intern(ctx.db);
        Self {
            language_element: LanguageElementSerializable::new(long_id.extern_type_id, ctx),
            generic_args: long_id
                .generic_args
                .into_iter()
                .map(|arg| GenericArgumentSerializable::new(arg, ctx))
                .collect(),
        }
    }
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<semantic::ConcreteExternTypeId, ()> {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx)?;

        let long_id = ConcreteExternTypeLongId {
            extern_type_id: ExternTypeLongId(module_file_id, ItemExternTypePtr(stable_ptr))
                .intern(ctx.db),
            generic_args: self
                .generic_args
                .into_iter()
                .map(|arg| arg.embed(ctx))
                .collect::<Result<_, _>>()?,
        };
        Ok(long_id.intern(ctx.db))
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct ModuleFileSerializable {
    module: ModuleIdSerializable,
    file_index: usize,
}
impl ModuleFileSerializable {
    fn new(module_file_id: ModuleFileId, ctx: &mut SemanticSerializationContext) -> Self {
        Self {
            module: ModuleIdSerializable::new(module_file_id.0, ctx),
            file_index: module_file_id.1.0,
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ModuleFileId, ()> {
        Ok(ModuleFileId(self.module.embed(ctx)?, FileIndex(self.file_index)))
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum ModuleIdSerializable {
    CrateRoot(CrateIdSerializable),
    Submodule(SubmoduleIdSerializable),
}
impl ModuleIdSerializable {
    fn new(module_id: ModuleId, ctx: &mut SemanticSerializationContext) -> Self {
        match module_id {
            ModuleId::CrateRoot(crate_id) => {
                ModuleIdSerializable::CrateRoot(CrateIdSerializable::new(crate_id, ctx))
            }
            ModuleId::Submodule(submodule_id) => {
                ModuleIdSerializable::Submodule(SubmoduleIdSerializable::new(submodule_id, ctx))
            }
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<ModuleId, ()> {
        match self {
            ModuleIdSerializable::CrateRoot(crate_id) => {
                Ok(ModuleId::CrateRoot(crate_id.embed(ctx)))
            }
            ModuleIdSerializable::Submodule(submodule_id) => {
                Ok(ModuleId::Submodule(submodule_id.embed(ctx)?))
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct CrateSerializable {
    name: SmolStr,
    discriminator: Option<SmolStr>,
}
impl CrateSerializable {
    fn new(crate_id: CrateLongId, _ctx: &mut SemanticSerializationContext) -> Self {
        let CrateLongId::Real { name, discriminator } = crate_id else {
            panic!("crate must be real");
        };
        Self { name, discriminator }
    }
    fn embed(self, _ctx: &mut SemanticDeserializationContext) -> CrateLongId {
        CrateLongId::Real { name: self.name, discriminator: self.discriminator }
    }
}
#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct CrateIdSerializable(usize);
impl CrateIdSerializable {
    fn new(crate_id: CrateId, ctx: &mut SemanticSerializationContext) -> Self {
        if let Some(id) = ctx.crate_ids.get(&crate_id) {
            return *id;
        }
        let crate_long_id = CrateSerializable::new(crate_id.lookup_intern(ctx.db), ctx);
        let id = CrateIdSerializable(ctx.crate_ids_lookup.len());
        ctx.crate_ids_lookup.push(crate_long_id);
        ctx.crate_ids.insert(crate_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> CrateId {
        if let Some(crate_id) = ctx.crate_ids.get(&self) {
            return *crate_id;
        }
        let crate_long_id = ctx.crate_ids_lookup[self.0].clone();
        let crate_id = crate_long_id.embed(ctx).intern(ctx.db);
        ctx.crate_ids.insert(self, crate_id);
        crate_id
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct SubmoduleSerializable {
    pub language_element: LanguageElementSerializable,
}
impl SubmoduleSerializable {
    fn new(submodule_id: SubmoduleLongId, ctx: &mut SemanticSerializationContext) -> Self {
        Self {
            language_element: LanguageElementSerializable::new(submodule_id.intern(ctx.db), ctx),
        }
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<SubmoduleLongId, ()> {
        let (module_file_id, stable_ptr) = self.language_element.embed(ctx)?;

        Ok(SubmoduleLongId(module_file_id, ItemModulePtr(stable_ptr)))
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct SubmoduleIdSerializable(usize);

impl SubmoduleIdSerializable {
    fn new(submodule_id: SubmoduleId, ctx: &mut SemanticSerializationContext) -> Self {
        if let Some(id) = ctx.submodule_ids.get(&submodule_id) {
            return *id;
        }
        let submodule = SubmoduleSerializable::new(submodule_id.lookup_intern(ctx.db), ctx);
        let id = SubmoduleIdSerializable(ctx.submodule_ids_lookup.len());
        ctx.submodule_ids_lookup.push(submodule);
        ctx.submodule_ids.insert(submodule_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> Result<SubmoduleId, ()> {
        if let Some(submodule_id) = ctx.submodule_ids.get(&self) {
            return Ok(*submodule_id);
        }
        let submodule = ctx.submodule_ids_lookup[self.0].clone();
        let submodule = submodule.embed(ctx)?.intern(ctx.db);
        ctx.submodule_ids.insert(self, submodule);
        Ok(submodule)
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct LanguageElementSerializable {
    module_file_id: ModuleFileSerializable,
    stable_ptr: SyntaxStablePtrIdSerializable,
}
impl LanguageElementSerializable {
    fn new<T: LanguageElementId>(
        language_element: T,
        ctx: &mut SemanticSerializationContext,
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
    fn embed(
        self,
        ctx: &mut SemanticDeserializationContext,
    ) -> Result<(ModuleFileId, SyntaxStablePtrId), ()> {
        let module_file_id = self.module_file_id.embed(ctx)?;
        let stable_ptr = self.stable_ptr.embed(ctx);
        Ok((module_file_id, stable_ptr))
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum SyntaxStablePtrSerializable {
    /// The root node of the tree.
    /// use the module_id for serialization.
    Root(ModuleFileSerializable, GreenIdSerializable),
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
        ctx: &mut SemanticSerializationContext,
    ) -> Self {
        match syntax_stable_ptr {
            SyntaxStablePtr::Root(root, green_id) => {
                assert!(Ok(root) == module_file_id.file_id(ctx.db.upcast()));
                SyntaxStablePtrSerializable::Root(
                    ModuleFileSerializable::new(module_file_id, ctx),
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
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> SyntaxStablePtr {
        match self {
            SyntaxStablePtrSerializable::Root(module, green_id) => {
                let module_id = module.embed(ctx).unwrap();
                SyntaxStablePtr::Root(
                    module_id.file_id(ctx.db.upcast()).map_err(|_| ()).unwrap(),
                    green_id.embed(ctx),
                )
            }
            SyntaxStablePtrSerializable::Child { parent, kind, key_fields, index } => {
                SyntaxStablePtr::Child {
                    parent: parent.embed(ctx),
                    kind,
                    key_fields: key_fields.into_iter().map(|field| field.embed(ctx)).collect(),
                    index,
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
struct SyntaxStablePtrIdSerializable(usize);
impl SyntaxStablePtrIdSerializable {
    fn new(
        syntax_stable_ptr_id: SyntaxStablePtrId,
        module_file_id: ModuleFileId,
        ctx: &mut SemanticSerializationContext,
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
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> SyntaxStablePtrId {
        if let Some(syntax_stable_ptr_id) = ctx.syntax_stable_ptr_ids.get(&self) {
            return *syntax_stable_ptr_id;
        }
        let stable_ptr = ctx.syntax_stable_ptr_ids_lookup[self.0].clone();
        let stable_ptr = stable_ptr.embed(ctx);
        let stable_ptr_id = stable_ptr.intern(ctx.db);
        ctx.syntax_stable_ptr_ids.insert(self, stable_ptr_id);
        stable_ptr_id
    }
}

#[derive(Serialize, Deserialize, Clone)]
enum GreenNodeDetailsSerializable {
    Token(SmolStr),
    Node { children: Vec<GreenIdSerializable>, width: TextWidth },
}

impl GreenNodeDetailsSerializable {
    fn new(
        green_node_details: &GreenNodeDetails,
        ctx: &mut SemanticSerializationContext,
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
    fn embed(&self, ctx: &mut SemanticDeserializationContext) -> GreenNodeDetails {
        match self {
            GreenNodeDetailsSerializable::Token(token) => GreenNodeDetails::Token(token.clone()),
            GreenNodeDetailsSerializable::Node { children, width } => GreenNodeDetails::Node {
                children: children.into_iter().map(|child| child.embed(ctx)).collect(),
                width: *width,
            },
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct GreenNodeSerializable {
    pub kind: SyntaxKind,
    details: GreenNodeDetailsSerializable,
}
impl GreenNodeSerializable {
    fn new(green_node: &GreenNode, ctx: &mut SemanticSerializationContext) -> Self {
        Self {
            kind: green_node.kind,
            details: GreenNodeDetailsSerializable::new(&green_node.details, ctx),
        }
    }
    fn embed(&self, ctx: &mut SemanticDeserializationContext) -> GreenNode {
        GreenNode { kind: self.kind, details: self.details.embed(ctx) }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Eq, Hash, PartialEq)]
struct GreenIdSerializable(usize);

impl GreenIdSerializable {
    fn new(green_id: GreenId, ctx: &mut SemanticSerializationContext) -> Self {
        if let Some(id) = ctx.green_ids.get(&green_id) {
            return *id;
        }
        let green_node = GreenNodeSerializable::new(green_id.lookup_intern(ctx.db).as_ref(), ctx);
        let id = GreenIdSerializable(ctx.green_ids_lookup.len());
        ctx.green_ids_lookup.push(green_node);
        ctx.green_ids.insert(green_id, id);
        id
    }
    fn embed(self, ctx: &mut SemanticDeserializationContext) -> GreenId {
        if let Some(green_id) = ctx.green_ids.get(&self) {
            return *green_id;
        }
        let green_node = ctx.green_ids_lookup[self.0].clone();
        let green_node = Arc::new(green_node.embed(ctx));
        let green_id = green_node.intern(ctx.db);
        ctx.green_ids.insert(self, green_id);
        green_id
    }
}
