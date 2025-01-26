use std::ops::{Deref, DerefMut, Index};
use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, ModuleFileId};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::usage::Usages;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use defs::diagnostic_utils::StableLocation;
use id_arena::Arena;
use itertools::{Itertools, zip_eq};
use semantic::corelib::{core_module, get_ty_by_name};
use semantic::types::wrap_in_snapshots;
use semantic::{ExprVarMemberPath, MatchArmSelector, TypeLongId};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use super::block_builder::{BlockBuilder, SealedBlockBuilder};
use super::generators;
use crate::blocks::FlatBlocksBuilder;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::ids::{
    ConcreteFunctionWithBodyId, FunctionWithBodyId, GeneratedFunctionKey, LocationId,
    SemanticFunctionIdEx, Signature,
};
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::objects::Variable;
use crate::{FlatLowered, MatchArm, MatchExternInfo, MatchInfo, VarUsage, VariableId};

pub struct VariableAllocator<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Module and file of the declared function.
    pub module_file_id: ModuleFileId,
    // Lookup context for impls.
    pub lookup_context: ImplLookupContext,
}
impl<'db> VariableAllocator<'db> {
    pub fn new(
        db: &'db dyn LoweringGroup,
        function_id: defs::ids::FunctionWithBodyId,
        variables: Arena<Variable>,
    ) -> Maybe<Self> {
        let generic_params = db.function_with_body_generic_params(function_id)?;
        let generic_param_ids = generic_params.iter().map(|p| p.id()).collect_vec();
        Ok(Self {
            db,
            variables,
            module_file_id: function_id.module_file_id(db.upcast()),
            lookup_context: ImplLookupContext::new(
                function_id.parent_module(db.upcast()),
                generic_param_ids,
            ),
        })
    }

    /// Allocates a new variable in the context's variable arena according to the context.
    pub fn new_var(&mut self, req: VarRequest) -> VariableId {
        self.variables.alloc(Variable::new(
            self.db,
            self.lookup_context.clone(),
            req.ty,
            req.location,
        ))
    }

    /// Retrieves the LocationId of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId) -> LocationId {
        LocationId::from_stable_location(self.db, StableLocation::new(stable_ptr))
    }
}
impl Index<VariableId> for VariableAllocator<'_> {
    type Output = Variable;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self.variables[index]
    }
}

/// Lowering context for the encapsulating semantic function.
///
/// Each semantic function may generate multiple lowered functions. This context is common to all
/// the generated lowered functions of an encapsulating semantic function.
pub struct EncapsulatingLoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Id for the current function being lowered.
    pub semantic_function_id: defs::ids::FunctionWithBodyId,
    /// Semantic model for current function body.
    pub function_body: Arc<semantic::FunctionBody>,
    /// Definitions encountered for semantic bindings. Since Constants are not lowered, this is
    /// only used for variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Binding>,
    /// Expression formatter of the free function.
    pub expr_formatter: ExprFormatter<'db>,
    /// Block usages for the entire encapsulating function.
    pub usages: Usages,
    /// Lowerings of generated functions.
    pub lowerings: OrderedHashMap<GeneratedFunctionKey, FlatLowered>,
}
impl<'db> EncapsulatingLoweringContext<'db> {
    pub fn new(
        db: &'db dyn LoweringGroup,
        semantic_function_id: defs::ids::FunctionWithBodyId,
    ) -> Maybe<Self> {
        let function_body = db.function_body(semantic_function_id)?;
        let usages = Usages::from_function_body(&function_body);
        Ok(Self {
            db,
            semantic_function_id,
            function_body,
            semantic_defs: Default::default(),
            expr_formatter: ExprFormatter { db: db.upcast(), function_id: semantic_function_id },
            usages,
            lowerings: Default::default(),
        })
    }
}

pub struct LoweringContext<'a, 'db> {
    pub encapsulating_ctx: Option<&'a mut EncapsulatingLoweringContext<'db>>,
    /// Variable allocator.
    pub variables: VariableAllocator<'db>,
    /// Current function signature.
    pub signature: Signature,
    /// Id for the current function being lowered.
    pub function_id: FunctionWithBodyId,
    /// Id for the current concrete function to be used when generating recursive calls.
    /// This it the generic function specialized with its own generic parameters.
    pub concrete_function_id: ConcreteFunctionWithBodyId,
    /// Current loop expression needed for recursive calls in `continue`
    pub current_loop_expr_id: Option<semantic::ExprId>,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Lowered blocks of the function.
    pub blocks: FlatBlocksBuilder,
}
impl<'a, 'db> LoweringContext<'a, 'db> {
    pub fn new(
        global_ctx: &'a mut EncapsulatingLoweringContext<'db>,
        function_id: FunctionWithBodyId,
        signature: Signature,
    ) -> Maybe<Self>
    where
        'db: 'a,
    {
        let db = global_ctx.db;
        let concrete_function_id = function_id.to_concrete(db)?;
        let semantic_function = function_id.base_semantic_function(db);
        Ok(Self {
            encapsulating_ctx: Some(global_ctx),
            variables: VariableAllocator::new(db, semantic_function, Default::default())?,
            signature,
            function_id,
            concrete_function_id,
            current_loop_expr_id: None,
            diagnostics: LoweringDiagnostics::default(),
            blocks: Default::default(),
        })
    }
}
impl<'db> Deref for LoweringContext<'_, 'db> {
    type Target = EncapsulatingLoweringContext<'db>;

    fn deref(&self) -> &Self::Target {
        self.encapsulating_ctx.as_ref().unwrap()
    }
}
impl DerefMut for LoweringContext<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.encapsulating_ctx.as_mut().unwrap()
    }
}
impl LoweringContext<'_, '_> {
    /// Allocates a new variable in the context's variable arena according to the context.
    pub fn new_var(&mut self, req: VarRequest) -> VariableId {
        self.variables.new_var(req)
    }

    /// Same as `new_var` but returns it as a `VarUsage`.
    /// This is useful when the variable definition and usage locations are the same.
    pub fn new_var_usage(&mut self, req: VarRequest) -> VarUsage {
        let location = req.location;

        VarUsage { var_id: self.variables.new_var(req), location }
    }

    /// Retrieves the LocationId of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId) -> LocationId {
        self.variables.get_location(stable_ptr)
    }
}

/// Request for a lowered variable allocation.
pub struct VarRequest {
    pub ty: semantic::TypeId,
    pub location: LocationId,
}

/// Representation of the value of a computed expression.
#[derive(Clone, Debug)]
pub enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(VarUsage),
    /// The expression value is a tuple.
    Tuple {
        exprs: Vec<LoweredExpr>,
        location: LocationId,
    },
    /// The expression value is a fixed size array.
    FixedSizeArray {
        ty: semantic::TypeId,
        exprs: Vec<LoweredExpr>,
        location: LocationId,
    },
    /// The expression value is an enum result from an extern call.
    ExternEnum(LoweredExprExternEnum),
    Member(ExprVarMemberPath, LocationId),
    Snapshot {
        expr: Box<LoweredExpr>,
        location: LocationId,
    },
}
impl LoweredExpr {
    // Returns a VarUsage corresponding to the lowered expression.
    pub fn as_var_usage(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut BlockBuilder,
    ) -> Result<VarUsage, LoweringFlowError> {
        match self {
            LoweredExpr::AtVariable(var_usage) => Ok(var_usage),
            LoweredExpr::Tuple { exprs, location } => {
                let inputs: Vec<_> = exprs
                    .into_iter()
                    .map(|expr| expr.as_var_usage(ctx, builder))
                    .collect::<Result<Vec<_>, _>>()?;
                let tys =
                    inputs.iter().map(|var_usage| ctx.variables[var_usage.var_id].ty).collect();
                let ty = semantic::TypeLongId::Tuple(tys).intern(ctx.db);
                Ok(generators::StructConstruct { inputs, ty, location }
                    .add(ctx, &mut builder.statements))
            }
            LoweredExpr::ExternEnum(extern_enum) => extern_enum.as_var_usage(ctx, builder),
            LoweredExpr::Member(member_path, _location) => {
                Ok(builder.get_ref(ctx, &member_path).unwrap())
            }
            LoweredExpr::Snapshot { expr, location } => {
                let input = expr.clone().as_var_usage(ctx, builder)?;
                let (original, snapshot) =
                    generators::Snapshot { input, location }.add(ctx, &mut builder.statements);
                if let LoweredExpr::Member(member_path, _location) = &*expr {
                    builder.update_ref(ctx, member_path, original);
                }

                Ok(VarUsage { var_id: snapshot, location })
            }
            LoweredExpr::FixedSizeArray { exprs, location, ty } => {
                let inputs = exprs
                    .into_iter()
                    .map(|expr| expr.as_var_usage(ctx, builder))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(generators::StructConstruct { inputs, ty, location }
                    .add(ctx, &mut builder.statements))
            }
        }
    }

    pub fn ty(&self, ctx: &mut LoweringContext<'_, '_>) -> semantic::TypeId {
        match self {
            LoweredExpr::AtVariable(var_usage) => ctx.variables[var_usage.var_id].ty,
            LoweredExpr::Tuple { exprs, .. } => {
                semantic::TypeLongId::Tuple(exprs.iter().map(|expr| expr.ty(ctx)).collect())
                    .intern(ctx.db)
            }
            LoweredExpr::ExternEnum(extern_enum) => semantic::TypeLongId::Concrete(
                semantic::ConcreteTypeId::Enum(extern_enum.concrete_enum_id),
            )
            .intern(ctx.db),
            LoweredExpr::Member(member_path, _) => member_path.ty(),
            LoweredExpr::Snapshot { expr, .. } => {
                wrap_in_snapshots(ctx.db.upcast(), expr.ty(ctx), 1)
            }
            LoweredExpr::FixedSizeArray { ty, .. } => *ty,
        }
    }
    pub fn location(&self) -> LocationId {
        match &self {
            LoweredExpr::AtVariable(VarUsage { location, .. })
            | LoweredExpr::Tuple { location, .. }
            | LoweredExpr::ExternEnum(LoweredExprExternEnum { location, .. })
            | LoweredExpr::Member(_, location)
            | LoweredExpr::Snapshot { location, .. } => *location,
            LoweredExpr::FixedSizeArray { location, .. } => *location,
        }
    }
}

/// Lazy expression value of an extern call returning an enum.
#[derive(Clone, Debug)]
pub struct LoweredExprExternEnum {
    pub function: semantic::FunctionId,
    pub concrete_enum_id: semantic::ConcreteEnumId,
    pub inputs: Vec<VarUsage>,
    pub member_paths: Vec<semantic::ExprVarMemberPath>,
    pub location: LocationId,
}
impl LoweredExprExternEnum {
    pub fn as_var_usage(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut BlockBuilder,
    ) -> LoweringResult<VarUsage> {
        let concrete_variants = ctx
            .db
            .concrete_enum_variants(self.concrete_enum_id)
            .map_err(LoweringFlowError::Failed)?;

        let mut arm_var_ids = vec![];
        let (sealed_blocks, block_ids): (Vec<_>, Vec<_>) = concrete_variants
            .clone()
            .into_iter()
            .map(|concrete_variant| {
                let mut subscope = builder.child_block_builder(ctx.blocks.alloc_empty());
                let block_id = subscope.block_id;

                let mut var_ids = vec![];
                // Bind the ref parameters.
                for member_path in &self.member_paths {
                    let var =
                        ctx.new_var(VarRequest { ty: member_path.ty(), location: self.location });
                    var_ids.push(var);

                    subscope.update_ref(ctx, member_path, var);
                }

                let variant_vars = extern_facade_return_tys(ctx, concrete_variant.ty)
                    .into_iter()
                    .map(|ty| ctx.new_var(VarRequest { ty, location: self.location }))
                    .collect_vec();
                var_ids.extend(variant_vars.iter());

                arm_var_ids.push(var_ids);
                let maybe_input =
                    extern_facade_expr(ctx, concrete_variant.ty, variant_vars, self.location)
                        .as_var_usage(ctx, &mut subscope);
                let input = match maybe_input {
                    Ok(var_usage) => var_usage,
                    Err(err) => {
                        return lowering_flow_error_to_sealed_block(ctx, subscope, err)
                            .map(|sb| (sb, block_id));
                    }
                };
                let result = generators::EnumConstruct {
                    input,
                    variant: concrete_variant,
                    location: self.location,
                }
                .add(ctx, &mut subscope.statements);
                Ok((subscope.goto_callsite(Some(result)), block_id))
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(LoweringFlowError::Failed)?
            .into_iter()
            .unzip();

        let match_info = MatchInfo::Extern(MatchExternInfo {
            function: self.function.lowered(ctx.db),
            inputs: self.inputs,
            arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
                .map(|((variant_id, block_id), var_ids)| MatchArm {
                    arm_selector: MatchArmSelector::VariantId(variant_id),
                    block_id,
                    var_ids,
                })
                .collect(),
            location: self.location,
        });
        builder
            .merge_and_end_with_match(ctx, match_info, sealed_blocks, self.location)?
            .as_var_usage(ctx, builder)
    }
}

pub type LoweringResult<T> = Result<T, LoweringFlowError>;

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    Panic(VarUsage, LocationId),
    Return(VarUsage, LocationId),
    /// Every match arm is terminating - does not flow to parent builder
    /// e.g. returns or panics.
    Match(MatchInfo),
}
impl LoweringFlowError {
    pub fn is_unreachable(&self) -> bool {
        match self {
            LoweringFlowError::Failed(_) => false,
            LoweringFlowError::Panic(_, _)
            | LoweringFlowError::Return(_, _)
            | LoweringFlowError::Match(_) => true,
        }
    }
}

/// Converts a lowering flow error to the appropriate block builder end, if possible.
pub fn lowering_flow_error_to_sealed_block(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    err: LoweringFlowError,
) -> Maybe<SealedBlockBuilder> {
    let block_id = builder.block_id;
    match err {
        LoweringFlowError::Failed(diag_added) => return Err(diag_added),
        LoweringFlowError::Return(return_var, location) => {
            builder.ret(ctx, return_var, location)?;
        }
        LoweringFlowError::Panic(data_var, location) => {
            let panic_instance = generators::StructConstruct {
                inputs: vec![],
                ty: get_ty_by_name(
                    ctx.db.upcast(),
                    core_module(ctx.db.upcast()),
                    "Panic".into(),
                    vec![],
                ),
                location,
            }
            .add(ctx, &mut builder.statements);
            let err_instance = generators::StructConstruct {
                inputs: vec![panic_instance, data_var],
                ty: TypeLongId::Tuple(vec![
                    ctx.variables[panic_instance.var_id].ty,
                    ctx.variables[data_var.var_id].ty,
                ])
                .intern(ctx.db),
                location,
            }
            .add(ctx, &mut builder.statements);
            builder.panic(ctx, err_instance)?;
        }
        LoweringFlowError::Match(info) => {
            builder.unreachable_match(ctx, info);
        }
    }
    Ok(SealedBlockBuilder::Ends(block_id))
}
