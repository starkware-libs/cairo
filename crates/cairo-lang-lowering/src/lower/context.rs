use std::ops::{Deref, DerefMut, Index};

use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_semantic::ConcreteVariant;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::items::imp::{ImplLookupContext, ImplLookupContextId};
use cairo_lang_semantic::usage::{MemberPath, Usages};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use defs::diagnostic_utils::StableLocation;
use itertools::{Itertools, zip_eq};
use salsa::Database;
use semantic::corelib::{core_module, get_ty_by_name};
use semantic::types::wrap_in_snapshots;
use semantic::{ExprVarMemberPath, MatchArmSelector, TypeLongId};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use super::block_builder::BlockBuilder;
use super::generators;
use crate::blocks::BlocksBuilder;
use crate::diagnostic::LoweringDiagnostics;
use crate::ids::{
    ConcreteFunctionWithBodyId, EnrichedSemanticSignature, FunctionWithBodyId,
    GeneratedFunctionKey, LocationId, SemanticFunctionIdEx,
};
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::objects::Variable;
use crate::{Lowered, MatchArm, MatchExternInfo, MatchInfo, VarUsage, VariableArena, VariableId};

pub struct VariableAllocator<'db> {
    pub db: &'db dyn Database,
    /// Arena of allocated lowered variables.
    pub variables: VariableArena<'db>,
    // Lookup context for impls.
    pub lookup_context: ImplLookupContextId<'db>,
}
impl<'db> VariableAllocator<'db> {
    pub fn new(
        db: &'db dyn Database,
        function_id: defs::ids::FunctionWithBodyId<'db>,
        variables: VariableArena<'db>,
    ) -> Maybe<Self> {
        let generic_params = db.function_with_body_generic_params(function_id)?;
        let generic_param_ids = generic_params.iter().map(|p| p.id()).collect_vec();
        Ok(Self {
            db,
            variables,
            lookup_context: ImplLookupContext::new(
                function_id.parent_module(db),
                generic_param_ids,
                db,
            )
            .intern(db),
        })
    }

    /// Allocates a new variable in the context's variable arena according to the context.
    pub fn new_var(&mut self, req: VarRequest<'db>) -> VariableId {
        self.variables.alloc(Variable::new(self.db, self.lookup_context, req.ty, req.location))
    }

    /// Retrieves the LocationId of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId<'db>) -> LocationId<'db> {
        LocationId::from_stable_location(self.db, StableLocation::new(stable_ptr))
    }
}
impl<'db> Index<VariableId> for VariableAllocator<'db> {
    type Output = Variable<'db>;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self.variables[index]
    }
}

/// Lowering context for the encapsulating semantic function.
///
/// Each semantic function may generate multiple lowered functions. This context is common to all
/// the generated lowered functions of an encapsulating semantic function.
pub struct EncapsulatingLoweringContext<'db> {
    pub db: &'db dyn Database,
    /// Id for the current function being lowered.
    pub semantic_function_id: defs::ids::FunctionWithBodyId<'db>,
    /// Semantic model for current function body.
    pub function_body: &'db semantic::FunctionBody<'db>,
    /// Definitions encountered for semantic bindings. Since Constants are not lowered, this is
    /// only used for variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId<'db>, semantic::Binding<'db>>,
    /// Expression formatter of the free function.
    pub expr_formatter: ExprFormatter<'db>,
    /// Block usages for the entire encapsulating function.
    pub usages: Usages<'db>,
    /// Lowerings of generated functions.
    pub lowerings: OrderedHashMap<GeneratedFunctionKey<'db>, Lowered<'db>>,
}
impl<'db> EncapsulatingLoweringContext<'db> {
    pub fn new(
        db: &'db dyn Database,
        semantic_function_id: defs::ids::FunctionWithBodyId<'db>,
    ) -> Maybe<Self> {
        let function_body = db.function_body(semantic_function_id)?;
        let usages = Usages::from_function_body(function_body);
        Ok(Self {
            db,
            semantic_function_id,
            function_body,
            semantic_defs: Default::default(),
            expr_formatter: ExprFormatter { db, function_id: semantic_function_id },
            usages,
            lowerings: Default::default(),
        })
    }
}

/// The loop result variants for a loop with an early return.
#[derive(Clone)]
pub struct LoopEarlyReturnInfo<'db> {
    pub normal_return_variant: ConcreteVariant<'db>,
    pub early_return_variant: ConcreteVariant<'db>,
}

/// Context for lowering a loop.
pub struct LoopContext<'db> {
    /// The loop expression
    pub loop_expr_id: semantic::ExprId,
    /// Optional info related to early return from the loop.
    pub early_return_info: Option<LoopEarlyReturnInfo<'db>>,
}

pub struct LoweringContext<'db, 'mt> {
    pub encapsulating_ctx: Option<&'mt mut EncapsulatingLoweringContext<'db>>,
    /// Variable allocator.
    pub variables: VariableAllocator<'db>,
    /// Current function signature.
    pub signature: EnrichedSemanticSignature<'db>,
    /// Id for the current function being lowered.
    pub function_id: FunctionWithBodyId<'db>,
    /// Id for the current concrete function to be used when generating recursive calls.
    /// This is the generic function specialized with its own generic parameters.
    pub concrete_function_id: ConcreteFunctionWithBodyId<'db>,
    /// Current loop context.
    pub current_loop_ctx: Option<LoopContext<'db>>,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics<'db>,
    /// Lowered blocks of the function.
    pub blocks: BlocksBuilder<'db>,
    // The return type in the current context, for loops this differs from signature.return_type.
    pub return_type: semantic::TypeId<'db>,
    /// The semantic variables that are captured as snapshots.
    ///
    /// For example, if we have a loop body that uses `@x.a`, then `x.a` will be added to
    /// `snapped_semantics`.
    pub snapped_semantics: OrderedHashMap<MemberPath<'db>, VariableId>,
}
impl<'db, 'mt> LoweringContext<'db, 'mt> {
    pub fn new(
        global_ctx: &'mt mut EncapsulatingLoweringContext<'db>,
        function_id: FunctionWithBodyId<'db>,
        signature: EnrichedSemanticSignature<'db>,
        return_type: semantic::TypeId<'db>,
    ) -> Maybe<Self>
    where
        'db: 'mt,
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
            current_loop_ctx: None,
            diagnostics: LoweringDiagnostics::default(),
            blocks: Default::default(),
            return_type,
            snapped_semantics: Default::default(),
        })
    }
}
impl<'db, 'mt> Deref for LoweringContext<'db, 'mt> {
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
impl<'db, 'mt> LoweringContext<'db, 'mt> {
    /// Allocates a new variable in the context's variable arena according to the context.
    pub fn new_var(&mut self, req: VarRequest<'db>) -> VariableId {
        self.variables.new_var(req)
    }

    /// Same as `new_var` but returns it as a `VarUsage`.
    /// This is useful when the variable definition and usage locations are the same.
    pub fn new_var_usage(&mut self, req: VarRequest<'db>) -> VarUsage<'db> {
        let location = req.location;

        VarUsage { var_id: self.variables.new_var(req), location }
    }

    /// Retrieves the LocationId of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId<'db>) -> LocationId<'db> {
        self.variables.get_location(stable_ptr)
    }
}

/// Request for a lowered variable allocation.
pub struct VarRequest<'db> {
    pub ty: semantic::TypeId<'db>,
    pub location: LocationId<'db>,
}

/// Representation of the value of a computed expression.
#[derive(Clone, Debug)]
pub enum LoweredExpr<'db> {
    /// The expression value lies in a variable.
    AtVariable(VarUsage<'db>),
    /// The expression value is a tuple.
    Tuple {
        exprs: Vec<LoweredExpr<'db>>,
        location: LocationId<'db>,
    },
    /// The expression value is a fixed size array.
    FixedSizeArray {
        ty: semantic::TypeId<'db>,
        exprs: Vec<LoweredExpr<'db>>,
        location: LocationId<'db>,
    },
    /// The expression value is an enum result from an extern call.
    ExternEnum(LoweredExprExternEnum<'db>),
    /// The expression value resides at a specific member path.
    MemberPath(ExprVarMemberPath<'db>, LocationId<'db>),
    Snapshot {
        expr: Box<LoweredExpr<'db>>,
        location: LocationId<'db>,
    },
}
impl<'db> LoweredExpr<'db> {
    /// Returns a [VarUsage] corresponding to the lowered expression.
    pub fn as_var_usage(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut BlockBuilder<'db>,
    ) -> LoweringResult<'db, VarUsage<'db>> {
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
            LoweredExpr::MemberPath(member_path, _location) => {
                Ok(builder.get_ref(ctx, &member_path).unwrap())
            }
            LoweredExpr::Snapshot { expr, location } => {
                if let LoweredExpr::MemberPath(member_path, _location) = &*expr
                    && let Some(var_usage) = builder.get_snap_ref(ctx, member_path)
                {
                    return Ok(VarUsage { var_id: var_usage.var_id, location });
                }

                let input = expr.clone().as_var_usage(ctx, builder)?;
                let (original, snapshot) =
                    generators::Snapshot { input, location }.add(ctx, &mut builder.statements);
                if let LoweredExpr::MemberPath(member_path, _location) = &*expr {
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

    pub fn ty(&self, ctx: &mut LoweringContext<'db, '_>) -> semantic::TypeId<'db> {
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
            LoweredExpr::MemberPath(member_path, _) => member_path.ty(),
            LoweredExpr::Snapshot { expr, .. } => wrap_in_snapshots(ctx.db, expr.ty(ctx), 1),
            LoweredExpr::FixedSizeArray { ty, .. } => *ty,
        }
    }
    pub fn location(&self) -> LocationId<'db> {
        match &self {
            LoweredExpr::AtVariable(VarUsage { location, .. })
            | LoweredExpr::Tuple { location, .. }
            | LoweredExpr::ExternEnum(LoweredExprExternEnum { location, .. })
            | LoweredExpr::MemberPath(_, location)
            | LoweredExpr::Snapshot { location, .. } => *location,
            LoweredExpr::FixedSizeArray { location, .. } => *location,
        }
    }
}

/// Lazy expression value of an extern call returning an enum.
#[derive(Clone, Debug)]
pub struct LoweredExprExternEnum<'db> {
    pub function: semantic::FunctionId<'db>,
    pub concrete_enum_id: semantic::ConcreteEnumId<'db>,
    pub inputs: Vec<VarUsage<'db>>,
    pub member_paths: Vec<semantic::ExprVarMemberPath<'db>>,
    pub location: LocationId<'db>,
}
impl<'db> LoweredExprExternEnum<'db> {
    pub fn as_var_usage(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        builder: &mut BlockBuilder<'db>,
    ) -> LoweringResult<'db, VarUsage<'db>> {
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
                        return handle_lowering_flow_error(ctx, subscope, err)
                            .map(|_| (None, block_id));
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

pub type LoweringResult<'db, T> = Result<T, LoweringFlowError<'db>>;

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug, Clone)]
pub enum LoweringFlowError<'db> {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    /// The expression that was being lowered always panics, and does not flow to the parent
    /// builder.
    Panic(VarUsage<'db>, LocationId<'db>),
    /// The expression that was being lowered always returns from the function, and does not flow to
    /// the parent builder.
    Return(VarUsage<'db>, LocationId<'db>),
    /// Every match arm is terminating - does not flow to parent builder
    /// e.g. returns or panics.
    Match(MatchInfo<'db>),
}
impl<'db> LoweringFlowError<'db> {
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
pub fn handle_lowering_flow_error<'db, 'mt>(
    ctx: &mut LoweringContext<'db, 'mt>,
    mut builder: BlockBuilder<'db>,
    err: LoweringFlowError<'db>,
) -> Maybe<()> {
    match err {
        LoweringFlowError::Failed(diag_added) => Err(diag_added),
        LoweringFlowError::Return(return_var, location) => builder.ret(ctx, return_var, location),
        LoweringFlowError::Panic(data_var, location) => {
            let panic_instance = generators::StructConstruct {
                inputs: vec![],
                ty: get_ty_by_name(
                    ctx.db,
                    core_module(ctx.db),
                    SmolStrId::from(ctx.db, "Panic"),
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
            builder.panic(ctx, err_instance)
        }
        LoweringFlowError::Match(info) => {
            builder.unreachable_match(ctx, info);
            Ok(())
        }
    }
}
