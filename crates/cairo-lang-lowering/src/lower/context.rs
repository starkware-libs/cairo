use std::ops::{Deref, DerefMut, Index};
use std::sync::Arc;

use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_defs::ids::{LanguageElementId, ModuleFileId};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::{zip_eq, Itertools};
use semantic::expr::inference::InferenceError;
use semantic::types::wrap_in_snapshots;
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use super::block_builder::{BlockBuilder, SealedBlockBuilder};
use super::generators;
use super::usage::BlockUsages;
use crate::blocks::FlatBlocksBuilder;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::ids::{ConcreteFunctionWithBodyId, FunctionWithBodyId, SemanticFunctionIdEx, Signature};
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::objects::Variable;
use crate::{FlatLowered, MatchArm, MatchExternInfo, MatchInfo, VariableId};

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
        Ok(Self {
            db,
            variables,
            module_file_id: function_id.module_file_id(db.upcast()),
            lookup_context: ImplLookupContext {
                module_id: function_id.parent_module(db.upcast()),
                extra_modules: vec![],
                generic_params,
            },
        })
    }

    /// Allocates a new variable in the context's variable arena according to the context.
    pub fn new_var(&mut self, req: VarRequest) -> VariableId {
        let ty_info = self.db.type_info(self.lookup_context.clone(), req.ty);
        self.variables.alloc(Variable {
            duplicatable: ty_info
                .clone()
                .map_err(InferenceError::Failed)
                .and_then(|info| info.duplicatable),
            droppable: ty_info
                .clone()
                .map_err(InferenceError::Failed)
                .and_then(|info| info.droppable),
            destruct_impl: ty_info
                .map_err(InferenceError::Failed)
                .and_then(|info| info.destruct_impl),
            ty: req.ty,
            location: req.location,
        })
    }

    /// Retrieves the StableLocation of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId) -> StableLocationOption {
        StableLocationOption::new(self.module_file_id, stable_ptr)
    }
}
impl<'db> Index<VariableId> for VariableAllocator<'db> {
    type Output = Variable;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self.variables[index]
    }
}

/// Lowering context for the encapsulating semantic function.
/// Each semantic function may generate multiple lowered functions. This context is common to all
/// the generated lowered functions of an encapsulating semantic function.
pub struct EncapsulatingLoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Id for the current function being lowered.
    pub semantic_function_id: defs::ids::FunctionWithBodyId,
    /// Semantic model for current function body.
    pub function_body: Arc<semantic::FunctionBody>,
    /// Definitions encountered for semantic variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
    /// Expression formatter of the free function.
    pub expr_formatter: ExprFormatter<'db>,
    /// Block usages for the entire encapsulating function.
    pub block_usages: BlockUsages,
    /// Lowerings of generated functions.
    pub lowerings: OrderedHashMap<semantic::ExprId, FlatLowered>,
}
impl<'db> EncapsulatingLoweringContext<'db> {
    pub fn new(
        db: &'db dyn LoweringGroup,
        semantic_function_id: defs::ids::FunctionWithBodyId,
    ) -> Maybe<Self> {
        let function_body = db.function_body(semantic_function_id)?;
        let block_usages = BlockUsages::from_function_body(&function_body);
        Ok(Self {
            db,
            semantic_function_id,
            function_body,
            semantic_defs: Default::default(),
            expr_formatter: ExprFormatter { db: db.upcast(), function_id: semantic_function_id },
            block_usages,
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
    pub current_loop_expr: Option<semantic::ExprLoop>,
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
        let module_file_id = semantic_function.module_file_id(db.upcast());
        Ok(Self {
            encapsulating_ctx: Some(global_ctx),
            variables: VariableAllocator::new(db, semantic_function, Default::default())?,
            signature,
            function_id,
            concrete_function_id,
            current_loop_expr: Option::None,
            diagnostics: LoweringDiagnostics::new(module_file_id),
            blocks: Default::default(),
        })
    }
}
impl<'a, 'db> Deref for LoweringContext<'a, 'db> {
    type Target = EncapsulatingLoweringContext<'db>;

    fn deref(&self) -> &Self::Target {
        self.encapsulating_ctx.as_ref().unwrap()
    }
}
impl<'a, 'db> DerefMut for LoweringContext<'a, 'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.encapsulating_ctx.as_mut().unwrap()
    }
}
impl<'a, 'db> LoweringContext<'a, 'db> {
    /// Allocates a new variable in the context's variable arena according to the context.
    pub fn new_var(&mut self, req: VarRequest) -> VariableId {
        self.variables.new_var(req)
    }

    /// Retrieves the StableLocation of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId) -> StableLocationOption {
        self.variables.get_location(stable_ptr)
    }
}

/// Request for a lowered variable allocation.
pub struct VarRequest {
    pub ty: semantic::TypeId,
    pub location: StableLocationOption,
}

/// Representation of the value of a computed expression.
#[derive(Clone, Debug)]
pub enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(VariableId),
    /// The expression value is a tuple.
    Tuple {
        exprs: Vec<LoweredExpr>,
        location: StableLocationOption,
    },
    /// The expression value is an enum result from an extern call.
    ExternEnum(LoweredExprExternEnum),
    SemanticVar(semantic::VarId, StableLocationOption),
    Snapshot {
        expr: Box<LoweredExpr>,
        location: StableLocationOption,
    },
}
impl LoweredExpr {
    pub fn var(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut BlockBuilder,
    ) -> Result<VariableId, LoweringFlowError> {
        match self {
            LoweredExpr::AtVariable(var_id) => Ok(var_id),
            LoweredExpr::Tuple { exprs, location } => {
                let inputs: Vec<_> = exprs
                    .into_iter()
                    .map(|expr| expr.var(ctx, builder))
                    .collect::<Result<Vec<_>, _>>()?;
                let tys = inputs.iter().map(|var| ctx.variables[*var].ty).collect();
                let ty = ctx.db.intern_type(semantic::TypeLongId::Tuple(tys));
                Ok(generators::StructConstruct { inputs, ty, location }
                    .add(ctx, &mut builder.statements))
            }
            LoweredExpr::ExternEnum(extern_enum) => extern_enum.var(ctx, builder),
            LoweredExpr::SemanticVar(semantic_var_id, location) => {
                Ok(builder.get_semantic(ctx, semantic_var_id, location))
            }
            LoweredExpr::Snapshot { expr, location } => {
                let (original, snapshot) =
                    generators::Snapshot { input: expr.clone().var(ctx, builder)?, location }
                        .add(ctx, &mut builder.statements);
                if let LoweredExpr::SemanticVar(semantic_var_id, _location) = &*expr {
                    builder.put_semantic(*semantic_var_id, original);
                }

                Ok(snapshot)
            }
        }
    }
    pub fn ty(&self, ctx: &mut LoweringContext<'_, '_>) -> semantic::TypeId {
        match self {
            LoweredExpr::AtVariable(var) => ctx.variables[*var].ty,
            LoweredExpr::Tuple { exprs, .. } => ctx.db.intern_type(semantic::TypeLongId::Tuple(
                exprs.iter().map(|expr| expr.ty(ctx)).collect(),
            )),
            LoweredExpr::ExternEnum(extern_enum) => {
                ctx.db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(
                    extern_enum.concrete_enum_id,
                )))
            }
            LoweredExpr::SemanticVar(semantic_var_id, _) => {
                ctx.semantic_defs[*semantic_var_id].ty()
            }
            LoweredExpr::Snapshot { expr, .. } => {
                wrap_in_snapshots(ctx.db.upcast(), expr.ty(ctx), 1)
            }
        }
    }
}

/// Lazy expression value of an extern call returning an enum.
#[derive(Clone, Debug)]
pub struct LoweredExprExternEnum {
    pub function: semantic::FunctionId,
    pub concrete_enum_id: semantic::ConcreteEnumId,
    pub inputs: Vec<VariableId>,
    pub member_paths: Vec<semantic::ExprVarMemberPath>,
    pub location: StableLocationOption,
}
impl LoweredExprExternEnum {
    pub fn var(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        builder: &mut BlockBuilder,
    ) -> LoweringResult<VariableId> {
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
                        .var(ctx, &mut subscope);
                let input = match maybe_input {
                    Ok(var) => var,
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
                .map(|((variant_id, block_id), var_ids)| MatchArm { variant_id, block_id, var_ids })
                .collect(),
            location: self.location,
        });
        builder
            .merge_and_end_with_match(ctx, match_info, sealed_blocks, self.location)?
            .var(ctx, builder)
    }
}

pub type LoweringResult<T> = Result<T, LoweringFlowError>;

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    Panic(VariableId),
    Return(VariableId, StableLocationOption),
    /// Every match arm is terminating - does not flow to parent builder
    /// e.g. returns or panics.
    Match(MatchInfo),
}
impl LoweringFlowError {
    pub fn is_unreachable(&self) -> bool {
        match self {
            LoweringFlowError::Failed(_) => false,
            LoweringFlowError::Panic(_)
            | LoweringFlowError::Return(_, _)
            | LoweringFlowError::Match(_) => true,
        }
    }
}

/// Converts a lowering flow error to the appropriate block builder end, if possible.
pub fn lowering_flow_error_to_sealed_block(
    ctx: &mut LoweringContext<'_, '_>,
    builder: BlockBuilder,
    err: LoweringFlowError,
) -> Maybe<SealedBlockBuilder> {
    let block_id = builder.block_id;
    match err {
        LoweringFlowError::Failed(diag_added) => return Err(diag_added),
        LoweringFlowError::Return(return_var, location) => {
            builder.ret(ctx, return_var, location)?;
        }
        LoweringFlowError::Panic(data_var) => {
            builder.panic(ctx, data_var)?;
        }
        LoweringFlowError::Match(info) => {
            builder.unreachable_match(ctx, info);
        }
    }
    Ok(SealedBlockBuilder::Ends(block_id))
}
