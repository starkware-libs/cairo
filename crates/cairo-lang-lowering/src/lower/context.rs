use std::ops::Index;
use std::sync::Arc;

use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId, ModuleFileId};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::{Mutability, VarId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::{zip_eq, Itertools};
use semantic::expr::inference::InferenceError;
use semantic::types::wrap_in_snapshots;

use super::generators;
use super::scope::{BlockBuilder, SealedBlockBuilder};
use super::usage::BlockUsages;
use crate::blocks::FlatBlocksBuilder;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::ids::{ConcreteFunctionWithBodyId, SemanticFunctionIdEx};
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::objects::Variable;
use crate::{MatchArm, MatchExternInfo, MatchInfo, VariableId};

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
        function_id: FunctionWithBodyId,
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

/// Builds a Lowering context.
pub struct LoweringContextBuilder<'db> {
    pub db: &'db dyn LoweringGroup,
    pub function_id: FunctionWithBodyId,
    pub function_body: Arc<semantic::items::function_with_body::FunctionBody>,
    /// Semantic signature for current function.
    pub signature: semantic::Signature,
    /// The `ref` parameters of the current function.
    pub ref_params: Vec<semantic::VarId>,
}
impl<'db> LoweringContextBuilder<'db> {
    /// Constructs a new LoweringContextBuilder with the generic signature of the given generic
    /// function.
    pub fn new(db: &'db dyn LoweringGroup, function_id: FunctionWithBodyId) -> Maybe<Self> {
        let signature = db.function_with_body_signature(function_id)?;
        Self::new_inner(db, function_id, signature)
    }
    /// Constructs a new LoweringContextBuilder with a concrete signature of the given concrete
    /// function.
    pub fn new_concrete(
        db: &'db dyn LoweringGroup,
        concrete_function_with_body_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<Self> {
        let function_id = concrete_function_with_body_id.function_with_body_id(db.upcast());

        let signature = concrete_function_with_body_id.signature(db)?;
        Self::new_inner(db, function_id.semantic_function(db), signature)
    }
    fn new_inner(
        db: &'db dyn LoweringGroup,
        function_id: FunctionWithBodyId,
        signature: semantic::Signature,
    ) -> Maybe<Self> {
        let ref_params = signature
            .params
            .iter()
            .filter(|param| param.mutability == Mutability::Reference)
            .map(|param| VarId::Param(param.id))
            .collect();
        Ok(LoweringContextBuilder {
            db,
            function_id,
            function_body: db.function_body(function_id)?,
            signature,
            ref_params,
        })
    }
    pub fn ctx<'a: 'db>(&'a self, usages: BlockUsages) -> Maybe<LoweringContext<'db>> {
        Ok(LoweringContext {
            db: self.db,
            variables: VariableAllocator::new(self.db, self.function_id, Arena::default())?,
            function_id: self.function_id,
            function_body: &self.function_body,
            signature: &self.signature,
            diagnostics: LoweringDiagnostics::new(
                self.function_id.module_file_id(self.db.upcast()),
            ),
            blocks: FlatBlocksBuilder::new(),
            semantic_defs: UnorderedHashMap::default(),
            ref_params: &self.ref_params,
            expr_formatter: ExprFormatter { db: self.db.upcast(), function_id: self.function_id },
            usages,
        })
    }
}

/// Context for the lowering phase of a free function.
pub struct LoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    // Variable allocator.
    pub variables: VariableAllocator<'db>,
    /// Id for the current function being lowered.
    pub function_id: FunctionWithBodyId,
    /// Semantic model for current function body.
    pub function_body: &'db semantic::FunctionBody,
    /// Semantic signature for current function.
    pub signature: &'db semantic::Signature,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Lowered blocks of the function.
    pub blocks: FlatBlocksBuilder,
    /// Definitions encountered for semantic variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
    /// The `ref` parameters of the current function.
    pub ref_params: &'db [semantic::VarId],
    // Expression formatter of the free function.
    pub expr_formatter: ExprFormatter<'db>,
    pub usages: BlockUsages,
}
impl<'db> LoweringContext<'db> {
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
        ctx: &mut LoweringContext<'_>,
        scope: &mut BlockBuilder,
    ) -> Result<VariableId, LoweringFlowError> {
        match self {
            LoweredExpr::AtVariable(var_id) => Ok(var_id),
            LoweredExpr::Tuple { exprs, location } => {
                let inputs: Vec<_> = exprs
                    .into_iter()
                    .map(|expr| expr.var(ctx, scope))
                    .collect::<Result<Vec<_>, _>>()?;
                let tys = inputs.iter().map(|var| ctx.variables[*var].ty).collect();
                let ty = ctx.db.intern_type(semantic::TypeLongId::Tuple(tys));
                Ok(generators::StructConstruct { inputs, ty, location }
                    .add(ctx, &mut scope.statements))
            }
            LoweredExpr::ExternEnum(extern_enum) => extern_enum.var(ctx, scope),
            LoweredExpr::SemanticVar(semantic_var_id, location) => {
                Ok(scope.get_semantic(ctx, semantic_var_id, location))
            }
            LoweredExpr::Snapshot { expr, location } => {
                let (original, snapshot) =
                    generators::Snapshot { input: expr.clone().var(ctx, scope)?, location }
                        .add(ctx, &mut scope.statements);
                if let LoweredExpr::SemanticVar(semantic_var_id, _location) = &*expr {
                    scope.put_semantic(*semantic_var_id, original);
                }

                Ok(snapshot)
            }
        }
    }
    pub fn ty(&self, ctx: &mut LoweringContext<'_>) -> semantic::TypeId {
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
    pub member_paths: Vec<semantic::VarMemberPath>,
    pub location: StableLocationOption,
}
impl LoweredExprExternEnum {
    pub fn var(
        self,
        ctx: &mut LoweringContext<'_>,
        scope: &mut BlockBuilder,
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
                let mut subscope = scope.subscope(ctx.blocks.alloc_empty());
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
        scope
            .merge_and_end_with_match(ctx, match_info, sealed_blocks, self.location)?
            .var(ctx, scope)
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
    /// Every match arm is terminating - does not flow to parent scope
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

/// Converts a lowering flow error to the appropriate block scope end, if possible.
pub fn lowering_flow_error_to_sealed_block(
    ctx: &mut LoweringContext<'_>,
    scope: BlockBuilder,
    err: LoweringFlowError,
) -> Maybe<SealedBlockBuilder> {
    let block_id = scope.block_id;
    match err {
        LoweringFlowError::Failed(diag_added) => return Err(diag_added),
        LoweringFlowError::Return(return_var, location) => {
            scope.ret(ctx, return_var, location)?;
        }
        LoweringFlowError::Panic(data_var) => {
            scope.panic(ctx, data_var)?;
        }
        LoweringFlowError::Match(info) => {
            scope.unreachable_match(ctx, info);
        }
    }
    Ok(SealedBlockBuilder::Ends(block_id))
}
