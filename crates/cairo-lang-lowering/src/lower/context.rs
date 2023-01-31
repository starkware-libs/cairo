use std::sync::Arc;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::{Mutability, VarId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::zip_eq;

use super::generators;
use super::scope::{merge_sealed, BlockBuilder, SealedBlockBuilder};
use crate::blocks::StructuredBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::objects::Variable;
use crate::{Statement, StatementMatchExtern, VariableId};

/// Builds a Lowering context.
pub struct LoweringContextBuilder<'db> {
    pub db: &'db dyn LoweringGroup,
    pub function_id: FunctionWithBodyId,
    pub function_body: Arc<semantic::items::function_with_body::FunctionBody>,
    /// Semantic signature for current function.
    pub signature: semantic::Signature,
    // TODO(spapini): Document. (excluding implicits).
    pub ref_params: Vec<semantic::VarId>,
    /// The available implicits in this function.
    pub implicits: Vec<semantic::TypeId>,
}
impl<'db> LoweringContextBuilder<'db> {
    pub fn new(db: &'db dyn LoweringGroup, function_id: FunctionWithBodyId) -> Maybe<Self> {
        let function_body = db.function_body(function_id)?;
        let signature = db.function_with_body_signature(function_id)?;
        let implicits = db.function_with_body_all_implicits_vec(function_id)?;
        let ref_params = signature
            .params
            .iter()
            .filter(|param| param.mutability == Mutability::Reference)
            .map(|param| VarId::Param(param.id))
            .collect();
        Ok(LoweringContextBuilder {
            db,
            function_id,
            function_body,
            signature,
            ref_params,
            implicits,
        })
    }
    pub fn ctx<'a: 'db>(&'a self) -> Maybe<LoweringContext<'db>> {
        let generic_params = self.db.function_with_body_generic_params(self.function_id)?;
        Ok(LoweringContext {
            db: self.db,
            function_id: self.function_id,
            function_body: &self.function_body,
            signature: &self.signature,
            diagnostics: LoweringDiagnostics::new(
                self.function_id.module_file_id(self.db.upcast()),
            ),
            variables: Arena::default(),
            blocks: StructuredBlocks::new(),
            semantic_defs: UnorderedHashMap::default(),
            ref_params: &self.ref_params,
            implicits: &self.implicits,
            lookup_context: ImplLookupContext {
                module_id: self.function_id.parent_module(self.db.upcast()),
                extra_modules: vec![],
                generic_params,
            },
            expr_formatter: ExprFormatter { db: self.db.upcast(), function_id: self.function_id },
        })
    }
}

/// Context for the lowering phase of a free function.
pub struct LoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Id for the current function being lowered.
    pub function_id: FunctionWithBodyId,
    /// Semantic model for current function body.
    pub function_body: &'db semantic::FunctionBody,
    /// Semantic signature for current function.
    pub signature: &'db semantic::Signature,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Lowered blocks of the function.
    pub blocks: StructuredBlocks,
    /// Definitions encountered for semantic variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
    // TODO(spapini): Document. (excluding implicits).
    pub ref_params: &'db [semantic::VarId],
    // The available implicits in this function.
    pub implicits: &'db [semantic::TypeId],
    // Lookup context for impls.
    pub lookup_context: ImplLookupContext,
    // Expression formatter of the free function.
    pub expr_formatter: ExprFormatter<'db>,
}
impl<'db> LoweringContext<'db> {
    pub fn new_var(&mut self, req: VarRequest) -> VariableId {
        let ty_info = self.db.type_info(self.lookup_context.clone(), req.ty).unwrap_or_default();
        self.variables.alloc(Variable {
            duplicatable: ty_info.duplicatable,
            droppable: ty_info.droppable,
            ty: req.ty,
            location: req.location,
        })
    }

    /// Retrieves the StableLocation of a stable syntax pointer in the current function file.
    pub fn get_location(&self, stable_ptr: SyntaxStablePtrId) -> StableLocation {
        StableLocation::new(self.function_id.module_file_id(self.db.upcast()), stable_ptr)
    }
}

/// Request for a lowered variable allocation.
pub struct VarRequest {
    pub ty: semantic::TypeId,
    pub location: StableLocation,
}

/// Representation of the value of a computed expression.
#[derive(Debug)]
pub enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(VariableId),
    /// The expression value is a tuple.
    Tuple { exprs: Vec<LoweredExpr>, location: StableLocation },
    /// The expression value is an enum result from an extern call.
    ExternEnum(LoweredExprExternEnum),
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
                Ok(generators::StructConstruct { inputs, ty, location }.add(ctx, scope))
            }
            LoweredExpr::ExternEnum(extern_enum) => extern_enum.var(ctx, scope),
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
        }
    }
}

/// Lazy expression value of an extern call returning an enum.
#[derive(Debug)]
pub struct LoweredExprExternEnum {
    pub function: semantic::FunctionId,
    pub concrete_enum_id: semantic::ConcreteEnumId,
    pub inputs: Vec<VariableId>,
    pub ref_args: Vec<semantic::VarId>,
    /// The implicits used/changed by the function.
    pub implicits: Vec<semantic::TypeId>,
    pub location: StableLocation,
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
        let sealed_blocks = concrete_variants
            .clone()
            .into_iter()
            .map(|concrete_variant| {
                let mut subscope = scope.subscope();

                // Bind implicits.
                for ty in &self.implicits {
                    let var =
                        subscope.add_input(ctx, VarRequest { ty: *ty, location: self.location });
                    subscope.put_implicit(ctx, *ty, var);
                }
                // Bind the ref parameters.
                for semantic in &self.ref_args {
                    let var = subscope.add_input(
                        ctx,
                        VarRequest {
                            ty: ctx.semantic_defs[*semantic].ty(),
                            location: self.location,
                        },
                    );
                    subscope.put_semantic(ctx, *semantic, var);
                }
                subscope.bind_refs();

                let variant_vars = extern_facade_return_tys(ctx, concrete_variant.ty)
                    .into_iter()
                    .map(|ty| subscope.add_input(ctx, VarRequest { ty, location: self.location }))
                    .collect();
                let maybe_input =
                    extern_facade_expr(ctx, concrete_variant.ty, variant_vars, self.location)
                        .var(ctx, &mut subscope);
                let input = match maybe_input {
                    Ok(var) => var,
                    Err(err) => return lowering_flow_error_to_sealed_block(ctx, subscope, err),
                };
                let result = generators::EnumConstruct {
                    input,
                    variant: concrete_variant,
                    location: self.location,
                }
                .add(ctx, &mut subscope);
                Ok(subscope.goto_callsite(Some(result)))
            })
            .collect::<Maybe<_>>()
            .map_err(LoweringFlowError::Failed)?;

        let merged = merge_sealed(ctx, scope, sealed_blocks, self.location);
        let arms = zip_eq(concrete_variants, merged.blocks).collect();
        // Emit the statement.
        scope.push_finalized_statement(Statement::MatchExtern(StatementMatchExtern {
            function: self.function,
            inputs: self.inputs,
            arms,
        }));
        merged.expr?.var(ctx, scope)
    }
}

pub type LoweringResult<T> = Result<T, LoweringFlowError>;

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    // TODO(spapini): Rename since other variants are unreachable as well.
    /// The current computation is unreachable.
    Unreachable,
    Panic(VariableId),
    Return(VariableId),
}
impl LoweringFlowError {
    pub fn is_unreachable(&self) -> bool {
        match self {
            LoweringFlowError::Failed(_) => false,
            LoweringFlowError::Unreachable
            | LoweringFlowError::Panic(_)
            | LoweringFlowError::Return(_) => true,
        }
    }
}

/// Converts a lowering flow error to the appropriate block scope end, if possible.
pub fn lowering_flow_error_to_sealed_block(
    ctx: &mut LoweringContext<'_>,
    scope: BlockBuilder,
    err: LoweringFlowError,
) -> Maybe<SealedBlockBuilder> {
    match err {
        LoweringFlowError::Failed(diag_added) => Err(diag_added),
        LoweringFlowError::Unreachable => Ok(scope.unreachable().into()),
        LoweringFlowError::Return(return_var) => Ok(scope.ret(ctx, return_var)?.into()),
        LoweringFlowError::Panic(data_var) => Ok(scope.panic(ctx, data_var)?.into()),
    }
}
