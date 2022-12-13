use diagnostics::{DiagnosticAdded, Maybe};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use semantic::expr::fmt::ExprFormatter;
use semantic::items::enm::SemanticEnumEx;
use semantic::items::imp::ImplLookupContext;
use utils::unordered_hash_map::UnorderedHashMap;

use super::lowered_expr_from_block_result;
use super::scope::{generators, BlockScope, BlockScopeEnd};
use super::variables::LivingVar;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::lower::scope::BlockFlowMerger;
use crate::objects::{Block, Variable};

/// Context for the lowering phase of a free function.
pub struct LoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Semantic model for current function definition.
    pub function_def: &'db semantic::FreeFunctionDefinition,
    // Semantic signature for current function.
    pub signature: semantic::Signature,
    /// Whether the current function may panic.
    pub may_panic: bool,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
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

/// Representation of the value of a computed expression.
#[derive(Debug)]
pub enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(LivingVar),
    /// The expression value is a tuple.
    Tuple(Vec<LoweredExpr>),
    /// The expression value is an enum result from an extern call.
    ExternEnum(LoweredExprExternEnum),
}
impl LoweredExpr {
    pub fn var(
        self,
        ctx: &mut LoweringContext<'_>,
        scope: &mut BlockScope,
    ) -> Result<LivingVar, LoweringFlowError> {
        match self {
            LoweredExpr::AtVariable(var_id) => Ok(var_id),
            LoweredExpr::Tuple(exprs) => {
                let inputs: Vec<_> = exprs
                    .into_iter()
                    .map(|expr| expr.var(ctx, scope))
                    .collect::<Result<Vec<_>, _>>()?;
                let tys = inputs.iter().map(|var| ctx.variables[var.var_id()].ty).collect();
                let ty = ctx.db.intern_type(semantic::TypeLongId::Tuple(tys));
                Ok(generators::StructConstruct { inputs, ty }.add(ctx, scope))
            }
            LoweredExpr::ExternEnum(extern_enum) => extern_enum.var(ctx, scope),
        }
    }
    pub fn ty(&self, ctx: &mut LoweringContext<'_>) -> semantic::TypeId {
        match self {
            LoweredExpr::AtVariable(var) => ctx.variables[var.var_id()].ty,
            LoweredExpr::Tuple(exprs) => ctx.db.intern_type(semantic::TypeLongId::Tuple(
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
    pub inputs: Vec<LivingVar>,
    pub ref_args: Vec<semantic::VarId>,
    /// The implicits used/changed by the function.
    pub implicits: Vec<semantic::TypeId>,
}
impl LoweredExprExternEnum {
    pub fn var(
        self,
        ctx: &mut LoweringContext<'_>,
        scope: &mut BlockScope,
    ) -> Result<LivingVar, LoweringFlowError> {
        let function_id = self.function;

        let concrete_variants = ctx.db.concrete_enum_variants(self.concrete_enum_id).unwrap();
        let (blocks, mut finalized_merger) =
            BlockFlowMerger::with(ctx, scope, &self.ref_args, |ctx, merger| {
                let block_opts = concrete_variants.clone().into_iter().map(|concrete_variant| {
                    let variant_input_tys = extern_facade_return_tys(ctx, concrete_variant.ty);
                    let ref_tys = self
                        .ref_args
                        .iter()
                        .map(|semantic_var_id| ctx.semantic_defs[*semantic_var_id].ty());
                    let input_tys = chain!(
                        self.implicits.iter().cloned(),
                        ref_tys,
                        variant_input_tys.into_iter()
                    )
                    .collect();
                    merger.run_in_subscope(ctx, input_tys, |ctx, subscope, mut arm_inputs| {
                        let implicit_outputs: Vec<_> =
                            arm_inputs.drain(0..self.implicits.len()).collect();
                        let ref_outputs: Vec<_> =
                            arm_inputs.drain(0..self.ref_args.len()).collect();
                        let result = extern_facade_expr(ctx, concrete_variant.ty, arm_inputs)
                            .var(ctx, subscope)
                            .map(|input| {
                                Ok(BlockScopeEnd::Callsite(Some(
                                    generators::EnumConstruct { input, variant: concrete_variant }
                                        .add(ctx, subscope),
                                )))
                            })
                            .unwrap_or_else(lowering_flow_error_to_block_scope_end)?;

                        // Bind implicits.
                        for (ty, output_var) in zip_eq(&self.implicits, implicit_outputs) {
                            subscope.put_implicit(*ty, output_var);
                        }
                        // Bind the ref variables.
                        for (semantic_var_id, output_var) in zip_eq(&self.ref_args, ref_outputs) {
                            subscope.put_semantic_variable(*semantic_var_id, output_var);
                        }

                        Ok(result)
                    })
                });
                block_opts.collect::<Maybe<Vec<_>>>().map_err(LoweringFlowError::Failed)
            });

        let finalized_blocks: Vec<_> = blocks?
            .into_iter()
            .map(|sealed| finalized_merger.finalize_block(ctx, sealed).block)
            .collect();
        let arms = zip_eq(concrete_variants, finalized_blocks).collect();

        let call_block_result = generators::MatchExtern {
            function: function_id,
            inputs: self.inputs,
            arms,
            end_info: finalized_merger.end_info.clone(),
        }
        .add(ctx, scope);
        lowered_expr_from_block_result(scope, call_block_result, finalized_merger)?.var(ctx, scope)
    }
}

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    /// The current computation is unreachable.
    Unreachable,
    Return(Vec<LivingVar>),
}

/// Converts a lowering flow error the appropriate block scope end, if possible.
pub fn lowering_flow_error_to_block_scope_end(err: LoweringFlowError) -> Maybe<BlockScopeEnd> {
    match err {
        LoweringFlowError::Failed(diag_added) => Err(diag_added),
        LoweringFlowError::Unreachable => Ok(BlockScopeEnd::Unreachable),
        LoweringFlowError::Return(return_vars) => Ok(BlockScopeEnd::Return(return_vars)),
    }
}

/// Cases where the flow of lowering a statement should halt.
pub enum StatementLoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    /// The block should end after this statement.
    End(BlockScopeEnd),
}
impl From<LoweringFlowError> for StatementLoweringFlowError {
    fn from(err: LoweringFlowError) -> Self {
        match err {
            LoweringFlowError::Failed(diag_added) => StatementLoweringFlowError::Failed(diag_added),
            LoweringFlowError::Unreachable => {
                StatementLoweringFlowError::End(BlockScopeEnd::Unreachable)
            }
            LoweringFlowError::Return(return_vars) => {
                StatementLoweringFlowError::End(BlockScopeEnd::Return(return_vars))
            }
        }
    }
}
