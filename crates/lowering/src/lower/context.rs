use id_arena::Arena;
use itertools::{chain, zip_eq};
use semantic::items::enm::SemanticEnumEx;
use semantic::items::imp::ImplLookupContext;
use utils::unordered_hash_map::UnorderedHashMap;

use super::scope::generators::CallBlockResult;
use super::scope::{generators, BlockScope, BlockScopeEnd};
use super::variables::LivingVar;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::lower::scope::BlockFlowMerger;
use crate::objects::{Block, Variable};

/// Context for the lowering phase.
pub struct LoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Semantic model for current function definition.
    pub function_def: &'db semantic::FreeFunctionDefinition,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
    /// Definitions encountered for semantic variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
    // TODO(spapini): Document.
    pub ref_params: &'db [semantic::VarId],
    // Lookup context for impls.
    pub lookup_context: ImplLookupContext,
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
    pub fn var(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        match self {
            LoweredExpr::AtVariable(var_id) => var_id,
            LoweredExpr::Tuple(exprs) => {
                let inputs: Vec<_> = exprs.into_iter().map(|expr| expr.var(ctx, scope)).collect();
                let tys = inputs.iter().map(|var| ctx.variables[var.var_id()].ty).collect();
                let ty = ctx.db.intern_type(semantic::TypeLongId::Tuple(tys));
                generators::StructConstruct { inputs, ty }.add(ctx, scope)
            }
            LoweredExpr::ExternEnum(extern_enum) => extern_enum.var(ctx, scope),
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
}
impl LoweredExprExternEnum {
    pub fn var(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        let concrete_variants = ctx.db.concrete_enum_variants(self.concrete_enum_id).unwrap();
        let (blocks, mut finalized_merger) =
            BlockFlowMerger::with(ctx, scope, &self.ref_args, |ctx, merger| {
                let block_opts = concrete_variants.into_iter().map(|concrete_variant| {
                    let variant_input_tys = extern_facade_return_tys(ctx, concrete_variant.ty);
                    let ref_tys = self
                        .ref_args
                        .iter()
                        .map(|semantic_var_id| ctx.semantic_defs[*semantic_var_id].ty());
                    let input_tys = chain!(ref_tys, variant_input_tys.into_iter()).collect();
                    merger.run_in_subscope(ctx, input_tys, |ctx, subscope, mut arm_inputs| {
                        let ref_outputs: Vec<_> =
                            arm_inputs.drain(0..self.ref_args.len()).collect();
                        let input = extern_facade_expr(ctx, concrete_variant.ty, arm_inputs)
                            .var(ctx, subscope);
                        let res = generators::EnumConstruct { input, variant: concrete_variant }
                            .add(ctx, subscope);

                        // Rebind the ref variables.
                        for (semantic_var_id, output_var) in zip_eq(&self.ref_args, ref_outputs) {
                            subscope.put_semantic_variable(*semantic_var_id, output_var);
                        }

                        Some(BlockScopeEnd::Callsite(Some(res)))
                    })
                });
                block_opts.collect::<Option<Vec<_>>>().ok_or(LoweringFlowError::Failed)
            });
        let arms = blocks
            .unwrap()
            .into_iter()
            .map(|sealed| finalized_merger.finalize_block(ctx, sealed).block)
            .collect();
        let call_block_result = generators::MatchExtern {
            function: self.function,
            inputs: self.inputs,
            arms,
            end_info: finalized_merger.end_info,
        }
        .add(ctx, scope);
        if let CallBlockResult::Callsite { pushes, maybe_output: Some(output) } = call_block_result
        {
            for (semantic_var_id, output_var) in zip_eq(&self.ref_args, pushes) {
                scope.put_semantic_variable(*semantic_var_id, output_var);
            }
            output
        } else {
            todo!("Handle empty enums.");
        }
    }
}

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed,
    /// The current computation is unreachable.
    Unreachable,
}
/// Cases where the flow of lowering a statement should halt.
pub enum StatementLoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed,
    /// The block should end after this statement.
    End(BlockScopeEnd),
}
impl From<LoweringFlowError> for StatementLoweringFlowError {
    fn from(err: LoweringFlowError) -> Self {
        match err {
            LoweringFlowError::Failed => StatementLoweringFlowError::Failed,
            LoweringFlowError::Unreachable => {
                StatementLoweringFlowError::End(BlockScopeEnd::Unreachable)
            }
        }
    }
}
