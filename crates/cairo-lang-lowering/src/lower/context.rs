use std::sync::Arc;

use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::{Mutability, VarId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::{chain, zip_eq};

use super::lowered_expr_from_block_result;
use super::scope::{generators, BlockScope, BlockScopeEnd};
use super::variables::LivingVar;
use crate::blocks::StructuredBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnostics;
use crate::lower::external::{extern_facade_expr, extern_facade_return_tys};
use crate::lower::scope::BlockFlowMerger;
use crate::objects::Variable;
use crate::VariableId;

/// Builds a Lowering context.
pub struct LoweringContextBuilder<'db> {
    pub db: &'db dyn LoweringGroup,
    // TODO(yuval): change to FunctionWithBodyId
    pub free_function_id: FreeFunctionId,
    pub function_def: Arc<semantic::FreeFunctionDefinition>,
    /// Semantic signature for current function.
    pub signature: semantic::Signature,
    // TODO(spapini): Document. (excluding implicits).
    pub ref_params: Vec<semantic::VarId>,
    /// The available implicits in this function.
    pub implicits: Vec<semantic::TypeId>,
}
impl<'db> LoweringContextBuilder<'db> {
    // TODO(yuval): change to FunctionWithBodyId
    pub fn new(db: &'db dyn LoweringGroup, free_function_id: FreeFunctionId) -> Maybe<Self> {
        let function_def = db.free_function_definition(free_function_id)?;
        let signature = db.free_function_declaration_signature(free_function_id)?;
        let implicits =
            db.function_with_body_all_implicits_vec(FunctionWithBodyId::Free(free_function_id))?;
        let ref_params = signature
            .params
            .iter()
            .filter(|param| param.mutability == Mutability::Reference)
            .map(|param| VarId::Param(param.id))
            .collect();
        Ok(LoweringContextBuilder {
            db,
            free_function_id,
            function_def,
            signature,
            ref_params,
            implicits,
        })
    }
    pub fn ctx<'a: 'db>(&'a self) -> Maybe<LoweringContext<'db>> {
        let generic_params =
            self.db.free_function_declaration_generic_params(self.free_function_id)?;
        Ok(LoweringContext {
            db: self.db,
            function_def: &self.function_def,
            signature: &self.signature,
            diagnostics: LoweringDiagnostics::new(
                self.free_function_id.module_file(self.db.upcast()),
            ),
            variables: Arena::default(),
            blocks: StructuredBlocks::new(),
            semantic_defs: UnorderedHashMap::default(),
            ref_params: &self.ref_params,
            implicits: &self.implicits,
            lookup_context: ImplLookupContext {
                module_id: self.free_function_id.parent_module(self.db.upcast()),
                extra_modules: vec![],
                generic_params,
            },
            expr_formatter: ExprFormatter {
                db: self.db.upcast(),
                free_function_id: self.free_function_id,
            },
        })
    }
}

/// Context for the lowering phase of a free function.
pub struct LoweringContext<'db> {
    pub db: &'db dyn LoweringGroup,
    /// Semantic model for current function definition.
    pub function_def: &'db semantic::FreeFunctionDefinition,
    // Semantic signature for current function.
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
    pub fn new_var(&mut self, ty: semantic::TypeId) -> VariableId {
        let ty_info = self.db.type_info(self.lookup_context.clone(), ty).unwrap_or_default();
        self.variables.alloc(Variable {
            duplicatable: ty_info.duplicatable,
            droppable: ty_info.droppable,
            ty,
        })
    }
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
    pub stable_ptr: SyntaxStablePtrId,
}
impl LoweredExprExternEnum {
    pub fn var(
        self,
        ctx: &mut LoweringContext<'_>,
        scope: &mut BlockScope,
    ) -> Result<LivingVar, LoweringFlowError> {
        let function_id = self.function;

        let concrete_variants = ctx
            .db
            .concrete_enum_variants(self.concrete_enum_id)
            .map_err(LoweringFlowError::Failed)?;
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
                            subscope.put_implicit(ctx, *ty, output_var);
                        }
                        // Bind the ref variables.
                        for (semantic_var_id, output_var) in zip_eq(&self.ref_args, ref_outputs) {
                            subscope.put_semantic_variable(ctx, *semantic_var_id, output_var);
                        }
                        subscope.bind_refs();

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
        lowered_expr_from_block_result(ctx, scope, call_block_result, finalized_merger)?
            .var(ctx, scope)
    }
}

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed(DiagnosticAdded),
    /// The current computation is unreachable.
    Unreachable,
    Return {
        refs: Vec<LivingVar>,
        returns: Vec<LivingVar>,
    },
    Panic {
        refs: Vec<LivingVar>,
        data: LivingVar,
    },
}

/// Converts a lowering flow error the appropriate block scope end, if possible.
pub fn lowering_flow_error_to_block_scope_end(err: LoweringFlowError) -> Maybe<BlockScopeEnd> {
    match err {
        LoweringFlowError::Failed(diag_added) => Err(diag_added),
        LoweringFlowError::Unreachable => Ok(BlockScopeEnd::Unreachable),
        LoweringFlowError::Return { refs, returns } => Ok(BlockScopeEnd::Return { refs, returns }),
        LoweringFlowError::Panic { refs, data } => Ok(BlockScopeEnd::Panic { refs, data }),
    }
}
