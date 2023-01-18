use std::collections::VecDeque;

use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::{get_enum_concrete_variant, get_panic_ty};
use cairo_lang_semantic::GenericArgumentId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::Arena;
use itertools::chain;
use semantic::items::functions::GenericFunctionId;

use crate::blocks::{Blocks, FlatBlocks};
use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContext, LoweringContextBuilder, VarRequest};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, RefIndex, Statement, StatementCall,
    StatementEnumConstruct, StatementMatchEnum, StructuredBlock, StructuredBlockEnd,
    StructuredLowered, StructuredStatement, VarRemapping, Variable, VariableId,
};

/// Lowering phase that converts BlockEnd::Panic into BlockEnd::Return, and wraps necessary types
/// with PanicResult<>.
pub fn lower_panics(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    lowered: &StructuredLowered,
) -> Maybe<FlatLowered> {
    let lowering_info = LoweringContextBuilder::new(db, function_id)?;
    let mut ctx = lowering_info.ctx()?;
    ctx.variables = lowered.variables.clone();

    // Skip this phase for non panicable functions.
    if !db.function_with_body_may_panic(function_id)? {
        return Ok(FlatLowered {
            diagnostics: Default::default(),
            root: lowered.root,
            variables: ctx.variables,
            blocks: Blocks(
                lowered.blocks.0.iter().map(|block| block.clone().try_into().expect("")).collect(),
            ),
        });
    }

    // Prepare context.
    let func_ok_variant = get_enum_concrete_variant(
        db.upcast(),
        "PanicResult",
        vec![GenericArgumentId::Type(ctx.signature.return_type)],
        "Ok",
    );
    let func_err_variant = get_enum_concrete_variant(
        db.upcast(),
        "PanicResult",
        vec![GenericArgumentId::Type(ctx.signature.return_type)],
        "Err",
    );
    let panic_data_ty = func_err_variant.ty;
    let mut ctx = PanicLoweringContext {
        ctx,
        block_queue: VecDeque::from(lowered.blocks.0.clone()),
        flat_blocks: FlatBlocks::new(),
        func_ok_variant,
        func_err_variant,
        panic_data_ty,
    };

    // Iterate block queue (old and new blocks).
    while let Some(block) = ctx.block_queue.pop_front() {
        let mut block_ctx = PanicBlockLoweringContext {
            ctx,
            current_refs: block.initial_refs,
            statements: Vec::new(),
        };
        for stmt in block.statements {
            block_ctx.update_refs(&stmt.ref_updates);
            block_ctx.handle_statement(&stmt)?;
        }
        ctx = block_ctx.handle_end(block.inputs, block.end);
    }

    Ok(FlatLowered {
        diagnostics: Default::default(),
        variables: ctx.ctx.variables,
        blocks: ctx.flat_blocks,
        root: lowered.root,
    })
}

struct PanicLoweringContext<'a> {
    ctx: LoweringContext<'a>,
    block_queue: VecDeque<StructuredBlock>,
    flat_blocks: Blocks<FlatBlock>,
    func_ok_variant: semantic::ConcreteVariant,
    func_err_variant: semantic::ConcreteVariant,
    panic_data_ty: semantic::TypeId,
}
impl<'a> PanicLoweringContext<'a> {
    pub fn db(&self) -> &dyn LoweringGroup {
        self.ctx.db
    }

    fn enqueue_block(&mut self, block: StructuredBlock) -> BlockId {
        self.block_queue.push_back(block);
        BlockId(self.flat_blocks.len() + self.block_queue.len())
    }
}

struct PanicBlockLoweringContext<'a> {
    ctx: PanicLoweringContext<'a>,
    current_refs: Vec<VariableId>,
    statements: Vec<Statement>,
}
impl<'a> PanicBlockLoweringContext<'a> {
    pub fn db(&self) -> &dyn LoweringGroup {
        self.ctx.db()
    }
    pub fn variables_mut(&mut self) -> &mut Arena<Variable> {
        &mut self.ctx.ctx.variables
    }

    fn new_var(&mut self, req: VarRequest) -> VariableId {
        self.ctx.ctx.new_var(req)
    }

    fn update_refs(&mut self, ref_changes: &OrderedHashMap<RefIndex, VariableId>) {
        for (ref_index, var_id) in ref_changes.iter() {
            self.current_refs[ref_index.0] = *var_id;
        }
    }

    fn handle_statement(&mut self, stmt: &StructuredStatement) -> Maybe<()> {
        match &stmt.statement {
            crate::Statement::Call(call) => {
                let concerete_function = self.db().lookup_intern_function(call.function).function;
                match concerete_function.generic_function {
                    GenericFunctionId::Free(free_callee)
                        if self.db().function_with_body_may_panic(FunctionWithBodyId::Free(
                            free_callee,
                        ))? =>
                    {
                        self.handle_stmt_call(call)
                    }
                    GenericFunctionId::Impl(impl_callee)
                        if self.db().function_with_body_may_panic(FunctionWithBodyId::Impl(
                            impl_callee.function,
                        ))? =>
                    {
                        self.handle_stmt_call(call)
                    }
                    _ => {
                        self.statements.push(stmt.statement.clone());
                    }
                }
            }
            _ => {
                self.statements.push(stmt.statement.clone());
            }
        }
        Ok(())
    }

    fn handle_stmt_call(&mut self, call: &StatementCall) {
        // Extract return variable.
        let mut outputs = call.outputs.clone();
        let original_return_var = outputs.pop().unwrap();
        let ty = self.variables_mut()[original_return_var].ty;
        let location = self.ctx.ctx.variables[original_return_var].location;

        // Allocate 2 new variables.
        // panic_result_var - for the new return variable, with is actually of type PanicResult<ty>.
        let panic_result_var =
            self.new_var(VarRequest { ty: get_panic_ty(self.db().upcast(), ty), location });
        outputs.push(panic_result_var);
        // inner_ok_value - for the Ok() match arm input.
        let inner_ok_value = self.new_var(VarRequest { ty, location });

        // Emit the new statement.
        self.statements.push(Statement::Call(StatementCall {
            function: call.function,
            inputs: call.inputs.clone(),
            outputs,
        }));

        // Start constructing a match on the result.
        // Prepare variants.
        let ty_arg = vec![GenericArgumentId::Type(ty)];
        let call_ok_variant =
            get_enum_concrete_variant(self.db().upcast(), "PanicResult", ty_arg.clone(), "Ok");
        let call_err_variant =
            get_enum_concrete_variant(self.db().upcast(), "PanicResult", ty_arg, "Err");

        // Prepare Ok() match arm block.
        let block_ok = self.ctx.enqueue_block(StructuredBlock {
            initial_refs: self.current_refs.clone(),
            inputs: vec![inner_ok_value],
            statements: vec![],
            end: StructuredBlockEnd::Callsite(VarRemapping {
                remapping: [(original_return_var, inner_ok_value)].into_iter().collect(),
            }),
        });

        // Prepare Err() match arm block.
        let data_var = self.new_var(VarRequest { ty: self.ctx.panic_data_ty, location });
        let block_err = self.ctx.enqueue_block(StructuredBlock {
            initial_refs: self.current_refs.clone(),
            inputs: vec![data_var],
            statements: vec![],
            end: StructuredBlockEnd::Panic { refs: self.current_refs.clone(), data: data_var },
        });

        // Emit the match statement.
        self.statements.push(Statement::MatchEnum(StatementMatchEnum {
            concrete_enum_id: call_ok_variant.concrete_enum_id,
            input: panic_result_var,
            arms: vec![(call_ok_variant, block_ok), (call_err_variant, block_err)],
        }));
    }

    fn handle_end(
        mut self,
        inputs: Vec<VariableId>,
        end: StructuredBlockEnd,
    ) -> PanicLoweringContext<'a> {
        let end = match end {
            StructuredBlockEnd::Callsite(rets) => FlatBlockEnd::Callsite(rets),
            StructuredBlockEnd::Panic { refs, data } => {
                // Wrap with PanicResult::Err.
                let ty = self.db().intern_type(semantic::TypeLongId::Concrete(
                    semantic::ConcreteTypeId::Enum(self.ctx.func_err_variant.concrete_enum_id),
                ));
                let location = self.ctx.ctx.variables[data].location;
                let output = self.new_var(VarRequest { ty, location });
                self.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: self.ctx.func_err_variant.clone(),
                    input: data,
                    output,
                }));
                FlatBlockEnd::Return(chain!(refs, [output]).collect())
            }
            StructuredBlockEnd::Return { refs, returns } => {
                assert_eq!(
                    returns.len(),
                    1,
                    "Panicable functions should be non-extern and thus have only 1 expr variables"
                );
                let inner_value_var = returns.into_iter().next().unwrap();
                let location = self.ctx.ctx.variables[inner_value_var].location;

                // Wrap with PanicResult::Ok.
                let ty = self.db().intern_type(semantic::TypeLongId::Concrete(
                    semantic::ConcreteTypeId::Enum(self.ctx.func_ok_variant.concrete_enum_id),
                ));
                let output = self.new_var(VarRequest { ty, location });
                self.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: self.ctx.func_ok_variant.clone(),
                    input: inner_value_var,
                    output,
                }));
                FlatBlockEnd::Return(chain!(refs, [output]).collect())
            }
            StructuredBlockEnd::Unreachable => FlatBlockEnd::Unreachable,
        };
        self.ctx.flat_blocks.alloc(FlatBlock { inputs, statements: self.statements, end });
        self.ctx
    }
}
