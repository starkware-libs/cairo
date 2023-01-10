use std::collections::VecDeque;

use cairo_lang_defs::ids::{FreeFunctionId, GenericFunctionId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::{get_enum_concrete_variant, get_panic_ty};
use cairo_lang_semantic::GenericArgumentId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use id_arena::Arena;
use itertools::chain;

use crate::blocks::{Blocks, FlatBlocks};
use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContext, LoweringContextBuilder};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, RefIndex, Statement, StatementCall,
    StatementEnumConstruct, StatementMatchEnum, StructuredBlock, StructuredBlockEnd,
    StructuredLowered, StructuredStatement, Variable, VariableId,
};

/// Lowering phase that converts BlockEnd::Panic into BlockEnd::Return, and wraps necessary types
/// with PanicResult<>.
pub fn lower_panics(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
    lowered: &StructuredLowered,
) -> Maybe<FlatLowered> {
    let lowering_info = LoweringContextBuilder::new(db, free_function_id)?;
    let mut ctx = lowering_info.ctx()?;
    ctx.variables = lowered.variables.clone();

    // Skip this phase for non panicable functions.
    if !db.free_function_may_panic(free_function_id)? {
        return Ok(FlatLowered {
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

    Ok(FlatLowered { variables: ctx.ctx.variables, blocks: ctx.flat_blocks, root: lowered.root })
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

    fn new_var(&mut self, ty: semantic::TypeId) -> VariableId {
        self.ctx.ctx.new_var(ty)
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
                    GenericFunctionId::Free(callee)
                        if self.db().free_function_may_panic(callee)? =>
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

        // Allocate 2 new variables.
        // panic_result_var - for the new return variable, with is actually of type PanicResult<ty>.
        let panic_result_var = self.new_var(get_panic_ty(self.db().upcast(), ty));
        outputs.push(panic_result_var);
        // inner_ok_value - for the Ok() match arm input.
        let inner_ok_value = self.new_var(ty);

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
            end: StructuredBlockEnd::Callsite(vec![inner_ok_value]),
        });

        // Prepare Err() match arm block.
        let data_var = self.new_var(self.ctx.panic_data_ty);
        let block_err = self.ctx.enqueue_block(StructuredBlock {
            initial_refs: self.current_refs.clone(),
            inputs: vec![data_var],
            statements: vec![],
            end: StructuredBlockEnd::Panic { refs: self.current_refs.clone(), data: data_var },
        });

        // Emit the match statement.
        self.statements.push(Statement::MatchEnum(StatementMatchEnum {
            concrete_enum: call_ok_variant.concrete_enum_id,
            input: panic_result_var,
            arms: vec![(call_ok_variant, block_ok), (call_err_variant, block_err)],
            outputs: vec![original_return_var],
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
                let output = self.new_var(self.db().intern_type(semantic::TypeLongId::Concrete(
                    semantic::ConcreteTypeId::Enum(self.ctx.func_err_variant.concrete_enum_id),
                )));
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

                // Wrap with PanicResult::Ok.
                let output = self.new_var(self.db().intern_type(semantic::TypeLongId::Concrete(
                    semantic::ConcreteTypeId::Enum(self.ctx.func_ok_variant.concrete_enum_id),
                )));
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
