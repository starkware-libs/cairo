use std::collections::VecDeque;

use cairo_defs::ids::{FreeFunctionId, GenericFunctionId};
use cairo_diagnostics::Maybe;
use cairo_semantic::corelib::{get_enum_concrete_variant, get_panic_ty};
use cairo_semantic::GenericArgumentId;

use crate::db::LoweringGroup;
use crate::lower::context::LoweringContextBuilder;
use crate::lower::Lowered;
use crate::{
    Block, BlockEnd, BlockId, Statement, StatementCall, StatementEnumConstruct, StatementMatchEnum,
};

pub fn lower_panics(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
    lowered: Lowered,
) -> Maybe<Lowered> {
    if !db.free_function_may_panic(free_function_id)? {
        // The function cannot panic. Nothing to do.
        return Ok(lowered);
    }

    let lowering_info = LoweringContextBuilder::new(db, free_function_id)?;
    let mut ctx = lowering_info.ctx()?;
    ctx.variables = lowered.variables;
    let mut block_queue = VecDeque::from(lowered.blocks.0);

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
    let err_ty = func_err_variant.ty;

    while let Some(block) = block_queue.pop_front() {
        let mut statements = Vec::new();
        for stmt in block.statements {
            match &stmt {
                crate::Statement::Call(call) => {
                    let concerete_function = ctx.db.lookup_intern_function(call.function).function;
                    match concerete_function.generic_function {
                        GenericFunctionId::Free(callee)
                            if db.free_function_may_panic(callee)? =>
                        {
                            // Replace output variable with a new PanicResult variable.
                            let mut outputs = call.outputs.clone();
                            let inner_value_var = outputs.pop().unwrap();
                            let ty = ctx.variables[inner_value_var].ty;
                            let panic_result_var = ctx.new_var(get_panic_ty(ctx.db.upcast(), ty));
                            outputs.push(panic_result_var);
                            statements.push(Statement::Call(StatementCall {
                                function: call.function,
                                inputs: call.inputs.clone(),
                                outputs,
                            }));

                            // Prepare variants.
                            let call_ok_variant = get_enum_concrete_variant(
                                db.upcast(),
                                "PanicResult",
                                vec![GenericArgumentId::Type(ty)],
                                "Ok",
                            );
                            let call_err_variant = get_enum_concrete_variant(
                                db.upcast(),
                                "PanicResult",
                                vec![GenericArgumentId::Type(ty)],
                                "Err",
                            );

                            // Add match statement.
                            // TODO: `drops` are wrong. Remove them.
                            block_queue.push_back(Block {
                                inputs: vec![inner_value_var],
                                statements: vec![],
                                drops: vec![],
                                end: BlockEnd::Callsite(vec![]),
                            });
                            let block_ok = BlockId(ctx.blocks.len() + block_queue.len());

                            let data_var = ctx.new_var(err_ty);
                            // TODO: `drops` are wrong. Remove them.
                            // TODO: Fix refs.
                            block_queue.push_back(Block {
                                inputs: vec![data_var],
                                statements: vec![],
                                drops: vec![],
                                end: BlockEnd::Panic { refs: vec![], data: data_var },
                            });
                            let block_err = BlockId(ctx.blocks.len() + block_queue.len());

                            statements.push(Statement::MatchEnum(StatementMatchEnum {
                                concrete_enum: call_ok_variant.concrete_enum_id,
                                input: panic_result_var,
                                arms: vec![
                                    (call_ok_variant, block_ok),
                                    (call_err_variant, block_err),
                                ],
                                outputs: vec![],
                            }));
                        }
                        _ => statements.push(stmt),
                    }
                }
                _ => statements.push(stmt),
            }
        }
        let end = match block.end {
            BlockEnd::Callsite(_) => block.end,
            BlockEnd::Panic { refs, data } => {
                // Wrap with PanicResult::Err.
                let output = ctx.new_var(ctx.db.intern_type(cairo_semantic::TypeLongId::Concrete(
                    cairo_semantic::ConcreteTypeId::Enum(func_err_variant.concrete_enum_id),
                )));
                statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: func_err_variant.clone(),
                    input: data,
                    output,
                }));
                BlockEnd::Return { refs, returns: vec![output] }
            }
            BlockEnd::Return { refs, returns } => {
                assert_eq!(
                    returns.len(),
                    1,
                    "Panicable functions should be non-extern and thus have only 1 expr variables"
                );
                let inner_value_var = returns.into_iter().next().unwrap();

                // Wrap with PanicResult::Ok.
                let output = ctx.new_var(ctx.db.intern_type(cairo_semantic::TypeLongId::Concrete(
                    cairo_semantic::ConcreteTypeId::Enum(func_ok_variant.concrete_enum_id),
                )));
                statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: func_ok_variant.clone(),
                    input: inner_value_var,
                    output,
                }));
                BlockEnd::Return { refs, returns: vec![output] }
            }
            BlockEnd::Unreachable => block.end,
        };
        // TODO: `drops` are wrong. Remove them.
        ctx.blocks.alloc(Block { inputs: block.inputs, statements, end, drops: vec![] });
    }

    Ok(Lowered { variables: ctx.variables, blocks: ctx.blocks, ..lowered })
}
