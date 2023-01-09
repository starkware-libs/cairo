use std::collections::VecDeque;

use cairo_lang_defs::ids::{FreeFunctionId, GenericFunctionId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::{get_enum_concrete_variant, get_panic_ty};
use cairo_lang_semantic::GenericArgumentId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::chain;

use crate::blocks::{Blocks, FlatBlocks};
use crate::db::LoweringGroup;
use crate::lower::context::LoweringContextBuilder;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, RefIndex, Statement, StatementCall,
    StatementEnumConstruct, StatementMatchEnum, StructuredBlock, StructuredBlockEnd,
    StructuredLowered, VariableId,
};

pub fn lower_panics(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
    lowered: &StructuredLowered,
) -> Maybe<FlatLowered> {
    let lowering_info = LoweringContextBuilder::new(db, free_function_id)?;
    let mut ctx = lowering_info.ctx()?;
    ctx.variables = lowered.variables.clone();
    let mut block_queue = VecDeque::from(lowered.blocks.0.clone());

    if !db.free_function_may_panic(free_function_id)? {
        return Ok(FlatLowered {
            root: lowered.root,
            variables: ctx.variables,
            blocks: Blocks(
                lowered.blocks.0.iter().map(|block| block.clone().try_into().expect("")).collect(),
            ),
        });
    }

    let mut flat_blocks = FlatBlocks::new();
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
        let mut current_refs = block.initial_refs;
        for stmt in block.statements {
            update_refs(&mut current_refs[..], stmt.ref_updates);
            match &stmt.statement {
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
                            let inner_ok_value = ctx.new_var(ty);
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
                            block_queue.push_back(StructuredBlock {
                                initial_refs: current_refs.clone(),
                                inputs: vec![inner_ok_value],
                                statements: vec![],
                                end: StructuredBlockEnd::Callsite(vec![inner_ok_value]),
                            });
                            let block_ok = BlockId(flat_blocks.len() + block_queue.len());

                            let data_var = ctx.new_var(err_ty);
                            // TODO: Fix refs.
                            block_queue.push_back(StructuredBlock {
                                initial_refs: current_refs.clone(),
                                inputs: vec![data_var],
                                statements: vec![],
                                end: StructuredBlockEnd::Panic {
                                    refs: current_refs.clone(),
                                    data: data_var,
                                },
                            });
                            let block_err = BlockId(flat_blocks.len() + block_queue.len());

                            statements.push(Statement::MatchEnum(StatementMatchEnum {
                                concrete_enum: call_ok_variant.concrete_enum_id,
                                input: panic_result_var,
                                arms: vec![
                                    (call_ok_variant, block_ok),
                                    (call_err_variant, block_err),
                                ],
                                outputs: vec![inner_value_var],
                            }));
                        }
                        _ => {
                            statements.push(stmt.statement);
                        }
                    }
                }
                _ => {
                    statements.push(stmt.statement);
                }
            }
        }
        let end = match block.end {
            StructuredBlockEnd::Callsite(rets) => FlatBlockEnd::Callsite(rets),
            StructuredBlockEnd::Panic { refs, data } => {
                // Wrap with PanicResult::Err.
                let output = ctx.new_var(ctx.db.intern_type(semantic::TypeLongId::Concrete(
                    semantic::ConcreteTypeId::Enum(func_err_variant.concrete_enum_id),
                )));
                statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: func_err_variant.clone(),
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
                let output = ctx.new_var(ctx.db.intern_type(semantic::TypeLongId::Concrete(
                    semantic::ConcreteTypeId::Enum(func_ok_variant.concrete_enum_id),
                )));
                statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: func_ok_variant.clone(),
                    input: inner_value_var,
                    output,
                }));
                FlatBlockEnd::Return(chain!(refs, [output]).collect())
            }
            StructuredBlockEnd::Unreachable => FlatBlockEnd::Unreachable,
        };
        // TODO: `drops` are wrong. Remove them.
        flat_blocks.alloc(FlatBlock { inputs: block.inputs, statements, end });
    }

    Ok(FlatLowered { variables: ctx.variables, blocks: flat_blocks, root: lowered.root })
}

fn update_refs(current_refs: &mut [VariableId], ref_changes: OrderedHashMap<RefIndex, VariableId>) {
    for (ref_index, var_id) in ref_changes.iter() {
        current_refs[ref_index.0] = *var_id;
    }
}
