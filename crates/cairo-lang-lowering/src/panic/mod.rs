use std::collections::VecDeque;

use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::{get_enum_concrete_variant, get_panic_ty};
use cairo_lang_semantic::GenericArgumentId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, zip_eq, Itertools};
use semantic::items::functions::GenericFunctionId;
use semantic::{ConcreteVariant, Mutability, Signature, TypeId};

use crate::blocks::{Blocks, FlatBlocks};
use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContext, LoweringContextBuilder, VarRequest};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, RefIndex, Statement,
    StatementCall, StatementEnumConstruct, StatementStructConstruct, StatementStructDestructure,
    StructuredBlock, StructuredBlockEnd, StructuredLowered, StructuredStatement, VarRemapping,
    VariableId,
};

// TODO(spapini): Remove tuple in the Ok() variant of the panic, by supporting multiple values in
// the Sierra type.

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
            variables: ctx.variables,
            blocks: Blocks(
                lowered.blocks.0.iter().map(|block| block.clone().try_into().expect("")).collect(),
            ),
        });
    }

    let panic_info = PanicSignatureInfo::new(db, ctx.signature);
    let mut ctx = PanicLoweringContext {
        ctx,
        block_queue: VecDeque::from(lowered.blocks.0.clone()),
        flat_blocks: FlatBlocks::new(),
        panic_info,
    };

    // Iterate block queue (old and new blocks).
    while let Some(block) = ctx.block_queue.pop_front() {
        ctx = handle_block(ctx, block)?;
    }

    Ok(FlatLowered {
        diagnostics: Default::default(),
        variables: ctx.ctx.variables,
        blocks: ctx.flat_blocks,
    })
}

/// Handles the lowering of panics in a single block.
fn handle_block(
    mut ctx: PanicLoweringContext<'_>,
    mut block: StructuredBlock,
) -> Maybe<PanicLoweringContext<'_>> {
    let mut block_ctx = PanicBlockLoweringContext {
        ctx,
        current_implicits: block.initial_implicits,
        statements: Vec::new(),
    };
    for (i, stmt) in block.statements.iter().cloned().enumerate() {
        block_ctx.update_refs(&stmt.implicit_updates);
        if let Some((continuation_block, cur_block_end)) = block_ctx.handle_statement(&stmt)? {
            // This case means that the lowering should split the block here.

            // Block ended with a match.
            ctx = block_ctx.handle_end(block.inputs, cur_block_end);

            // The rest of the statements in this block have not been handled yet, and should be
            // handles as a part of the continuation block - the second block in the "split".
            let block_to_edit = &mut ctx.block_queue[continuation_block.0 - ctx.flat_blocks.len()];
            block_to_edit.statements.extend(block.statements.drain(i + 1..));
            block_to_edit.end = block.end;
            return Ok(ctx);
        }
    }
    ctx = block_ctx.handle_end(block.inputs, block.end);
    Ok(ctx)
}

pub struct PanicSignatureInfo {
    /// The types of all the variables returned on OK: Reference variables and the original result.
    ok_ret_tys: Vec<TypeId>,
    /// The type of the Ok() variant.
    ok_ty: TypeId,
    /// The Ok() variant.
    ok_variant: ConcreteVariant,
    /// The Err() variant.
    err_variant: ConcreteVariant,
    /// The PanicResult concrete type - the new return type of the function.
    pub panic_ty: TypeId,
}
impl PanicSignatureInfo {
    pub fn new(db: &dyn LoweringGroup, signature: &Signature) -> Self {
        let refs = signature
            .params
            .iter()
            .filter(|param| matches!(param.mutability, Mutability::Reference))
            .map(|param| param.ty);
        let original_return_ty = signature.return_type;

        let ok_ret_tys = chain!(refs, [original_return_ty]).collect_vec();
        let ok_ty = db.intern_type(semantic::TypeLongId::Tuple(ok_ret_tys.clone()));
        let ok_variant = get_enum_concrete_variant(
            db.upcast(),
            "PanicResult",
            vec![GenericArgumentId::Type(ok_ty)],
            "Ok",
        );
        let err_variant = get_enum_concrete_variant(
            db.upcast(),
            "PanicResult",
            vec![GenericArgumentId::Type(ok_ty)],
            "Err",
        );
        let panic_ty = get_panic_ty(db.upcast(), ok_ty);
        Self { ok_ret_tys, ok_ty, ok_variant, err_variant, panic_ty }
    }
}

struct PanicLoweringContext<'a> {
    ctx: LoweringContext<'a>,
    block_queue: VecDeque<StructuredBlock>,
    flat_blocks: Blocks<FlatBlock>,
    panic_info: PanicSignatureInfo,
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
    current_implicits: Vec<VariableId>,
    statements: Vec<Statement>,
}
impl<'a> PanicBlockLoweringContext<'a> {
    pub fn db(&self) -> &dyn LoweringGroup {
        self.ctx.db()
    }

    fn new_var(&mut self, req: VarRequest) -> VariableId {
        self.ctx.ctx.new_var(req)
    }

    fn update_refs(&mut self, ref_changes: &OrderedHashMap<RefIndex, VariableId>) {
        for (ref_index, var_id) in ref_changes.iter() {
            self.current_implicits[ref_index.0] = *var_id;
        }
    }

    /// Handles a statement. If needed, returns the continuation block and the block end for this
    /// block.
    /// The continuation block happens when a panic match is added, and the block needs to be split.
    /// The continuation block is the second block in the "split". This function already partially
    /// creates this second block, and returns it.
    fn handle_statement(
        &mut self,
        stmt: &StructuredStatement,
    ) -> Maybe<Option<(BlockId, StructuredBlockEnd)>> {
        match &stmt.statement {
            Statement::Call(call) => {
                let concerete_function = self.db().lookup_intern_function(call.function).function;
                match concerete_function.generic_function {
                    GenericFunctionId::Free(free_callee)
                        if self.db().function_with_body_may_panic(FunctionWithBodyId::Free(
                            free_callee,
                        ))? =>
                    {
                        return Ok(Some(self.handle_call_panic(call)?));
                    }
                    GenericFunctionId::Impl(impl_callee)
                        if self.db().function_with_body_may_panic(FunctionWithBodyId::Impl(
                            impl_callee.function,
                        ))? =>
                    {
                        return Ok(Some(self.handle_call_panic(call)?));
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
        Ok(None)
    }

    /// Handles a call statement to a panicking function.
    /// Returns the continuation block ID for the caller to complete it, and the block end to set
    /// for the current block.
    fn handle_call_panic(&mut self, call: &StatementCall) -> Maybe<(BlockId, StructuredBlockEnd)> {
        // Extract return variable.
        let mut original_outputs = call.outputs.clone();
        let location = self.ctx.ctx.variables[original_outputs[0]].location;

        // Get callee info.
        let callee_signature = self.ctx.ctx.db.concrete_function_signature(call.function)?;
        let callee_info = PanicSignatureInfo::new(self.ctx.ctx.db, &callee_signature);

        // Allocate 2 new variables.
        // panic_result_var - for the new return variable, with is actually of type PanicResult<ty>.
        let panic_result_var = self.new_var(VarRequest { ty: callee_info.panic_ty, location });
        let n_callee_implicits = original_outputs.len() - callee_info.ok_ret_tys.len();
        let mut call_outputs = original_outputs.drain(..n_callee_implicits).collect_vec();
        call_outputs.push(panic_result_var);
        // inner_ok_value - for the Ok() match arm input.
        let inner_ok_value = self.new_var(VarRequest { ty: callee_info.ok_ty, location });
        // inner_ok_values - for the destructure.
        let inner_ok_values = callee_info
            .ok_ret_tys
            .iter()
            .copied()
            .map(|ty| self.new_var(VarRequest { ty, location }))
            .collect_vec();

        // Emit the new statement.
        self.statements.push(Statement::Call(StatementCall {
            function: call.function,
            inputs: call.inputs.clone(),
            outputs: call_outputs,
            location,
        }));

        // Start constructing a match on the result.
        let block_continuation = self.ctx.enqueue_block(StructuredBlock {
            initial_implicits: self.current_implicits.clone(),
            inputs: vec![],
            statements: vec![],
            end: StructuredBlockEnd::NotSet,
        });

        // Prepare Ok() match arm block. This block will be the continuation block.
        // This block is only partially created. It is returned at this function to let the caller
        // complete it.
        let block_ok = self.ctx.enqueue_block(StructuredBlock {
            initial_implicits: self.current_implicits.clone(),
            inputs: vec![inner_ok_value],
            statements: vec![StructuredStatement {
                statement: Statement::StructDestructure(StatementStructDestructure {
                    input: inner_ok_value,
                    outputs: inner_ok_values.clone(),
                }),
                implicit_updates: Default::default(),
            }],
            end: StructuredBlockEnd::Goto {
                target: block_continuation,
                remapping: VarRemapping {
                    remapping: zip_eq(original_outputs, inner_ok_values).collect(),
                },
            },
        });

        // Prepare Err() match arm block.
        let data_var =
            self.new_var(VarRequest { ty: self.ctx.panic_info.err_variant.ty, location });
        let block_err = self.ctx.enqueue_block(StructuredBlock {
            initial_implicits: self.current_implicits.clone(),
            inputs: vec![data_var],
            statements: vec![],
            end: StructuredBlockEnd::Panic {
                implicits: self.current_implicits.clone(),
                data: data_var,
            },
        });

        let cur_block_end = StructuredBlockEnd::Match {
            info: MatchInfo::Enum(MatchEnumInfo {
                concrete_enum_id: callee_info.ok_variant.concrete_enum_id,
                input: panic_result_var,
                arms: vec![
                    (callee_info.ok_variant, block_ok),
                    (callee_info.err_variant, block_err),
                ],
            }),
        };

        Ok((block_continuation, cur_block_end))
    }

    fn handle_end(
        mut self,
        inputs: Vec<VariableId>,
        end: StructuredBlockEnd,
    ) -> PanicLoweringContext<'a> {
        let end = match end {
            StructuredBlockEnd::Goto { target, remapping } => FlatBlockEnd::Goto(target, remapping),
            StructuredBlockEnd::Panic { implicits, data } => {
                // Wrap with PanicResult::Err.
                let ty = self.ctx.panic_info.panic_ty;
                let location = self.ctx.ctx.variables[data].location;
                let output = self.new_var(VarRequest { ty, location });
                self.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: self.ctx.panic_info.err_variant.clone(),
                    input: data,
                    output,
                }));
                FlatBlockEnd::Return(chain!(implicits, [output]).collect())
            }
            StructuredBlockEnd::Return { implicits, returns } => {
                let location = self.ctx.ctx.variables[returns[0]].location;

                // Tuple construction.
                let tupled_res =
                    self.new_var(VarRequest { ty: self.ctx.panic_info.ok_ty, location });
                self.statements.push(Statement::StructConstruct(StatementStructConstruct {
                    inputs: returns,
                    output: tupled_res,
                }));

                // Wrap with PanicResult::Ok.
                let ty = self.ctx.panic_info.panic_ty;
                let output = self.new_var(VarRequest { ty, location });
                self.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: self.ctx.panic_info.ok_variant.clone(),
                    input: tupled_res,
                    output,
                }));
                FlatBlockEnd::Return(chain!(implicits, [output]).collect())
            }
            StructuredBlockEnd::NotSet => unreachable!(),
            StructuredBlockEnd::Match { info } => FlatBlockEnd::Match { info },
        };
        self.ctx.flat_blocks.alloc(FlatBlock { inputs, statements: self.statements, end });
        self.ctx
    }
}
