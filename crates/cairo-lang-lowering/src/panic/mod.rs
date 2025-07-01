use std::collections::VecDeque;

use assert_matches::assert_matches;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::{Flag, flag_unsafe_panic};
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::corelib::{
    core_submodule, get_core_enum_concrete_variant, get_function_id, get_panic_ty, never_ty,
};
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{self as semantic, GenericArgumentId};
use cairo_lang_utils::{Intern, Upcast};
use itertools::{Itertools, chain, zip_eq};
use semantic::{ConcreteVariant, MatchArmSelector, TypeId};

use crate::blocks::BlocksBuilder;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::{
    ConcreteFunctionWithBodyId, FunctionId, FunctionLongId, SemanticFunctionIdEx, Signature,
};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    Block, BlockEnd, BlockId, DependencyType, Lowered, LoweringStage, MatchArm, MatchEnumInfo,
    MatchExternInfo, MatchInfo, Statement, StatementCall, StatementEnumConstruct,
    StatementStructConstruct, StatementStructDestructure, VarRemapping, VarUsage, VariableId,
};

// TODO(spapini): Remove tuple in the Ok() variant of the panic, by supporting multiple values in
// the Sierra type.

/// Lowering phase that converts `BlockEnd::Panic` into `BlockEnd::Return`, and wraps necessary
/// types with `PanicResult<>`.
pub fn lower_panics(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut Lowered,
) -> Maybe<()> {
    // Skip this phase for non panicable functions.
    if !db.function_with_body_may_panic(function_id)? {
        return Ok(());
    }

    let opt_trace_fn = if matches!(
        db.get_flag(FlagId::new(db, "panic_backtrace")),
        Some(flag) if matches!(*flag, Flag::PanicBacktrace(true)),
    ) {
        Some(
            ModuleHelper::core(db)
                .submodule("internal")
                .function_id(
                    "trace",
                    vec![GenericArgumentId::Constant(
                        ConstValue::Int(
                            0x70616e6963u64.into(), // 'panic' as numeric.
                            db.core_info().felt252,
                        )
                        .intern(db),
                    )],
                )
                .lowered(db),
        )
    } else {
        None
    };

    if flag_unsafe_panic(db) {
        lower_unsafe_panic(db, lowered, opt_trace_fn);
        return Ok(());
    }

    let signature = function_id.signature(db)?;
    // All types should be fully concrete at this point.
    assert!(signature.is_fully_concrete(db));
    let panic_info = PanicSignatureInfo::new(db, &signature);
    let variables = VariableAllocator::new(
        db,
        function_id.base_semantic_function(db).function_with_body_id(db),
        std::mem::take(&mut lowered.variables),
    )
    .unwrap();
    let mut ctx = PanicLoweringContext {
        variables,
        block_queue: VecDeque::from(lowered.blocks.get().clone()),
        flat_blocks: BlocksBuilder::new(),
        panic_info,
    };

    if let Some(trace_fn) = opt_trace_fn {
        for block in ctx.block_queue.iter_mut() {
            if let BlockEnd::Panic(end) = &block.end {
                block.statements.push(Statement::Call(StatementCall {
                    function: trace_fn,
                    inputs: vec![],
                    with_coupon: false,
                    outputs: vec![],
                    location: end.location,
                }));
            }
        }
    }

    // Iterate block queue (old and new blocks).
    while let Some(block) = ctx.block_queue.pop_front() {
        ctx = handle_block(ctx, block)?;
    }

    lowered.variables = ctx.variables.variables;
    lowered.blocks = ctx.flat_blocks.build().unwrap();

    Ok(())
}

/// Lowering phase that converts BlockEnd::Panic into BlockEnd::Match { function: unsafe_panic }.
/// 'opt_trace_fn' is an optional function to call before the panic.
fn lower_unsafe_panic(
    db: &dyn LoweringGroup,
    lowered: &mut Lowered,
    opt_trace_fn: Option<FunctionId>,
) {
    let panics = core_submodule(db, "panics");
    let panic_func_id =
        FunctionLongId::Semantic(get_function_id(db, panics, "unsafe_panic".into(), vec![]))
            .intern(db);

    for block in lowered.blocks.iter_mut() {
        let BlockEnd::Panic(err_data) = &mut block.end else {
            continue;
        };

        // Clean up undroppable panic related variables to pass add_destructs.
        let Some(Statement::StructConstruct(tuple_construct)) = block.statements.pop() else {
            panic!("Expected a tuple construct before the panic.");
        };
        // Assert that `err_data` bye the statement that was removed above.
        assert_eq!(tuple_construct.output, err_data.var_id);

        let panic_construct_statement = block.statements.pop();
        // Assert that the output of `panic_construct_statement` is the first input of
        // 'tuple_construct'.
        assert_matches!(panic_construct_statement, Some(Statement::StructConstruct(panic_construct)) if panic_construct.output == tuple_construct.inputs[0].var_id);

        if let Some(trace_fn) = opt_trace_fn {
            block.statements.push(Statement::Call(StatementCall {
                function: trace_fn,
                inputs: vec![],
                with_coupon: false,
                outputs: vec![],
                location: err_data.location,
            }));
        }

        block.end = BlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                arms: vec![],
                location: err_data.location,
                function: panic_func_id,
                inputs: vec![],
            }),
        }
    }
}

/// Handles the lowering of panics in a single block.
fn handle_block(
    mut ctx: PanicLoweringContext<'_>,
    mut block: Block,
) -> Maybe<PanicLoweringContext<'_>> {
    let mut block_ctx = PanicBlockLoweringContext { ctx, statements: Vec::new() };
    for (i, stmt) in block.statements.iter().cloned().enumerate() {
        if let Some((cur_block_end, continuation_block)) = block_ctx.handle_statement(&stmt)? {
            // This case means that the lowering should split the block here.

            // Block ended with a match.
            ctx = block_ctx.handle_end(cur_block_end);
            if let Some(continuation_block) = continuation_block {
                // The rest of the statements in this block have not been handled yet, and should be
                // handled as a part of the continuation block - the second block in the "split".
                let block_to_edit =
                    &mut ctx.block_queue[continuation_block.0 - ctx.flat_blocks.len()];
                block_to_edit.statements.extend(block.statements.drain(i + 1..));
                block_to_edit.end = block.end;
            }
            return Ok(ctx);
        }
    }
    ctx = block_ctx.handle_end(block.end);
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
    pub err_variant: ConcreteVariant,
    /// The PanicResult concrete type - the new return type of the function.
    pub actual_return_ty: TypeId,
    /// Does the function always panic.
    /// Note that if it does - the function returned type is always `(Panic, Array<felt252>)`.
    pub always_panic: bool,
}
impl PanicSignatureInfo {
    pub fn new(db: &dyn LoweringGroup, signature: &Signature) -> Self {
        let extra_rets = signature.extra_rets.iter().map(|param| param.ty());
        let original_return_ty = signature.return_type;

        let ok_ret_tys = chain!(extra_rets, [original_return_ty]).collect_vec();
        let ok_ty = semantic::TypeLongId::Tuple(ok_ret_tys.clone()).intern(db);
        let ok_variant = get_core_enum_concrete_variant(
            db,
            "PanicResult",
            vec![GenericArgumentId::Type(ok_ty)],
            "Ok",
        );
        let err_variant = get_core_enum_concrete_variant(
            db,
            "PanicResult",
            vec![GenericArgumentId::Type(ok_ty)],
            "Err",
        );
        let always_panic = original_return_ty == never_ty(db);
        let panic_ty = if always_panic { err_variant.ty } else { get_panic_ty(db, ok_ty) };

        Self {
            ok_ret_tys,
            ok_ty,
            ok_variant,
            err_variant,
            actual_return_ty: panic_ty,
            always_panic,
        }
    }
}

struct PanicLoweringContext<'a> {
    variables: VariableAllocator<'a>,
    block_queue: VecDeque<Block>,
    flat_blocks: BlocksBuilder,
    panic_info: PanicSignatureInfo,
}
impl PanicLoweringContext<'_> {
    pub fn db(&self) -> &dyn LoweringGroup {
        self.variables.db
    }

    fn enqueue_block(&mut self, block: Block) -> BlockId {
        self.block_queue.push_back(block);
        BlockId(self.flat_blocks.len() + self.block_queue.len())
    }
}

struct PanicBlockLoweringContext<'a> {
    ctx: PanicLoweringContext<'a>,
    statements: Vec<Statement>,
}
impl<'a> PanicBlockLoweringContext<'a> {
    pub fn db(&self) -> &dyn LoweringGroup {
        self.ctx.db()
    }

    fn new_var(&mut self, req: VarRequest) -> VariableId {
        self.ctx.variables.new_var(req)
    }

    /// Handles a statement. If needed, returns the continuation block and the block end for this
    /// block.
    /// The continuation block happens when a panic match is added, and the block needs to be split.
    /// The continuation block is the second block in the "split". This function already partially
    /// creates this second block, and returns it.
    /// In case there is no panic match - but just a panic, there is no continuation block.
    fn handle_statement(&mut self, stmt: &Statement) -> Maybe<Option<(BlockEnd, Option<BlockId>)>> {
        if let Statement::Call(call) = &stmt {
            if let Some(with_body) = call.function.body(self.db())? {
                if self.db().function_with_body_may_panic(with_body)? {
                    return Ok(Some(self.handle_call_panic(call)?));
                }
            }
        }
        self.statements.push(stmt.clone());
        Ok(None)
    }

    /// Handles a call statement to a panicking function.
    /// Returns the continuation block ID for the caller to complete it, and the block end to set
    /// for the current block.
    fn handle_call_panic(&mut self, call: &StatementCall) -> Maybe<(BlockEnd, Option<BlockId>)> {
        // Extract return variable.
        let mut original_outputs = call.outputs.clone();
        let location = call.location.with_auto_generation_note(self.db(), "Panic handling");

        // Get callee info.
        let callee_signature = call.function.signature(self.ctx.variables.db)?;
        let callee_info = PanicSignatureInfo::new(self.ctx.variables.db, &callee_signature);
        if callee_info.always_panic {
            // The panic value, which is actually of type (Panics, Array<felt252>).
            let panic_result_var =
                self.new_var(VarRequest { ty: callee_info.actual_return_ty, location });
            // Emit the new statement.
            self.statements.push(Statement::Call(StatementCall {
                function: call.function,
                inputs: call.inputs.clone(),
                with_coupon: call.with_coupon,
                outputs: vec![panic_result_var],
                location,
            }));
            return Ok((BlockEnd::Panic(VarUsage { var_id: panic_result_var, location }), None));
        }

        // Allocate 2 new variables.
        // panic_result_var - for the new return variable, with is actually of type PanicResult<ty>.
        let panic_result_var =
            self.new_var(VarRequest { ty: callee_info.actual_return_ty, location });
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
            with_coupon: call.with_coupon,
            outputs: call_outputs,
            location,
        }));

        // Start constructing a match on the result.
        let block_continuation =
            self.ctx.enqueue_block(Block { statements: vec![], end: BlockEnd::NotSet });

        // Prepare Ok() match arm block. This block will be the continuation block.
        // This block is only partially created. It is returned at this function to let the caller
        // complete it.
        let block_ok = self.ctx.enqueue_block(Block {
            statements: vec![Statement::StructDestructure(StatementStructDestructure {
                input: VarUsage { var_id: inner_ok_value, location },
                outputs: inner_ok_values.clone(),
            })],
            end: BlockEnd::Goto(
                block_continuation,
                VarRemapping {
                    remapping: zip_eq(
                        original_outputs,
                        inner_ok_values.into_iter().map(|var_id| VarUsage { var_id, location }),
                    )
                    .collect(),
                },
            ),
        });

        // Prepare Err() match arm block.
        let err_var = self.new_var(VarRequest { ty: self.ctx.panic_info.err_variant.ty, location });
        let block_err = self.ctx.enqueue_block(Block {
            statements: vec![],
            end: BlockEnd::Panic(VarUsage { var_id: err_var, location }),
        });

        let cur_block_end = BlockEnd::Match {
            info: MatchInfo::Enum(MatchEnumInfo {
                concrete_enum_id: callee_info.ok_variant.concrete_enum_id,
                input: VarUsage { var_id: panic_result_var, location },
                arms: vec![
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(callee_info.ok_variant),
                        block_id: block_ok,
                        var_ids: vec![inner_ok_value],
                    },
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(callee_info.err_variant),
                        block_id: block_err,
                        var_ids: vec![err_var],
                    },
                ],
                location,
            }),
        };

        Ok((cur_block_end, Some(block_continuation)))
    }

    fn handle_end(mut self, end: BlockEnd) -> PanicLoweringContext<'a> {
        let end = match end {
            BlockEnd::Goto(target, remapping) => BlockEnd::Goto(target, remapping),
            BlockEnd::Panic(err_data) => {
                // Wrap with PanicResult::Err.
                let ty = self.ctx.panic_info.actual_return_ty;
                let location = err_data.location;
                let output = if self.ctx.panic_info.always_panic {
                    err_data.var_id
                } else {
                    let output = self.new_var(VarRequest { ty, location });
                    self.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                        variant: self.ctx.panic_info.err_variant,
                        input: err_data,
                        output,
                    }));
                    output
                };
                BlockEnd::Return(vec![VarUsage { var_id: output, location }], location)
            }
            BlockEnd::Return(returns, location) => {
                // Tuple construction.
                let tupled_res =
                    self.new_var(VarRequest { ty: self.ctx.panic_info.ok_ty, location });
                self.statements.push(Statement::StructConstruct(StatementStructConstruct {
                    inputs: returns,
                    output: tupled_res,
                }));

                // Wrap with PanicResult::Ok.
                let ty = self.ctx.panic_info.actual_return_ty;
                let output = self.new_var(VarRequest { ty, location });
                self.statements.push(Statement::EnumConstruct(StatementEnumConstruct {
                    variant: self.ctx.panic_info.ok_variant,
                    input: VarUsage { var_id: tupled_res, location },
                    output,
                }));
                BlockEnd::Return(vec![VarUsage { var_id: output, location }], location)
            }
            BlockEnd::NotSet => unreachable!(),
            BlockEnd::Match { info } => BlockEnd::Match { info },
        };
        self.ctx.flat_blocks.alloc(Block { statements: self.statements, end });
        self.ctx
    }
}

// ============= Query implementations =============

/// Query implementation of [crate::db::LoweringGroup::function_may_panic].
pub fn function_may_panic(db: &dyn LoweringGroup, function: FunctionId) -> Maybe<bool> {
    if let Some(body) = function.body(db)? {
        return db.function_with_body_may_panic(body);
    }
    Ok(function.signature(db)?.panicable)
}

/// A trait to add helper methods in [LoweringGroup].
pub trait MayPanicTrait<'a>: Upcast<dyn LoweringGroup + 'a> {
    /// Returns whether a [ConcreteFunctionWithBodyId] may panic.
    fn function_with_body_may_panic(&self, function: ConcreteFunctionWithBodyId) -> Maybe<bool> {
        let scc_representative = self.upcast().lowered_scc_representative(
            function,
            DependencyType::Call,
            LoweringStage::Monomorphized,
        );
        self.upcast().scc_may_panic(scc_representative)
    }
}
impl<'a, T: Upcast<dyn LoweringGroup + 'a> + ?Sized> MayPanicTrait<'a> for T {}

/// Query implementation of [crate::db::LoweringGroup::scc_may_panic].
pub fn scc_may_panic(db: &dyn LoweringGroup, scc: ConcreteSCCRepresentative) -> Maybe<bool> {
    // Find the SCC representative.
    let scc_functions = db.lowered_scc(scc.0, DependencyType::Call, LoweringStage::Monomorphized);
    for function in scc_functions {
        if db.needs_withdraw_gas(function)? {
            return Ok(true);
        }
        if db.has_direct_panic(function)? {
            return Ok(true);
        }
        // For each direct callee, find if it may panic.
        let direct_callees = db.lowered_direct_callees(
            function,
            DependencyType::Call,
            LoweringStage::Monomorphized,
        )?;
        for direct_callee in direct_callees {
            if let Some(callee_body) = direct_callee.body(db)? {
                let callee_scc = db.lowered_scc_representative(
                    callee_body,
                    DependencyType::Call,
                    LoweringStage::Monomorphized,
                );
                if callee_scc != scc && db.scc_may_panic(callee_scc)? {
                    return Ok(true);
                }
            } else if direct_callee.signature(db)?.panicable {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

/// Query implementation of [crate::db::LoweringGroup::has_direct_panic].
pub fn has_direct_panic(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    let lowered_function = db.lowered_body(function_id, LoweringStage::Monomorphized)?;
    Ok(itertools::any(&lowered_function.blocks, |(_, block)| {
        matches!(&block.end, BlockEnd::Panic(..))
    }))
}
