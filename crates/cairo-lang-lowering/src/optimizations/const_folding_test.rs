use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::corelib::{
    CorelibSemantic, core_array_felt252_ty, core_box_ty, get_usize_ty, option_none_variant,
    option_some_variant,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::{ConstValue, ConstValueId};
use cairo_lang_semantic::test_utils::{setup_test_function, setup_test_function_ex};
use cairo_lang_semantic::{GenericArgumentId, MatchArmSelector, TypeLongId};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use num_bigint::BigInt;
use salsa::Database;

use super::const_folding;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::{ConcreteFunctionWithBodyId, SemanticFunctionIdEx};
use crate::objects::blocks::BlocksBuilder;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_utils::LoweringDatabaseForTesting;
use crate::{
    Block, BlockEnd, Lowered, LoweringStage, MatchArm, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementConst, StatementDesnap, StatementSnapshot, StatementUnbox, VarUsage,
    Variable, VariableArena,
};

cairo_lang_test_utils::test_file_test!(
    const_folding,
    "src/optimizations/test_data",
    {
        const_folding: "const_folding",
    },
    test_match_optimizer
);

fn test_match_optimizer(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let mut before = db
        .lowered_body(function_id, LoweringStage::PreOptimizations)
        .unwrap_or_else(|_| {
            let semantic_diags = db
                .module_semantic_diagnostics(test_function.module_id)
                .unwrap_or_default()
                .format(db);
            let lowering_diags = db
                .module_lowering_diagnostics(test_function.module_id)
                .unwrap_or_default()
                .format(db);

            panic!(
                "Failed to get lowered body for function {function_id:?}.\nSemantic diagnostics: \
                 {semantic_diags}\nLowering diagnostics: {lowering_diags}",
            )
        })
        .clone();
    OptimizationPhase::ApplyInlining { enable_const_folding: false }
        .apply(db, function_id, &mut before)
        .unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::CancelOps.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let mut after = before.clone();
    OptimizationPhase::ConstFolding.apply(db, function_id, &mut after).unwrap();

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "before".into(),
            format!("{:?}", before.debug(&LoweredFormatter::new(db, &before.variables))),
        ),
        (
            "after".into(),
            format!("{:?}", after.debug(&LoweredFormatter::new(db, &after.variables))),
        ),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}

fn alloc_var<'db>(
    db: &'db dyn Database,
    variables: &mut VariableArena<'db>,
    ty: cairo_lang_semantic::TypeId<'db>,
    location: crate::ids::LocationId<'db>,
) -> crate::VariableId {
    variables.alloc(Variable::with_default_context(db, ty, location))
}

fn int_const<'db>(
    db: &'db dyn Database,
    ty: cairo_lang_semantic::TypeId<'db>,
    value: i64,
) -> ConstValueId<'db> {
    ConstValueId::from_int(db, ty, &BigInt::from(value))
}

fn assert_int_const<'db>(db: &'db dyn Database, stmt: &Statement<'db>, expected: i64) {
    let Statement::Const(StatementConst { value, boxed: false, .. }) = stmt else {
        panic!("Expected const statement, got {stmt:?}");
    };
    let ConstValue::Int(actual, _) = value.long(db) else {
        panic!("Expected integer const, got {:?}", value.long(db));
    };
    assert_eq!(actual, &BigInt::from(expected));
}

#[test]
fn const_folding_propagates_snapshot_pop_front_success_outputs() {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, _) =
        setup_test_function_ex(db, "fn foo() -> usize { 0 }", "foo", "", None, None).split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let template = db.lowered_body(function_id, LoweringStage::PreOptimizations).unwrap().clone();
    let location = template.signature.location;

    let felt_ty = db.core_info().felt252;
    let usize_ty = get_usize_ty(db);
    let array_ty = core_array_felt252_ty(db);
    let snapshot_array_ty = TypeLongId::Snapshot(array_ty).intern(db);
    let snapshot_felt_ty = TypeLongId::Snapshot(felt_ty).intern(db);
    let box_snapshot_felt_ty = core_box_ty(db, snapshot_felt_ty);

    let array_module = ModuleHelper::core(db).submodule("array");
    let generic_args = vec![GenericArgumentId::Type(felt_ty)];
    let array_new = array_module.function_id("array_new", generic_args.clone()).lowered(db);
    let array_append = array_module.function_id("array_append", generic_args.clone()).lowered(db);
    let array_snapshot_pop_front =
        array_module.function_id("array_snapshot_pop_front", generic_args.clone()).lowered(db);
    let array_len = array_module.function_id("array_len", generic_args).lowered(db);

    let mut variables = VariableArena::default();
    let arr0 = alloc_var(db, &mut variables, array_ty, location);
    let one = alloc_var(db, &mut variables, felt_ty, location);
    let arr1 = alloc_var(db, &mut variables, array_ty, location);
    let arr2 = alloc_var(db, &mut variables, array_ty, location);
    let snapshot = alloc_var(db, &mut variables, snapshot_array_ty, location);
    let success_arr = alloc_var(db, &mut variables, snapshot_array_ty, location);
    let success_box = alloc_var(db, &mut variables, box_snapshot_felt_ty, location);
    let failure_arr = alloc_var(db, &mut variables, snapshot_array_ty, location);
    let len_out = alloc_var(db, &mut variables, usize_ty, location);
    let failure_out = alloc_var(db, &mut variables, usize_ty, location);

    let root = Block {
        statements: vec![
            Statement::Call(StatementCall {
                function: array_new,
                inputs: vec![],
                with_coupon: false,
                outputs: vec![arr0],
                location,
                is_specialization_base_call: false,
            }),
            Statement::Const(StatementConst::new_flat(int_const(db, felt_ty, 1), one)),
            Statement::Call(StatementCall {
                function: array_append,
                inputs: vec![
                    VarUsage { var_id: arr0, location },
                    VarUsage { var_id: one, location },
                ],
                with_coupon: false,
                outputs: vec![arr1],
                location,
                is_specialization_base_call: false,
            }),
            Statement::Snapshot(StatementSnapshot::new(
                VarUsage { var_id: arr1, location },
                arr2,
                snapshot,
            )),
        ],
        end: BlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: array_snapshot_pop_front,
                inputs: vec![VarUsage { var_id: snapshot, location }],
                arms: vec![
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_some_variant(
                            db,
                            box_snapshot_felt_ty,
                        )),
                        block_id: crate::BlockId(1),
                        var_ids: vec![success_arr, success_box],
                    },
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_none_variant(
                            db,
                            box_snapshot_felt_ty,
                        )),
                        block_id: crate::BlockId(2),
                        var_ids: vec![failure_arr],
                    },
                ],
                location,
            }),
        },
    };
    let success = Block {
        statements: vec![Statement::Call(StatementCall {
            function: array_len,
            inputs: vec![VarUsage { var_id: success_arr, location }],
            with_coupon: false,
            outputs: vec![len_out],
            location,
            is_specialization_base_call: false,
        })],
        end: BlockEnd::Return(vec![VarUsage { var_id: len_out, location }], location),
    };
    let failure = Block {
        statements: vec![Statement::Const(StatementConst::new_flat(
            int_const(db, usize_ty, 99),
            failure_out,
        ))],
        end: BlockEnd::Return(vec![VarUsage { var_id: failure_out, location }], location),
    };

    let mut lowered = Lowered {
        diagnostics: template.diagnostics.clone(),
        signature: template.signature.clone(),
        variables,
        blocks: BlocksBuilder(vec![root, success, failure]).build().unwrap(),
        parameters: vec![],
    };

    const_folding(db, function_id, &mut lowered);

    assert_int_const(db, &lowered.blocks[crate::BlockId(1)].statements[0], 0);
}

#[test]
fn const_folding_propagates_snapshot_pop_back_success_outputs() {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, _) =
        setup_test_function_ex(db, "fn foo() -> felt252 { 0 }", "foo", "", None, None).split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let template = db.lowered_body(function_id, LoweringStage::PreOptimizations).unwrap().clone();
    let location = template.signature.location;

    let felt_ty = db.core_info().felt252;
    let array_ty = core_array_felt252_ty(db);
    let snapshot_array_ty = TypeLongId::Snapshot(array_ty).intern(db);
    let snapshot_felt_ty = TypeLongId::Snapshot(felt_ty).intern(db);
    let box_snapshot_felt_ty = core_box_ty(db, snapshot_felt_ty);

    let core = ModuleHelper::core(db);
    let array_module = core.submodule("array");
    let generic_args = vec![GenericArgumentId::Type(felt_ty)];
    let array_new = array_module.function_id("array_new", generic_args.clone()).lowered(db);
    let array_append = array_module.function_id("array_append", generic_args.clone()).lowered(db);
    let array_snapshot_pop_back =
        array_module.function_id("array_snapshot_pop_back", generic_args).lowered(db);
    let felt_add = core.function_id("felt252_add", vec![]).lowered(db);

    let mut variables = VariableArena::default();
    let arr0 = alloc_var(db, &mut variables, array_ty, location);
    let one = alloc_var(db, &mut variables, felt_ty, location);
    let arr1 = alloc_var(db, &mut variables, array_ty, location);
    let two = alloc_var(db, &mut variables, felt_ty, location);
    let arr2 = alloc_var(db, &mut variables, array_ty, location);
    let arr3 = alloc_var(db, &mut variables, array_ty, location);
    let snapshot = alloc_var(db, &mut variables, snapshot_array_ty, location);
    let success_arr = alloc_var(db, &mut variables, snapshot_array_ty, location);
    let success_box = alloc_var(db, &mut variables, box_snapshot_felt_ty, location);
    let failure_arr = alloc_var(db, &mut variables, snapshot_array_ty, location);
    let snapshot_value = alloc_var(db, &mut variables, snapshot_felt_ty, location);
    let value = alloc_var(db, &mut variables, felt_ty, location);
    let addend = alloc_var(db, &mut variables, felt_ty, location);
    let sum = alloc_var(db, &mut variables, felt_ty, location);
    let failure_out = alloc_var(db, &mut variables, felt_ty, location);

    let root = Block {
        statements: vec![
            Statement::Call(StatementCall {
                function: array_new,
                inputs: vec![],
                with_coupon: false,
                outputs: vec![arr0],
                location,
                is_specialization_base_call: false,
            }),
            Statement::Const(StatementConst::new_flat(int_const(db, felt_ty, 1), one)),
            Statement::Call(StatementCall {
                function: array_append,
                inputs: vec![
                    VarUsage { var_id: arr0, location },
                    VarUsage { var_id: one, location },
                ],
                with_coupon: false,
                outputs: vec![arr1],
                location,
                is_specialization_base_call: false,
            }),
            Statement::Const(StatementConst::new_flat(int_const(db, felt_ty, 2), two)),
            Statement::Call(StatementCall {
                function: array_append,
                inputs: vec![
                    VarUsage { var_id: arr1, location },
                    VarUsage { var_id: two, location },
                ],
                with_coupon: false,
                outputs: vec![arr2],
                location,
                is_specialization_base_call: false,
            }),
            Statement::Snapshot(StatementSnapshot::new(
                VarUsage { var_id: arr2, location },
                arr3,
                snapshot,
            )),
        ],
        end: BlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: array_snapshot_pop_back,
                inputs: vec![VarUsage { var_id: snapshot, location }],
                arms: vec![
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_some_variant(
                            db,
                            box_snapshot_felt_ty,
                        )),
                        block_id: crate::BlockId(1),
                        var_ids: vec![success_arr, success_box],
                    },
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_none_variant(
                            db,
                            box_snapshot_felt_ty,
                        )),
                        block_id: crate::BlockId(2),
                        var_ids: vec![failure_arr],
                    },
                ],
                location,
            }),
        },
    };
    let success = Block {
        statements: vec![
            Statement::Unbox(StatementUnbox {
                input: VarUsage { var_id: success_box, location },
                output: snapshot_value,
            }),
            Statement::Desnap(StatementDesnap {
                input: VarUsage { var_id: snapshot_value, location },
                output: value,
            }),
            Statement::Const(StatementConst::new_flat(int_const(db, felt_ty, 1), addend)),
            Statement::Call(StatementCall {
                function: felt_add,
                inputs: vec![
                    VarUsage { var_id: value, location },
                    VarUsage { var_id: addend, location },
                ],
                with_coupon: false,
                outputs: vec![sum],
                location,
                is_specialization_base_call: false,
            }),
        ],
        end: BlockEnd::Return(vec![VarUsage { var_id: sum, location }], location),
    };
    let failure = Block {
        statements: vec![Statement::Const(StatementConst::new_flat(
            int_const(db, felt_ty, 0),
            failure_out,
        ))],
        end: BlockEnd::Return(vec![VarUsage { var_id: failure_out, location }], location),
    };

    let mut lowered = Lowered {
        diagnostics: template.diagnostics.clone(),
        signature: template.signature.clone(),
        variables,
        blocks: BlocksBuilder(vec![root, success, failure]).build().unwrap(),
        parameters: vec![],
    };

    const_folding(db, function_id, &mut lowered);

    assert_int_const(db, lowered.blocks[crate::BlockId(1)].statements.last().unwrap(), 3);
}
