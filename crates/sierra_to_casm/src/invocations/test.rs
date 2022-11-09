use std::collections::HashMap;
use std::vec;

use casm::ap_change::ApChange;
use casm::operand::DerefOrImmediate;
use casm::{casm, deref};
use indoc::indoc;
use itertools::zip_eq;
use pretty_assertions::assert_eq;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use sierra::program::{Statement, StatementIdx};
use sierra::program_registry::ProgramRegistry;
use sierra::ProgramParser;
use sierra_gas::gas_info::GasInfo;
use utils::extract_matches;

use super::{CompiledInvocation, ProgramInfo};
use crate::compiler::check_basic_structure;
use crate::environment::gas_wallet::GasWallet;
use crate::environment::Environment;
use crate::invocations::{compile_invocation, BranchChanges};
use crate::metadata::Metadata;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::type_sizes::get_type_size_map;

fn as_ref_expr(cell_expr: CellExpression) -> ReferenceExpression {
    ReferenceExpression::from_cell(cell_expr)
}

// Compiles the last libfunc invocation in sierra_code.
fn compile_libfunc(sierra_code: &str, refs: Vec<ReferenceExpression>) -> CompiledInvocation {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    let registry =
        ProgramRegistry::<CoreType, CoreLibFunc>::new(&program).expect("Failed to build registery");
    let type_sizes =
        get_type_size_map(&program, &registry).expect("Failed to build type information");
    let invocation = extract_matches!(
        program.statements.last().unwrap(),
        Statement::Invocation,
        "Could not find invocation."
    );

    let libfunc = registry.get_libfunc(&invocation.libfunc_id).expect("Failed to get lib func");
    let statement_idx = StatementIdx(program.statements.len() - 1);
    check_basic_structure(statement_idx, invocation, libfunc)
        .expect("basic structure check failed.");

    let program_info = ProgramInfo {
        metadata: &Metadata {
            function_ap_change: HashMap::new(),
            gas_info: GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() },
        },
        type_sizes: &type_sizes,
    };

    let args: Vec<ReferenceValue> = zip_eq(refs.into_iter(), libfunc.param_signatures())
        .map(|(expression, param)| ReferenceValue { expression, ty: param.ty.clone() })
        .collect();

    let environment = Environment::new(GasWallet::Disabled);
    compile_invocation(program_info, invocation, libfunc, statement_idx, &args, environment)
        .expect("Failed to compile invocation.")
}

#[test]
fn test_felt_add() {
    let refs: Vec<ReferenceExpression> = [deref!([fp + 5]), deref!([ap + 5])]
        .map(|x| as_ref_expr(CellExpression::Deref(x)))
        .into_iter()
        .collect();
    let sierra_code = indoc! {"
        type felt = felt;

        libfunc felt_add = felt_add;

        felt_add([1], [2]) -> ([1]);"
    };
    let compiled_invocation = compile_libfunc(sierra_code, refs);

    let expected_ref = as_ref_expr(CellExpression::BinOp(BinOpExpression {
        op: FeltOperator::Add,
        a: deref!([fp + 5]),
        b: DerefOrImmediate::Deref(deref!([ap + 5])),
    }));
    let felt_ty = ConcreteTypeId::from_string("felt");
    let expected = CompiledInvocation {
        instructions: vec![],
        relocations: vec![],
        results: vec![BranchChanges {
            refs: vec![ReferenceValue { expression: expected_ref, ty: felt_ty }],
            ap_change: ApChange::Known(0),
            gas_change: 0,
        }],
        environment: Environment::new(GasWallet::Disabled),
    };
    assert_eq!(compiled_invocation, expected);
}

#[test]
fn test_store_temp() {
    let refs = vec![as_ref_expr(CellExpression::BinOp(BinOpExpression {
        op: FeltOperator::Add,
        a: deref!([fp + 5]),
        b: DerefOrImmediate::Deref(deref!([ap + 5])),
    }))];
    let sierra_code = indoc! {"
        type felt = felt;

        libfunc store_temp_felt = store_temp<felt>;

        store_temp_felt([1]) -> ([1]);"
    };
    let compiled_invocation = compile_libfunc(sierra_code, refs);

    let expected_ref = as_ref_expr(CellExpression::Deref(deref!([ap + -1])));
    let felt_ty = ConcreteTypeId::from_string("felt");
    let expected = CompiledInvocation {
        instructions: casm! {[ap + 0] = [fp + 5] + [ap + 5], ap++;}.instructions,
        relocations: vec![],
        results: vec![BranchChanges {
            refs: vec![ReferenceValue { expression: expected_ref, ty: felt_ty }],
            ap_change: ApChange::Known(1),
            gas_change: -1,
        }],
        environment: Environment::new(GasWallet::Disabled),
    };
    assert_eq!(compiled_invocation, expected);
}
