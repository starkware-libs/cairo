use std::collections::HashMap;

use casm::ap_change::ApChange;
use casm::operand::DerefOrImmediate;
use casm::{casm, deref};
use indoc::indoc;
use pretty_assertions::assert_eq;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::extensions::felt::FeltOperator;
use sierra::ids::ConcreteTypeId;
use sierra::program::{Statement, StatementIdx};
use sierra::program_registry::ProgramRegistry;
use sierra::ProgramParser;
use sierra_gas::gas_info::GasInfo;

use super::{CompiledInvocation, ProgramInfo};
use crate::compiler::check_basic_structure;
use crate::environment::gas_wallet::GasWallet;
use crate::environment::Environment;
use crate::invocations::compile_invocation;
use crate::metadata::Metadata;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::type_sizes::get_type_size_map;

fn as_ref_value(cell_expr: CellExpression, ty: &ConcreteTypeId) -> ReferenceValue {
    ReferenceValue { expression: ReferenceExpression::from_cell(cell_expr), ty: ty.clone() }
}

// Compiles the last libfunc invocation in sierra_code.
fn compile_libfunc(sierra_code: &str, refs: &[ReferenceValue]) -> CompiledInvocation {
    let program = ProgramParser::new().parse(sierra_code).unwrap();

    let registry =
        ProgramRegistry::<CoreType, CoreLibFunc>::new(&program).expect("Failed to build registery");

    let type_sizes =
        get_type_size_map(&program, &registry).expect("Failed to build type information");

    let Some(Statement::Invocation(invocation)) = program.statements.last() else {
        panic!("Could not find invocation.");
    };

    let statement_idx = StatementIdx(program.statements.len() - 1);

    let environment = Environment::new(GasWallet::Disabled);

    let libfunc = registry.get_libfunc(&invocation.libfunc_id).expect("Failed to get lib func");
    check_basic_structure(statement_idx, invocation, libfunc)
        .expect("basic structure check failed.");

    let program_info = ProgramInfo {
        metadata: &Metadata {
            function_ap_change: HashMap::new(),
            gas_info: GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() },
        },
        type_sizes: &type_sizes,
    };
    compile_invocation(program_info, invocation, libfunc, statement_idx, refs, environment)
        .expect("Failed to compile invocation.")
}

#[test]
fn test_felt_add() {
    let felt_ty = ConcreteTypeId::from_string("felt");
    let refs: Vec<ReferenceValue> = [(deref!([fp + 5]), &felt_ty), (deref!([ap + 5]), &felt_ty)]
        .map(|(x, ty)| as_ref_value(CellExpression::Deref(x), ty))
        .into_iter()
        .collect();

    let sierra_code = indoc! {"
        type felt = felt;
    
        libfunc felt_add = felt_add;

        felt_add([1], [2]) -> ([1]);"
    };

    let compiled_invocation = compile_libfunc(sierra_code, &refs);

    let expected_ref = as_ref_value(
        CellExpression::BinOp(BinOpExpression {
            op: FeltOperator::Add,
            a: deref!([fp + 5]),
            b: DerefOrImmediate::Deref(deref!([ap + 5])),
        }),
        &felt_ty,
    );

    assert_eq!(compiled_invocation.instructions, []);
    assert_eq!(compiled_invocation.results.len(), 1);
    assert_eq!(compiled_invocation.results[0].ap_change, ApChange::Known(0));
    assert_eq!(compiled_invocation.results[0].refs, [expected_ref]);
    assert_eq!(compiled_invocation.results[0].gas_change, 0);
}

#[test]
fn test_store_temp() {
    let felt_ty = ConcreteTypeId::from_string("felt");
    let refs = [as_ref_value(
        CellExpression::BinOp(BinOpExpression {
            op: FeltOperator::Add,
            a: deref!([fp + 5]),
            b: DerefOrImmediate::Deref(deref!([ap + 5])),
        }),
        &felt_ty,
    )];

    let sierra_code = indoc! {"
        type felt = felt;

        libfunc store_temp_felt = store_temp<felt>;

        store_temp_felt([1]) -> ([1]);"
    };

    let compiled_invocation = compile_libfunc(sierra_code, &refs);

    let expected_ref = as_ref_value(CellExpression::Deref(deref!([ap + -1])), &felt_ty);

    assert_eq!(
        compiled_invocation.instructions,
        casm! {[ap + 0] = [fp + 5] + [ap + 5], ap++;}.instructions
    );
    assert_eq!(compiled_invocation.results.len(), 1);
    assert_eq!(compiled_invocation.results[0].ap_change, ApChange::Known(1));
    assert_eq!(compiled_invocation.results[0].refs, [expected_ref]);
    assert_eq!(compiled_invocation.results[0].gas_change, -1);
}
