use std::collections::HashMap;

use test_case::test_case;

use super::mem_cell::MemCell;
use super::LibFuncSimulationError::{
    self, FunctionSimulationError, MemoryLayoutMismatch, WrongNumberOfArgs,
};
use super::{core, SimulationError};
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{CoreLibFunc, GenericLibFunc};
use crate::program::{Function, GenericArg, StatementIdx};

fn type_arg(name: &str) -> GenericArg {
    GenericArg::Type(name.into())
}

fn value_arg(v: i64) -> GenericArg {
    GenericArg::Value(v)
}

fn user_func_arg(name: &str) -> GenericArg {
    GenericArg::UserFunc(name.into())
}

/// Expects to find a libfunc and simulate it.
fn simulate(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<Vec<MemCell>>,
) -> Result<(Vec<Vec<MemCell>>, usize), LibFuncSimulationError> {
    let mock_func_entry = |id: &str| {
        (
            id.into(),
            Function { id: id.into(), entry: StatementIdx(0), ret_types: vec![], params: vec![] },
        )
    };
    core::simulate(
        &CoreLibFunc::by_id(&id.into())
            .unwrap()
            .specialize(
                SpecializationContext {
                    concrete_type_ids: &HashMap::from([
                        (("int".into(), &[][..]), "int".into()),
                        (("NonZero".into(), &[type_arg("int")][..]), "NonZeroInt".into()),
                        (("Deferred".into(), &[type_arg("int")][..]), "DeferredInt".into()),
                        (("GasBuiltin".into(), &[][..]), "GasBuiltin".into()),
                        (
                            ("Deferred".into(), &[type_arg("GasBuiltin")][..]),
                            "DeferredGasBuiltin".into(),
                        ),
                    ]),
                    functions: &HashMap::from([
                        mock_func_entry("drop_all_inputs"),
                        mock_func_entry("identity"),
                        mock_func_entry("unimplemented"),
                    ]),
                },
                &generic_args,
            )
            .unwrap(),
        inputs,
        |id, inputs| {
            if id == &"drop_all_inputs".into() {
                Ok(vec![])
            } else if id == &"identity".into() {
                Ok(inputs)
            } else {
                Err(FunctionSimulationError(
                    id.clone(),
                    Box::new(SimulationError::StatementOutOfBounds(StatementIdx(0))),
                ))
            }
        },
    )
}

/// Expects to find a libfunc, wrapping and unwrapping the MemCell types and vectors of the
/// inputs and outputs, assumming all of size 1.
#[test_case("get_gas", vec![value_arg(4)], vec![5] => Ok((vec![1], 0)); "get_gas<4>(5)")]
#[test_case("get_gas", vec![value_arg(4)], vec![2] => Ok((vec![2], 1)); "get_gas<4>(2)")]
#[test_case("int_jump_nz", vec![], vec![2] => Ok((vec![2], 0)); "int_jump_nz(2)")]
#[test_case("int_jump_nz", vec![], vec![0] => Ok((vec![], 1)); "int_jump_nz(0)")]
#[test_case("jump", vec![], vec![] => Ok((vec![], 0)); "jump()")]
fn simulate_invocation(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<i64>,
) -> Result<(Vec<i64>, usize), LibFuncSimulationError> {
    simulate(id, generic_args, inputs.into_iter().map(|value| vec![MemCell { value }]).collect())
        .map(|(outputs, chosen_branch)| {
            (
                outputs
                    .into_iter()
                    .map(|mut cells_vec| {
                        // Unwrapping vector and MemCell.
                        assert_eq!(cells_vec.len(), 1);
                        cells_vec.remove(0).value
                    })
                    .collect(),
                chosen_branch,
            )
        })
}

/// Tests for simulation of a non branch invocations.
#[test_case("refund_gas", vec![value_arg(5)], vec![2] => Ok(vec![7]); "refund_gas<5>(2)")]
#[test_case("int_add", vec![], vec![2, 3] => Ok(vec![5]); "int_add(2, 3)")]
#[test_case("int_sub", vec![], vec![5, 3] => Ok(vec![2]); "int_sub(5, 3)")]
#[test_case("int_mul", vec![], vec![5, 3] => Ok(vec![15]); "int_mul(5, 3)")]
#[test_case("int_div", vec![], vec![32, 5] => Ok(vec![6]); "int_div(32, 5)")]
#[test_case("int_mod", vec![], vec![32, 5] => Ok(vec![2]); "int_mod(32, 5)")]
#[test_case("int_add", vec![value_arg(3)], vec![2] => Ok(vec![5]); "int_add<3>(2)")]
#[test_case("int_sub", vec![value_arg(3)], vec![5] => Ok(vec![2]); "int_sub<3>(5)")]
#[test_case("int_mul", vec![value_arg(3)], vec![5] => Ok(vec![15]); "int_mul<3>(5)")]
#[test_case("int_div", vec![value_arg(5)], vec![32] => Ok(vec![6]); "int_div<5>(32)")]
#[test_case("int_mod", vec![value_arg(5)], vec![32] => Ok(vec![2]); "int_mod<5>(32)")]
#[test_case("int_const", vec![value_arg(3)], vec![] => Ok(vec![3]); "int_const<3>()")]
#[test_case("int_dup", vec![], vec![24] => Ok(vec![24, 24]); "int_dup(24)")]
#[test_case("int_ignore", vec![], vec![2] => Ok(vec![]); "int_ignore(2)")]
#[test_case("unwrap_nz", vec![type_arg("int")], vec![6] => Ok(vec![6]); "unwrap_nz<int>(6)")]
#[test_case("store_temp", vec![type_arg("int")], vec![6] => Ok(vec![6]); "store_temp<int>(6)")]
#[test_case("align_temps", vec![type_arg("int")], vec![] => Ok(vec![]); "align_temps<int>()")]
#[test_case("store_local", vec![type_arg("int")], vec![6] => Ok(vec![6]); "store_local<int>(6)")]
#[test_case("alloc_locals", vec![], vec![] => Ok(vec![]); "alloc_locals()")]
#[test_case("rename", vec![type_arg("int")], vec![6] => Ok(vec![6]); "rename<int>(6)")]
#[test_case("move", vec![type_arg("int")], vec![6] => Ok(vec![6]); "move<int>(6)")]
#[test_case("function_call", vec![user_func_arg("drop_all_inputs")], vec![3, 5] => Ok(vec![]);
            "function_call<drop_all_inputs>()")]
#[test_case("function_call", vec![user_func_arg("identity")], vec![3, 5] => Ok(vec![3, 5]);
            "function_call<identity>()")]
fn simulate_none_branch(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<i64>,
) -> Result<Vec<i64>, LibFuncSimulationError> {
    simulate_invocation(id, generic_args, inputs).map(|(outputs, chosen_branch)| {
        assert_eq!(chosen_branch, 0);
        outputs
    })
}

#[test_case("get_gas", vec![value_arg(4)], vec![vec![]] => MemoryLayoutMismatch;
            "get_gas<4>(empty)")]
#[test_case("get_gas", vec![value_arg(4)], vec![] => WrongNumberOfArgs; "get_gas<4>()")]
#[test_case("refund_gas", vec![value_arg(5)], vec![vec![]] => MemoryLayoutMismatch;
            "refund_gas<5>(empty)")]
#[test_case("refund_gas", vec![value_arg(5)], vec![] => WrongNumberOfArgs; "refund_gas<5>()")]
#[test_case("int_add", vec![], vec![vec![1]] => WrongNumberOfArgs; "int_add(1)")]
#[test_case("int_sub", vec![], vec![vec![1]] => WrongNumberOfArgs; "int_sub(1)")]
#[test_case("int_mul", vec![], vec![vec![1]] => WrongNumberOfArgs; "int_mul(1)")]
#[test_case("int_div", vec![], vec![vec![1]] => WrongNumberOfArgs; "int_div(1)")]
#[test_case("int_mod", vec![], vec![vec![1]] => WrongNumberOfArgs; "int_mod(1)")]
#[test_case("int_add", vec![value_arg(3)], vec![] => WrongNumberOfArgs; "int_add<3>()")]
#[test_case("int_sub", vec![value_arg(3)], vec![] => WrongNumberOfArgs; "int_sub<3>()")]
#[test_case("int_mul", vec![value_arg(3)], vec![] => WrongNumberOfArgs; "int_mul<3>()")]
#[test_case("int_div", vec![value_arg(5)], vec![] => WrongNumberOfArgs; "int_div<5>()")]
#[test_case("int_mod", vec![value_arg(5)], vec![] => WrongNumberOfArgs; "int_mod<5>()")]
#[test_case("int_const", vec![value_arg(3)], vec![vec![1]] => WrongNumberOfArgs;
            "int_const<3>(1)")]
#[test_case("int_dup", vec![], vec![] => WrongNumberOfArgs; "int_dup()")]
#[test_case("int_ignore", vec![], vec![] => WrongNumberOfArgs; "int_ignore()")]
#[test_case("int_jump_nz", vec![], vec![] => WrongNumberOfArgs; "int_jump_nz()")]
#[test_case("unwrap_nz", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "unwrap_nz<int>()")]
#[test_case("store_temp", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "store_temp<int>()")]
#[test_case("align_temps", vec![type_arg("int")], vec![vec![4]] => WrongNumberOfArgs;
            "align_temps<int>(4)")]
#[test_case("store_local", vec![type_arg("int")], vec![] => WrongNumberOfArgs;
            "store_local<int>()")]
#[test_case("alloc_locals", vec![], vec![vec![4]] => WrongNumberOfArgs; "alloc_locals(4)")]
#[test_case("rename", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "rename<int>()")]
#[test_case("move", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "move<int>()")]
#[test_case("jump", vec![], vec![vec![4]] => WrongNumberOfArgs; "jump(4)")]
#[test_case("function_call", vec![user_func_arg("unimplemented")], vec![] =>
            FunctionSimulationError(
                "unimplemented".into(),
                Box::new(SimulationError::StatementOutOfBounds(StatementIdx(0))));
            "function_call<unimplemented>()")]
fn simulate_error(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<Vec<i64>>,
) -> LibFuncSimulationError {
    simulate(
        id,
        generic_args,
        inputs
            .into_iter()
            .map(|input| input.into_iter().map(|value| MemCell { value }).collect())
            .collect(),
    )
    .err()
    .unwrap()
}
