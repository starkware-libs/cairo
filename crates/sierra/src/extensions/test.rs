use test_case::test_case;

use super::{ExtensionError, Extensions, InputError, SpecializationError};
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

fn type_arg(name: &str) -> GenericArg {
    GenericArg::Type(name.into())
}

fn value_arg(v: i64) -> GenericArg {
    GenericArg::Value(v)
}

#[test_case("NoneExistent", vec![] => Err(SpecializationError::UnsupportedLibCallName);
            "NoneExistent")]
#[test_case("get_gas", vec![value_arg(2)] => Ok(()); "get_gas<2>")]
#[test_case("get_gas", vec![] => Err(SpecializationError::UnsupportedGenericArg); "get_gas")]
#[test_case("get_gas", vec![value_arg(-2)] => Err(SpecializationError::UnsupportedGenericArg);
            "get_gas<minus 2>")]
#[test_case("refund_gas", vec![value_arg(7)] => Ok(()); "refund_gas<7>")]
#[test_case("refund_gas", vec![] => Err(SpecializationError::UnsupportedGenericArg);
            "refund_gas")]
#[test_case("refund_gas", vec![value_arg(-7)] => Err(SpecializationError::UnsupportedGenericArg);
            "refund_gas<minus 7>")]
#[test_case("int_add", vec![] => Ok(()); "int_add")]
#[test_case("int_sub", vec![] => Ok(()); "int_sub")]
#[test_case("int_mul", vec![] => Ok(()); "int_mul")]
#[test_case("int_div", vec![] => Ok(()); "int_div")]
#[test_case("int_mod", vec![] => Ok(()); "int_mod")]
#[test_case("int_add", vec![value_arg(2)] => Ok(()); "int_add<2>")]
#[test_case("int_sub", vec![value_arg(5)] => Ok(()); "int_sub<5>")]
#[test_case("int_mul", vec![value_arg(7)] => Ok(()); "int_mul<7>")]
#[test_case("int_div", vec![value_arg(9)] => Ok(()); "int_div<9>")]
#[test_case("int_div", vec![value_arg(0)] => Err(SpecializationError::UnsupportedGenericArg);
            "int_div<0>")]
#[test_case("int_mod", vec![value_arg(1)] => Ok(()); "int_mod<1>")]
#[test_case("int_mod", vec![value_arg(0)] => Err(SpecializationError::UnsupportedGenericArg);
            "int_mod<0>")]
#[test_case("int_const", vec![value_arg(8)] => Ok(()); "int_const<8>")]
#[test_case("int_const", vec![] => Err(SpecializationError::UnsupportedGenericArg); "int_const")]
#[test_case("int_ignore", vec![] => Ok(()); "int_ignore")]
#[test_case("int_ignore", vec![type_arg("T")] =>
            Err(SpecializationError::WrongNumberOfGenericArgs);
            "int_ignore<T>")]
#[test_case("int_dup", vec![] => Ok(()); "int_dup")]
#[test_case("int_dup", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfGenericArgs);
            "int_dup<T>")]
#[test_case("int_jump_nz", vec![] => Ok(()); "int_jump_nz")]
#[test_case("int_jump_nz", vec![type_arg("T")] =>
            Err(SpecializationError::WrongNumberOfGenericArgs);
            "int_jump_nz<T>")]
#[test_case("int_unwrap_nz", vec![] => Ok(()); "int_unwrap_nz")]
#[test_case("int_unwrap_nz", vec![type_arg("T")] =>
            Err(SpecializationError::WrongNumberOfGenericArgs);
            "int_unwrap_nz<T>")]
#[test_case("store_temp", vec![type_arg("int")] => Ok(()); "store_temp<int>")]
#[test_case("store_temp", vec![] => Err(SpecializationError::UnsupportedGenericArg);
            "store_temp")]
#[test_case("align_temps", vec![type_arg("int")] => Ok(()); "align_temps<int>")]
#[test_case("align_temps", vec![] => Err(SpecializationError::UnsupportedGenericArg);
            "align_temps")]
#[test_case("store_local", vec![type_arg("int")] => Ok(()); "store_local<int>")]
#[test_case("store_local", vec![] => Err(SpecializationError::UnsupportedGenericArg);
            "store_local")]
#[test_case("alloc_locals", vec![] => Ok(()); "alloc_locals")]
#[test_case("alloc_locals", vec![type_arg("int")] =>
            Err(SpecializationError::WrongNumberOfGenericArgs);
            "alloc_locals<int>")]
#[test_case("rename", vec![type_arg("int")] => Ok(()); "rename<int>")]
#[test_case("rename", vec![] => Err(SpecializationError::UnsupportedGenericArg); "rename")]
#[test_case("move", vec![type_arg("int")] => Ok(()); "move<int>")]
#[test_case("move", vec![] => Err(SpecializationError::UnsupportedGenericArg); "move no args")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfGenericArgs);
            "jump<T>")]
fn find_specialization(id: &str, generic_args: Vec<GenericArg>) -> Result<(), SpecializationError> {
    Extensions::default().specialize(&id.into(), &generic_args).map(|_| ()).map_err(|error| {
        match error {
            ExtensionError::Specialization { extension_id: _, error } => error,
            other => panic!("unexpected extension error: {:?}", other),
        }
    })
}

/// Expects to find an extension and simulate it.
fn simulate(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<Vec<MemCell>>,
) -> Result<(Vec<Vec<MemCell>>, usize), InputError> {
    Extensions::default().specialize(&id.into(), &generic_args).unwrap().simulate(inputs)
}

/// Expects to find an extension, wrapping and unwrapping the MemCell types and vectors of the
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
) -> Result<(Vec<i64>, usize), InputError> {
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
#[test_case("int_unwrap_nz", vec![], vec![6] => Ok(vec![6]); "int_unwrap_nz(6)")]
#[test_case("store_temp", vec![type_arg("int")], vec![6] => Ok(vec![6]); "store_temp<int>(6)")]
#[test_case("align_temps", vec![type_arg("int")], vec![] => Ok(vec![]); "align_temps<int>()")]
#[test_case("store_local", vec![type_arg("int")], vec![6] => Ok(vec![6]); "store_local<int>(6)")]
#[test_case("alloc_locals", vec![], vec![] => Ok(vec![]); "alloc_locals()")]
#[test_case("rename", vec![type_arg("int")], vec![6] => Ok(vec![6]); "rename<int>(6)")]
#[test_case("move", vec![type_arg("int")], vec![6] => Ok(vec![6]); "move<int>(6)")]
fn simulate_none_branch(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<i64>,
) -> Result<Vec<i64>, InputError> {
    simulate_invocation(id, generic_args, inputs).map(|(outputs, chosen_branch)| {
        assert_eq!(chosen_branch, 0);
        outputs
    })
}

#[test_case("get_gas", vec![value_arg(4)], vec![vec![]] => InputError::MemoryLayoutMismatch;
            "get_gas<4>(empty)")]
#[test_case("get_gas", vec![value_arg(4)], vec![] => InputError::WrongNumberOfArgs;
            "get_gas<4>()")]
#[test_case("refund_gas", vec![value_arg(5)], vec![vec![]] => InputError::MemoryLayoutMismatch;
            "refund_gas<5>(empty)")]
#[test_case("refund_gas", vec![value_arg(5)], vec![] => InputError::WrongNumberOfArgs;
            "refund_gas<5>()")]
#[test_case("int_add", vec![], vec![vec![1]] => InputError::WrongNumberOfArgs; "int_add(1)")]
#[test_case("int_sub", vec![], vec![vec![1]] => InputError::WrongNumberOfArgs; "int_sub(1)")]
#[test_case("int_mul", vec![], vec![vec![1]] => InputError::WrongNumberOfArgs; "int_mul(1)")]
#[test_case("int_div", vec![], vec![vec![1]] => InputError::WrongNumberOfArgs; "int_div(1)")]
#[test_case("int_mod", vec![], vec![vec![1]] => InputError::WrongNumberOfArgs; "int_mod(1)")]
#[test_case("int_add", vec![value_arg(3)], vec![] => InputError::WrongNumberOfArgs;
            "int_add<3>()")]
#[test_case("int_sub", vec![value_arg(3)], vec![] => InputError::WrongNumberOfArgs;
            "int_sub<3>()")]
#[test_case("int_mul", vec![value_arg(3)], vec![] => InputError::WrongNumberOfArgs;
            "int_mul<3>()")]
#[test_case("int_div", vec![value_arg(5)], vec![] => InputError::WrongNumberOfArgs;
            "int_div<5>()")]
#[test_case("int_mod", vec![value_arg(5)], vec![] => InputError::WrongNumberOfArgs;
            "int_mod<5>()")]
#[test_case("int_const", vec![value_arg(3)], vec![vec![1]] => InputError::WrongNumberOfArgs;
            "int_const<3>(1)")]
#[test_case("int_dup", vec![], vec![] => InputError::WrongNumberOfArgs; "int_dup()")]
#[test_case("int_ignore", vec![], vec![] => InputError::WrongNumberOfArgs; "int_ignore()")]
#[test_case("int_jump_nz", vec![], vec![] => InputError::WrongNumberOfArgs; "int_jump_nz()")]
#[test_case("int_unwrap_nz", vec![], vec![] => InputError::WrongNumberOfArgs; "int_unwrap_nz()")]
#[test_case("store_temp", vec![type_arg("int")], vec![] => InputError::WrongNumberOfArgs;
            "store_temp<int>()")]
#[test_case("align_temps", vec![type_arg("int")], vec![vec![4]] => InputError::WrongNumberOfArgs;
            "align_temps<int>(4)")]
#[test_case("store_local", vec![type_arg("int")], vec![] => InputError::WrongNumberOfArgs;
            "store_local<int>()")]
#[test_case("alloc_locals", vec![], vec![vec![4]] => InputError::WrongNumberOfArgs;
            "alloc_locals(4)")]
#[test_case("rename", vec![type_arg("int")], vec![] => InputError::WrongNumberOfArgs;
            "rename<int>()")]
#[test_case("move", vec![type_arg("int")], vec![] => InputError::WrongNumberOfArgs;
            "move<int>()")]
#[test_case("jump", vec![], vec![vec![4]] => InputError::WrongNumberOfArgs; "jump(4)")]
fn simulate_error(id: &str, generic_args: Vec<GenericArg>, inputs: Vec<Vec<i64>>) -> InputError {
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
