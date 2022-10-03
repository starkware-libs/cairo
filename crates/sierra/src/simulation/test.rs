use test_case::test_case;

use super::value::CoreValue::{self, Array, GasBuiltin, Integer, NonZero, Uninitialized};
use super::LibFuncSimulationError::{
    self, FunctionSimulationError, MemoryLayoutMismatch, WrongNumberOfArgs,
};
use super::{core, SimulationError};
use crate::extensions::core::CoreLibFunc;
use crate::extensions::lib_func::{SignatureSpecializationContext, SpecializationContext};
use crate::extensions::types::TypeInfo;
use crate::extensions::GenericLibFunc;
use crate::ids::{ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::{Function, FunctionSignature, GenericArg, StatementIdx};

fn type_arg(name: &str) -> GenericArg {
    GenericArg::Type(name.into())
}

fn value_arg(v: i64) -> GenericArg {
    GenericArg::Value(v)
}

fn user_func_arg(name: &str) -> GenericArg {
    GenericArg::UserFunc(name.into())
}

struct MockSpecializationContext {}

impl SpecializationContext for MockSpecializationContext {
    fn upcast(&self) -> &dyn SignatureSpecializationContext {
        self
    }

    fn try_get_function(&self, function_id: &FunctionId) -> Option<Function> {
        ["drop_all_inputs", "identity", "unimplemented"]
            .into_iter()
            .map(|name| -> FunctionId { name.into() })
            .find(|id: &FunctionId| id == function_id)
            .map(|_| Function::new(function_id.clone(), vec![], vec![], StatementIdx(0)))
    }
}
impl SignatureSpecializationContext for MockSpecializationContext {
    fn try_get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Option<ConcreteTypeId> {
        match (id, &generic_args) {
            (id, &[]) if id == "int".into() => Some("int".into()),
            (id, &[GenericArg::Type(ty)]) if id == "NonZero".into() && ty == &"int".into() => {
                Some("NonZeroInt".into())
            }
            (id, &[GenericArg::Type(ty)])
                if id == "uninitialized".into() && ty == &"int".into() =>
            {
                Some("UninitializedInt".into())
            }
            (id, &[GenericArg::Type(ty)]) if id == "Array".into() && ty == &"int".into() => {
                Some("ArrayInt".into())
            }
            (id, &[]) if id == "GasBuiltin".into() => Some("GasBuiltin".into()),
            _ => None,
        }
    }

    fn try_get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        if id == "int".into() || id == "NonZeroInt".into() {
            Some(TypeInfo { storable: true, droppable: true, duplicatable: true })
        } else if id == "UninitializedInt".into() {
            Some(TypeInfo { storable: false, droppable: true, duplicatable: false })
        } else {
            None
        }
    }

    fn try_get_function_signature(&self, function_id: &FunctionId) -> Option<FunctionSignature> {
        self.try_get_function(function_id).map(|f| f.signature)
    }
}

/// Expects to find a libfunc and simulate it.
fn simulate(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    core::simulate(
        &CoreLibFunc::by_id(&id.into())
            .unwrap()
            .specialize(&MockSpecializationContext {}, &generic_args)
            .unwrap(),
        inputs,
        || Some(4),
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

#[test_case("get_gas", vec![], vec![GasBuiltin(5)] => Ok((vec![GasBuiltin(1)], 0)); "get_gas(5)")]
#[test_case("get_gas", vec![], vec![GasBuiltin(2)] => Ok((vec![GasBuiltin(2)], 1)); "get_gas(2)")]
#[test_case("int_jump_nz", vec![], vec![Integer(2)] => Ok((vec![NonZero(Box::new(Integer(2)))], 0)); "int_jump_nz(2)")]
#[test_case("int_jump_nz", vec![], vec![Integer(0)] => Ok((vec![], 1)); "int_jump_nz(0)")]
#[test_case("jump", vec![], vec![] => Ok((vec![], 0)); "jump()")]
fn simulate_branch(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    simulate(id, generic_args, inputs)
}

/// Tests for simulation of a non branch invocations.
#[test_case("refund_gas", vec![], vec![GasBuiltin(2)] => Ok(vec![GasBuiltin(6)]); "refund_gas(2)")]
#[test_case("array_new", vec![type_arg("int")], vec![] => Ok(vec![Array(vec![])]); "array_new()")]
#[test_case("array_append", vec![type_arg("int")], vec![Array(vec![]), Integer(4)] =>
            Ok(vec![Array(vec![Integer(4)])]); "array_append([], 4)")]
#[test_case("int_add", vec![], vec![Integer(2), Integer(3)] => Ok(vec![Integer(5)]); "int_add(2, 3)")]
#[test_case("int_sub", vec![], vec![Integer(5), Integer(3)] => Ok(vec![Integer(2)]); "int_sub(5, 3)")]
#[test_case("int_mul", vec![], vec![Integer(5), Integer(3)] => Ok(vec![Integer(15)]); "int_mul(5, 3)")]
#[test_case("int_div", vec![], vec![Integer(32), NonZero(Box::new(Integer(5)))] => Ok(vec![Integer(6)]); "int_div(32, 5)")]
#[test_case("int_mod", vec![], vec![Integer(32), NonZero(Box::new(Integer(5)))] => Ok(vec![Integer(2)]); "int_mod(32, 5)")]
#[test_case("int_add", vec![value_arg(3)], vec![Integer(2)] => Ok(vec![Integer(5)]); "int_add<3>(2)")]
#[test_case("int_sub", vec![value_arg(3)], vec![Integer(5)] => Ok(vec![Integer(2)]); "int_sub<3>(5)")]
#[test_case("int_mul", vec![value_arg(3)], vec![Integer(5)] => Ok(vec![Integer(15)]); "int_mul<3>(5)")]
#[test_case("int_div", vec![value_arg(5)], vec![Integer(32)] => Ok(vec![Integer(6)]); "int_div<5>(32)")]
#[test_case("int_mod", vec![value_arg(5)], vec![Integer(32)] => Ok(vec![Integer(2)]); "int_mod<5>(32)")]
#[test_case("int_const", vec![value_arg(3)], vec![] => Ok(vec![Integer(3)]); "int_const<3>()")]
#[test_case("dup", vec![type_arg("int")], vec![Integer(24)] => Ok(vec![Integer(24), Integer(24)]); "dup<int>(24)")]
#[test_case("drop", vec![type_arg("int")], vec![Integer(2)] => Ok(vec![]); "drop<int>(2)")]
#[test_case("unwrap_nz", vec![type_arg("int")], vec![NonZero(Box::new(Integer(6)))] => Ok(vec![Integer(6)]); "unwrap_nz<int>(6)")]
#[test_case("store_temp", vec![type_arg("int")], vec![Integer(6)] => Ok(vec![Integer(6)]); "store_temp<int>(6)")]
#[test_case("align_temps", vec![type_arg("int")], vec![] => Ok(vec![]); "align_temps<int>()")]
#[test_case("store_local", vec![type_arg("int")], vec![Uninitialized, Integer(6)] => Ok(vec![Integer(6)]);
 "store_local<int>(_, 6)")]
#[test_case("finalize_locals", vec![], vec![] => Ok(vec![]); "finalize_locals()")]
#[test_case("rename", vec![type_arg("int")], vec![Integer(6)] => Ok(vec![Integer(6)]); "rename<int>(6)")]
#[test_case("function_call", vec![user_func_arg("drop_all_inputs")], vec![Integer(3), Integer(5)] => Ok(vec![]);
            "function_call<drop_all_inputs>()")]
#[test_case("function_call", vec![user_func_arg("identity")], vec![Integer(3), Integer(5)] => Ok(vec![Integer(3), Integer(5)]);
            "function_call<identity>()")]
fn simulate_none_branch(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<CoreValue>,
) -> Result<Vec<CoreValue>, LibFuncSimulationError> {
    simulate(id, generic_args, inputs).map(|(outputs, chosen_branch)| {
        assert_eq!(chosen_branch, 0);
        outputs
    })
}

#[test_case("get_gas", vec![], vec![Uninitialized] => MemoryLayoutMismatch;
            "get_gas(empty)")]
#[test_case("get_gas", vec![], vec![] => WrongNumberOfArgs; "get_gas()")]
#[test_case("refund_gas", vec![], vec![Uninitialized] => MemoryLayoutMismatch;
            "refund_gas(empty)")]
#[test_case("refund_gas", vec![], vec![] => WrongNumberOfArgs; "refund_gas()")]
#[test_case("int_add", vec![], vec![Integer(1)] => WrongNumberOfArgs; "int_add(1)")]
#[test_case("int_sub", vec![], vec![Integer(1)] => WrongNumberOfArgs; "int_sub(1)")]
#[test_case("int_mul", vec![], vec![Integer(1)] => WrongNumberOfArgs; "int_mul(1)")]
#[test_case("int_div", vec![], vec![Integer(1)] => WrongNumberOfArgs; "int_div(1)")]
#[test_case("int_mod", vec![], vec![Integer(1)] => WrongNumberOfArgs; "int_mod(1)")]
#[test_case("int_add", vec![value_arg(3)], vec![] => WrongNumberOfArgs; "int_add<3>()")]
#[test_case("int_sub", vec![value_arg(3)], vec![] => WrongNumberOfArgs; "int_sub<3>()")]
#[test_case("int_mul", vec![value_arg(3)], vec![] => WrongNumberOfArgs; "int_mul<3>()")]
#[test_case("int_div", vec![value_arg(5)], vec![] => WrongNumberOfArgs; "int_div<5>()")]
#[test_case("int_mod", vec![value_arg(5)], vec![] => WrongNumberOfArgs; "int_mod<5>()")]
#[test_case("int_const", vec![value_arg(3)], vec![Integer(1)] => WrongNumberOfArgs;
            "int_const<3>(1)")]
#[test_case("dup", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "dup<int>()")]
#[test_case("drop", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "drop<int>()")]
#[test_case("int_jump_nz", vec![], vec![] => WrongNumberOfArgs; "int_jump_nz<int>()")]
#[test_case("unwrap_nz", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "unwrap_nz<int>()")]
#[test_case("store_temp", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "store_temp<int>()")]
#[test_case("align_temps", vec![type_arg("int")], vec![Integer(1)] => WrongNumberOfArgs;
            "align_temps<int>(4)")]
#[test_case("store_local", vec![type_arg("int")], vec![] => WrongNumberOfArgs;
            "store_local<int>()")]
#[test_case("finalize_locals", vec![], vec![Integer(4)] => WrongNumberOfArgs; "finalize_locals(4)")]
#[test_case("rename", vec![type_arg("int")], vec![] => WrongNumberOfArgs; "rename<int>()")]
#[test_case("jump", vec![], vec![Integer(4)] => WrongNumberOfArgs; "jump(4)")]
#[test_case("function_call", vec![user_func_arg("unimplemented")], vec![] =>
            FunctionSimulationError(
                "unimplemented".into(),
                Box::new(SimulationError::StatementOutOfBounds(StatementIdx(0))));
            "function_call<unimplemented>()")]
fn simulate_error(
    id: &str,
    generic_args: Vec<GenericArg>,
    inputs: Vec<CoreValue>,
) -> LibFuncSimulationError {
    simulate(id, generic_args, inputs).err().unwrap()
}
