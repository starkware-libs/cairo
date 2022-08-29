use std::collections::HashMap;

use test_case::test_case;

use super::core::{CoreLibFunc, CoreType};
use super::lib_func::SpecializationContext;
use super::SpecializationError::{
    self, MissingFunction, UnsupportedGenericArg, UnsupportedId, WrongNumberOfGenericArgs,
};
use crate::extensions::{GenericLibFunc, GenericType};
use crate::program::{Function, GenericArg, StatementIdx};

fn type_arg(name: &str) -> GenericArg {
    GenericArg::Type(name.into())
}

fn value_arg(v: i64) -> GenericArg {
    GenericArg::Value(v)
}

#[test_case("NoneExistent", vec![] => Err(UnsupportedId); "NoneExistent")]
#[test_case("GasBuiltin", vec![] => Ok(()); "GasBuiltin")]
#[test_case("GasBuiltin", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "GasBuiltin<T>")]
#[test_case("felt", vec![] => Ok(()); "felt")]
#[test_case("felt", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "felt<T>")]
#[test_case("int", vec![] => Ok(()); "int")]
#[test_case("int", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "int<T>")]
#[test_case("NonZero", vec![type_arg("T")] => Ok(()); "NonZero<T>")]
#[test_case("NonZero", vec![] => Err(UnsupportedGenericArg); "NonZero")]
fn find_type_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreType::by_id(&id.into()).ok_or(UnsupportedId)?.specialize(&generic_args).map(|_| ())
}

#[test_case("NoneExistent", vec![] => Err(UnsupportedId); "NoneExistent")]
#[test_case("function_call", vec![GenericArg::UserFunc("UnregisteredFunction".into())]
             => Err(MissingFunction("UnregisteredFunction".into()));
             "function_call<&UnregisteredFunction>")]
#[test_case("function_call", vec![GenericArg::UserFunc("RegisteredFunction".into())] => Ok(());
            "function_call<&RegisteredFunction>")]
#[test_case("function_call", vec![] => Err(UnsupportedGenericArg); "function_call")]
#[test_case("get_gas", vec![value_arg(2)] => Ok(()); "get_gas<2>")]
#[test_case("get_gas", vec![] => Err(UnsupportedGenericArg); "get_gas")]
#[test_case("get_gas", vec![value_arg(-2)] => Err(UnsupportedGenericArg); "get_gas<minus 2>")]
#[test_case("refund_gas", vec![value_arg(7)] => Ok(()); "refund_gas<7>")]
#[test_case("refund_gas", vec![] => Err(UnsupportedGenericArg); "refund_gas")]
#[test_case("refund_gas", vec![value_arg(-7)] => Err(UnsupportedGenericArg);
            "refund_gas<minus 7>")]
#[test_case("felt_add", vec![] => Ok(()); "felt_add")]
#[test_case("felt_add", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "int_add<0>")]
#[test_case("felt_mul", vec![] => Ok(()); "felt_mul")]
#[test_case("felt_mul", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "int_mul<0>")]
#[test_case("felt_dup", vec![] => Ok(()); "felt_dup")]
#[test_case("felt_dup", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "felt_dup<0>")]
#[test_case("int_add", vec![] => Ok(()); "int_add")]
#[test_case("int_sub", vec![] => Ok(()); "int_sub")]
#[test_case("int_mul", vec![] => Ok(()); "int_mul")]
#[test_case("int_div", vec![] => Ok(()); "int_div")]
#[test_case("int_mod", vec![] => Ok(()); "int_mod")]
#[test_case("int_add", vec![value_arg(2)] => Ok(()); "int_add<2>")]
#[test_case("int_sub", vec![value_arg(5)] => Ok(()); "int_sub<5>")]
#[test_case("int_mul", vec![value_arg(7)] => Ok(()); "int_mul<7>")]
#[test_case("int_div", vec![value_arg(9)] => Ok(()); "int_div<9>")]
#[test_case("int_div", vec![value_arg(0)] => Err(UnsupportedGenericArg); "int_div<0>")]
#[test_case("int_mod", vec![value_arg(1)] => Ok(()); "int_mod<1>")]
#[test_case("int_mod", vec![value_arg(0)] => Err(UnsupportedGenericArg); "int_mod<0>")]
#[test_case("int_const", vec![value_arg(8)] => Ok(()); "int_const<8>")]
#[test_case("int_const", vec![] => Err(UnsupportedGenericArg); "int_const")]
#[test_case("int_ignore", vec![] => Ok(()); "int_ignore")]
#[test_case("int_ignore", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "int_ignore<T>")]
#[test_case("int_dup", vec![] => Ok(()); "int_dup")]
#[test_case("int_dup", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "int_dup<T>")]
#[test_case("int_jump_nz", vec![] => Ok(()); "int_jump_nz")]
#[test_case("int_jump_nz", vec![type_arg("int")] => Err(WrongNumberOfGenericArgs); "int_jump_nz<int>")]
#[test_case("unwrap_nz", vec![type_arg("int")] => Ok(()); "unwrap_nz<int>")]
#[test_case("unwrap_nz", vec![] => Err(UnsupportedGenericArg); "unwrap_nz")]
#[test_case("store_temp", vec![type_arg("int")] => Ok(()); "store_temp<int>")]
#[test_case("store_temp", vec![] => Err(UnsupportedGenericArg); "store_temp")]
#[test_case("align_temps", vec![type_arg("int")] => Ok(()); "align_temps<int>")]
#[test_case("align_temps", vec![] => Err(UnsupportedGenericArg); "align_temps")]
#[test_case("store_local", vec![type_arg("int")] => Ok(()); "store_local<int>")]
#[test_case("store_local", vec![] => Err(UnsupportedGenericArg); "store_local")]
#[test_case("alloc_locals", vec![] => Ok(()); "alloc_locals")]
#[test_case("alloc_locals", vec![type_arg("int")] => Err(WrongNumberOfGenericArgs);
            "alloc_locals<int>")]
#[test_case("rename", vec![type_arg("int")] => Ok(()); "rename<int>")]
#[test_case("rename", vec![] => Err(UnsupportedGenericArg); "rename")]
#[test_case("move", vec![type_arg("int")] => Ok(()); "move<int>")]
#[test_case("move", vec![] => Err(UnsupportedGenericArg); "move no args")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "jump<T>")]
fn find_libfunc_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    let functions = &HashMap::from([(
        "RegisteredFunction".into(),
        Function {
            id: "RegisteredFunction".into(),
            entry: StatementIdx(5),
            ret_types: vec![],
            params: vec![],
        },
    )]);
    CoreLibFunc::by_id(&id.into())
        .ok_or(UnsupportedId)?
        .specialize(
            SpecializationContext {
                concrete_type_ids: &HashMap::from([
                    (("felt".into(), &[][..]), "felt".into()),
                    (("int".into(), &[][..]), "int".into()),
                    (("NonZero".into(), &[type_arg("int")][..]), "NonZeroInt".into()),
                    (("Deferred".into(), &[type_arg("int")][..]), "DeferredInt".into()),
                    (("GasBuiltin".into(), &[][..]), "GasBuiltin".into()),
                    (
                        ("Deferred".into(), &[type_arg("GasBuiltin")][..]),
                        "DeferredGasBuiltin".into(),
                    ),
                ]),
                functions,
            },
            &generic_args,
        )
        .map(|_| ())
}
