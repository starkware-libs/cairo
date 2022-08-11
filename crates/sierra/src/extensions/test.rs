use test_case::test_case;

use super::{ExtensionError, Extensions, SpecializationError};
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
#[test_case("get_gas", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "get_gas")]
#[test_case("get_gas", vec![value_arg(-2)] => Err(SpecializationError::UnsupportedTemplateArg);
            "get_gas<minus 2>")]
#[test_case("refund_gas", vec![value_arg(7)] => Ok(()); "refund_gas<7>")]
#[test_case("refund_gas", vec![] => Err(SpecializationError::UnsupportedTemplateArg);
            "refund_gas")]
#[test_case("refund_gas", vec![value_arg(-7)] => Err(SpecializationError::UnsupportedTemplateArg);
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
#[test_case("int_div", vec![value_arg(0)] => Err(SpecializationError::UnsupportedTemplateArg);
            "int_div<0>")]
#[test_case("int_mod", vec![value_arg(1)] => Ok(()); "int_mod<1>")]
#[test_case("int_mod", vec![value_arg(0)] => Err(SpecializationError::UnsupportedTemplateArg);
            "int_mod<0>")]
#[test_case("int_const", vec![value_arg(8)] => Ok(()); "int_const<8>")]
#[test_case("int_const", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "int_const")]
#[test_case("int_ignore", vec![] => Ok(()); "int_ignore")]
#[test_case("int_ignore", vec![type_arg("T")] =>
            Err(SpecializationError::WrongNumberOfTemplateArgs);
            "int_ignore<T>")]
#[test_case("int_dup", vec![] => Ok(()); "int_dup")]
#[test_case("int_dup", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs);
            "int_dup<T>")]
#[test_case("int_jump_nz", vec![] => Ok(()); "int_jump_nz")]
#[test_case("int_jump_nz", vec![type_arg("T")] =>
            Err(SpecializationError::WrongNumberOfTemplateArgs);
            "int_jump_nz<T>")]
#[test_case("int_unwrap_nz", vec![] => Ok(()); "int_unwrap_nz")]
#[test_case("int_unwrap_nz", vec![type_arg("T")] =>
            Err(SpecializationError::WrongNumberOfTemplateArgs);
            "int_unwrap_nz<T>")]
#[test_case("store_temp", vec![type_arg("int")] => Ok(()); "store_temp<int>")]
#[test_case("store_temp", vec![] => Err(SpecializationError::UnsupportedTemplateArg);
            "store_temp")]
#[test_case("align_temps", vec![type_arg("int")] => Ok(()); "align_temps<int>")]
#[test_case("align_temps", vec![] => Err(SpecializationError::UnsupportedTemplateArg);
            "align_temps")]
#[test_case("store_local", vec![type_arg("int")] => Ok(()); "store_local<int>")]
#[test_case("store_local", vec![] => Err(SpecializationError::UnsupportedTemplateArg);
            "store_local")]
#[test_case("alloc_locals", vec![] => Ok(()); "alloc_locals")]
#[test_case("alloc_locals", vec![type_arg("int")] =>
            Err(SpecializationError::WrongNumberOfTemplateArgs);
            "alloc_locals<int>")]
#[test_case("rename", vec![type_arg("int")] => Ok(()); "rename<int>")]
#[test_case("rename", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "rename")]
#[test_case("move", vec![type_arg("int")] => Ok(()); "move<int>")]
#[test_case("move", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "move no args")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs);
            "jump<T>")]
fn find_specialization(id: &str, args: Vec<GenericArg>) -> Result<(), SpecializationError> {
    Extensions::default().specialize(&id.into(), &args).map(|_| ()).map_err(|error| match error {
        ExtensionError::Specialization { extension_id: _, error } => error,
        _ => panic!("Unexpected error."),
    })
}
