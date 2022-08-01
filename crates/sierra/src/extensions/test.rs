use test_case::test_case;

use super::{ExtensionError, Extensions, SpecializationError};
use crate::program::{Extension, Identifier, TemplateArg, Type};

fn type_arg(name: &str) -> TemplateArg {
    TemplateArg::Type(Type { id: Identifier::Name(name.into()), args: vec![] })
}

#[test_case("NoneExistent", vec![] => Err(SpecializationError::UnsupportedLibCallName); "NoneExistent")]
#[test_case("get_gas", vec![TemplateArg::Value(2)] => Ok(()); "get_gas<2>")]
#[test_case("get_gas", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "get_gas")]
#[test_case("get_gas", vec![TemplateArg::Value(-2)] => Err(SpecializationError::UnsupportedTemplateArg); "get_gas<minus 2>")]
#[test_case("refund_gas", vec![TemplateArg::Value(7)] => Ok(()); "refund_gas<7>")]
#[test_case("refund_gas", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "refund_gas")]
#[test_case("refund_gas", vec![TemplateArg::Value(-7)] => Err(SpecializationError::UnsupportedTemplateArg); "refund_gas<minus 7>")]
#[test_case("int_op", vec![type_arg("Add")] => Ok(()); "int_op<Add>")]
#[test_case("int_op", vec![type_arg("Sub")] => Ok(()); "int_op<Sub>")]
#[test_case("int_op", vec![type_arg("Mul")] => Ok(()); "int_op<Mul>")]
#[test_case("int_op", vec![type_arg("Div")] => Ok(()); "int_op<Div>")]
#[test_case("int_op", vec![type_arg("Mod")] => Ok(()); "int_op<Mod>")]
#[test_case("int_op", vec![type_arg("Add"), TemplateArg::Value(2)] => Ok(()); "int_op<Add, 2>")]
#[test_case("int_op", vec![type_arg("Sub"), TemplateArg::Value(5)] => Ok(()); "int_op<Sub, 5>")]
#[test_case("int_op", vec![type_arg("Mul"), TemplateArg::Value(7)] => Ok(()); "int_op<Mul, 7>")]
#[test_case("int_op", vec![type_arg("Div"), TemplateArg::Value(9)] => Ok(()); "int_op<Div, 9>")]
#[test_case("int_op", vec![type_arg("Mod"), TemplateArg::Value(1)] => Ok(()); "int_op<Mod, 1>")]
#[test_case("int_op", vec![TemplateArg::Value(8)] => Ok(()); "int_op<8>")]
#[test_case("int_op", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "int_op")]
#[test_case("int_ignore", vec![] => Ok(()); "int_ignore")]
#[test_case("int_ignore", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs); "int_ignore<T>")]
#[test_case("int_dup", vec![] => Ok(()); "int_dup")]
#[test_case("int_dup", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs); "int_dup<T>")]
#[test_case("int_jump_nz", vec![] => Ok(()); "int_jump_nz")]
#[test_case("int_jump_nz", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs); "int_jump_nz<T>")]
#[test_case("int_unwrap_nz", vec![] => Ok(()); "int_unwrap_nz")]
#[test_case("int_unwrap_nz", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs); "int_unwrap_nz<T>")]
#[test_case("store", vec![type_arg("int")] => Ok(()); "store<int>")]
#[test_case("store", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "store")]
#[test_case("rename", vec![type_arg("int")] => Ok(()); "rename<int>")]
#[test_case("rename", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "rename")]
#[test_case("move", vec![type_arg("int")] => Ok(()); "move<int>")]
#[test_case("move", vec![] => Err(SpecializationError::UnsupportedTemplateArg); "move no args")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs); "jump<T>")]
fn find_specialization(id: &str, tmpl_args: Vec<TemplateArg>) -> Result<(), SpecializationError> {
    Extensions::default()
        .specialize(&Extension { id: Identifier::Name(id.into()), tmpl_args })
        .map(|_| ())
        .map_err(|error| match error {
            ExtensionError::Specialization { extension: _, error } => error,
        })
}
