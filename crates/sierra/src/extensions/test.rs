use test_case::test_case;

use super::core::{CoreLibFunc, CoreType};
use super::lib_func::{SignatureSpecializationContext, SpecializationContext};
use super::types::{TypeInfo, TypeSpecializationContext};
use super::SpecializationError::{
    self, MissingFunction, UnsupportedGenericArg, UnsupportedId, WrongNumberOfGenericArgs,
};
use crate::extensions::{GenericLibFunc, GenericType};
use crate::ids::{ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::{Function, FunctionSignature, GenericArg, StatementIdx};

fn type_arg(name: &str) -> GenericArg {
    GenericArg::Type(name.into())
}

fn value_arg(v: i64) -> GenericArg {
    GenericArg::Value(v)
}

struct MockSpecializationContext {}
impl TypeSpecializationContext for MockSpecializationContext {
    fn get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        match id {
            id if id == "T".into()
                || id == "felt".into()
                || id == "int".into()
                || id == "NonZeroFelt".into()
                || id == "NonZeroInt".into() =>
            {
                Some(TypeInfo { storable: true, droppable: true, duplicatable: true })
            }
            id if id == "UninitializedFelt".into() || id == "UninitializedInt".into() => {
                Some(TypeInfo { storable: false, droppable: true, duplicatable: false })
            }
            id if id == "GasBuiltin".into() => {
                Some(TypeInfo { storable: true, droppable: false, duplicatable: false })
            }
            _ => None,
        }
    }
}
impl SignatureSpecializationContext for MockSpecializationContext {
    fn get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Option<ConcreteTypeId> {
        match (id, &generic_args) {
            (id, &[]) if id == "felt".into() => Some("felt".into()),
            (id, &[]) if id == "int".into() => Some("int".into()),
            (id, &[GenericArg::Type(ty)]) if id == "NonZero".into() && ty == &"felt".into() => {
                Some("NonZeroFelt".into())
            }
            (id, &[GenericArg::Type(ty)]) if id == "NonZero".into() && ty == &"int".into() => {
                Some("NonZeroInt".into())
            }
            (id, &[GenericArg::Type(ty)])
                if id == "uninitialized".into() && ty == &"felt".into() =>
            {
                Some("UninitializedFelt".into())
            }
            (id, &[GenericArg::Type(ty)])
                if id == "uninitialized".into() && ty == &"int".into() =>
            {
                Some("UninitializedInt".into())
            }
            (id, &[]) if id == "GasBuiltin".into() => Some("GasBuiltin".into()),
            _ => None,
        }
    }

    fn get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        <Self as TypeSpecializationContext>::get_type_info(self, id)
    }

    fn get_function_signature(&self, function_id: &FunctionId) -> Option<FunctionSignature> {
        self.get_function(function_id).map(|f| f.signature)
    }
}
impl SpecializationContext for MockSpecializationContext {
    fn upcast(&self) -> &dyn SignatureSpecializationContext {
        self
    }

    fn get_function(&self, function_id: &FunctionId) -> Option<Function> {
        match function_id {
            id if id == &"RegisteredFunction".into() => {
                Some(Function::new("RegisteredFunction".into(), vec![], vec![], StatementIdx(5)))
            }
            _ => None,
        }
    }
}

#[test_case("NoneExistent", vec![] => Err(UnsupportedId); "NoneExistent")]
#[test_case("GasBuiltin", vec![] => Ok(()); "GasBuiltin")]
#[test_case("GasBuiltin", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "GasBuiltin<T>")]
#[test_case("felt", vec![] => Ok(()); "felt")]
#[test_case("felt", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "felt<T>")]
#[test_case("int", vec![] => Ok(()); "int")]
#[test_case("int", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "int<T>")]
#[test_case("NonZero", vec![type_arg("T")] => Ok(()); "NonZero<T>")]
#[test_case("NonZero", vec![] => Err(WrongNumberOfGenericArgs); "NonZero")]
#[test_case("NonZero", vec![value_arg(5)] => Err(UnsupportedGenericArg); "NonZero<5>")]
#[test_case("Ref", vec![type_arg("T")] => Ok(()); "Ref<T>")]
#[test_case("Ref", vec![] => Err(WrongNumberOfGenericArgs); "Ref<>")]
#[test_case("Ref", vec![value_arg(5)] => Err(UnsupportedGenericArg); "Ref<5>")]
#[test_case("uninitialized", vec![type_arg("T")] => Ok(()); "uninitialized<T>")]
fn find_type_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreType::by_id(&id.into())
        .ok_or(UnsupportedId)?
        .specialize(&MockSpecializationContext {}, &generic_args)
        .map(|_| ())
}

#[test_case("NoneExistent", vec![] => Err(UnsupportedId); "NoneExistent")]
#[test_case("function_call", vec![GenericArg::UserFunc("UnregisteredFunction".into())]
             => Err(MissingFunction("UnregisteredFunction".into()));
             "function_call<&UnregisteredFunction>")]
#[test_case("function_call", vec![GenericArg::UserFunc("RegisteredFunction".into())] => Ok(());
            "function_call<&RegisteredFunction>")]
#[test_case("function_call", vec![] => Err(UnsupportedGenericArg); "function_call")]
#[test_case("get_gas", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "get_gas<0>")]
#[test_case("get_gas", vec![] => Ok(()); "get_gas")]
#[test_case("refund_gas", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "refund_gas<0>")]
#[test_case("refund_gas", vec![] => Ok(()); "refund_gas")]
#[test_case("felt_add", vec![] => Ok(()); "felt_add")]
#[test_case("felt_add", vec![value_arg(0)] =>  Ok(()); "felt_add<0>")]
#[test_case("felt_mul", vec![] => Ok(()); "felt_mul")]
#[test_case("felt_mul", vec![value_arg(0)] =>  Ok(()); "felt_mul<0>")]
#[test_case("felt_dup", vec![] => Ok(()); "felt_dup")]
#[test_case("felt_dup", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "felt_dup<0>")]
#[test_case("felt_jump_nz", vec![] => Ok(()); "felt_jump_nz<>")]
#[test_case("felt_jump_nz", vec![type_arg("felt")] => Err(WrongNumberOfGenericArgs);
            "felt_jump_nz<int>")]
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
#[test_case("int_drop", vec![] => Ok(()); "int_drop")]
#[test_case("int_drop", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "int_drop<T>")]
#[test_case("int_dup", vec![] => Ok(()); "int_dup")]
#[test_case("int_dup", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "int_dup<T>")]
#[test_case("int_jump_nz", vec![] => Ok(()); "int_jump_nz<>")]
#[test_case("int_jump_nz", vec![type_arg("int")] => Err(WrongNumberOfGenericArgs);
            "int_jump_nz<int>")]
#[test_case("unwrap_nz", vec![type_arg("int")] => Ok(()); "unwrap_nz<int>")]
#[test_case("unwrap_nz", vec![] => Err(WrongNumberOfGenericArgs); "unwrap_nz")]
#[test_case("store_temp", vec![type_arg("int")] => Ok(()); "store_temp<int>")]
#[test_case("store_temp", vec![] => Err(WrongNumberOfGenericArgs); "store_temp")]
#[test_case("align_temps", vec![type_arg("int")] => Ok(()); "align_temps<int>")]
#[test_case("align_temps", vec![value_arg(3)] => Err(UnsupportedGenericArg); "align_temps<3>")]
#[test_case("align_temps", vec![] => Err(WrongNumberOfGenericArgs); "align_temps")]
#[test_case("store_local", vec![type_arg("int")] => Ok(()); "store_local<int>")]
#[test_case("store_local", vec![] => Err(WrongNumberOfGenericArgs); "store_local")]
#[test_case("finalize_locals", vec![] => Ok(()); "finalize_locals")]
#[test_case("finalize_locals", vec![type_arg("int")] => Err(WrongNumberOfGenericArgs);
            "finalize_locals<int>")]
#[test_case("alloc_local", vec![type_arg("int")] => Ok(()); "alloc_local<int>")]
#[test_case("alloc_local", vec![] => Err(WrongNumberOfGenericArgs); "alloc_local<>")]
#[test_case("rename", vec![type_arg("int")] => Ok(()); "rename<int>")]
#[test_case("rename", vec![] => Err(WrongNumberOfGenericArgs); "rename")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "jump<T>")]
#[test_case("revoke_ap_tracking", vec![] => Ok(()); "revoke_ap_tracking")]
fn find_libfunc_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreLibFunc::by_id(&id.into())
        .ok_or(UnsupportedId)?
        .specialize(&MockSpecializationContext {}, &generic_args)
        .map(|_| ())
}
