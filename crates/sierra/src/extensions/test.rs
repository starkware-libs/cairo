use bimap::BiMap;
use num_bigint::BigInt;
use test_case::test_case;

use super::core::{CoreLibFunc, CoreType};
use super::lib_func::{SignatureSpecializationContext, SpecializationContext};
use super::types::TypeInfo;
use super::SpecializationError::{
    self, IndexOutOfRange, MissingFunction, UnsupportedGenericArg, UnsupportedId,
    WrongNumberOfGenericArgs,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::{GenericLibFunc, GenericType};
use crate::ids::{ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, Function, FunctionSignature, GenericArg, StatementIdx};
use crate::test_utils::build_bijective_mapping;

fn type_arg(name: &str) -> GenericArg {
    GenericArg::Type(name.into())
}

fn user_type_arg(name: &str) -> GenericArg {
    GenericArg::UserType(name.into())
}

fn value_arg(v: i64) -> GenericArg {
    GenericArg::Value(BigInt::from(v))
}

struct MockSpecializationContext {
    mapping: BiMap<ConcreteTypeId, ConcreteTypeLongId>,
}
impl MockSpecializationContext {
    pub fn new() -> Self {
        Self { mapping: build_bijective_mapping() }
    }
}

impl TypeSpecializationContext for MockSpecializationContext {
    fn try_get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        if id == "T".into()
            || id == "felt".into()
            || id == "uint128".into()
            || id == "Option".into()
            || id == "NonZeroFelt".into()
            || id == "NonZeroInt".into()
            || id == "Tuple<>".into()
            || id == "Uint128AndFelt".into()
        {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: true,
                duplicatable: true,
                size: 1,
            })
        } else if id == "ArrayFelt".into() || id == "ArrayUint128".into() {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: true,
                duplicatable: false,
                size: 2,
            })
        } else if id == "UninitializedFelt".into() || id == "UninitializedUint128".into() {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: false,
                droppable: true,
                duplicatable: false,
                size: 0,
            })
        } else if id == "GasBuiltin".into() {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: false,
                duplicatable: false,
                size: 1,
            })
        } else {
            None
        }
    }
}
impl SignatureSpecializationContext for MockSpecializationContext {
    fn try_get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Option<ConcreteTypeId> {
        self.mapping
            .get_by_right(&ConcreteTypeLongId {
                generic_id: id,
                generic_args: generic_args.to_vec(),
            })
            .cloned()
    }

    fn try_get_function_signature(&self, function_id: &FunctionId) -> Option<FunctionSignature> {
        self.try_get_function(function_id).map(|f| f.signature)
    }

    fn as_type_specialization_context(&self) -> &dyn TypeSpecializationContext {
        self
    }
}

impl SpecializationContext for MockSpecializationContext {
    fn upcast(&self) -> &dyn SignatureSpecializationContext {
        self
    }

    fn try_get_function(&self, function_id: &FunctionId) -> Option<Function> {
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
#[test_case("RangeCheck", vec![] => Ok(()); "RangeCheck")]
#[test_case("RangeCheck", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "RangeCheck<T>")]
#[test_case("felt", vec![] => Ok(()); "felt")]
#[test_case("felt", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "felt<T>")]
#[test_case("uint128", vec![] => Ok(()); "uint128")]
#[test_case("uint128", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "uint128<T>")]
#[test_case("Array", vec![type_arg("uint128")] => Ok(()); "Array<uint128>")]
#[test_case("Array", vec![] => Err(WrongNumberOfGenericArgs); "Array")]
#[test_case("Array", vec![value_arg(5)] => Err(UnsupportedGenericArg); "Array<5>")]
#[test_case("Array", vec![type_arg("UninitializedFelt")] => Err(UnsupportedGenericArg);
            "Array<UninitializedFelt>")]
#[test_case("NonZero", vec![type_arg("T")] => Ok(()); "NonZero<T>")]
#[test_case("NonZero", vec![] => Err(WrongNumberOfGenericArgs); "NonZero")]
#[test_case("NonZero", vec![value_arg(5)] => Err(UnsupportedGenericArg); "NonZero<5>")]
#[test_case("Box", vec![type_arg("T")] => Ok(()); "Box<T>")]
#[test_case("Box", vec![] => Err(WrongNumberOfGenericArgs); "Box<>")]
#[test_case("Box", vec![value_arg(5)] => Err(UnsupportedGenericArg); "Box<5>")]
#[test_case("Uninitialized", vec![type_arg("T")] => Ok(()); "Uninitialized<T>")]
#[test_case("Enum", vec![user_type_arg("name")] => Ok(()); "Enum<name>")]
#[test_case("Enum", vec![user_type_arg("name"), type_arg("uint128")] => Ok(());
            "Enum<name, uint128>")]
#[test_case("Enum", vec![user_type_arg("name"), type_arg("uint128"), type_arg("felt")] => Ok(());
            "Enum<name, uint128, felt>")]
#[test_case("Enum", vec![user_type_arg("name"), value_arg(5)] => Err(UnsupportedGenericArg);
            "Enum<name, 5>")]
#[test_case("Enum", vec![user_type_arg("name"), type_arg("UninitializedFelt")]
            => Err(UnsupportedGenericArg);
            "Enum<name, UninitializedFelt>")]
#[test_case("Enum", vec![type_arg("uint128"), type_arg("felt")] => Err(UnsupportedGenericArg);
            "Enum<uint128, felt>")]
#[test_case("Struct", vec![user_type_arg("Unit")] => Ok(()); "Struct<Unit>")]
#[test_case("Struct", vec![user_type_arg("Wrap"), type_arg("uint128")] => Ok(());
            "Struct<Wrap, uint128>")]
#[test_case("Struct", vec![user_type_arg("Pair"), type_arg("uint128"), type_arg("felt")] => Ok(());
            "Struct<Pair, uint128, felt>")]
#[test_case("Struct", vec![user_type_arg("name"), value_arg(5)] => Err(UnsupportedGenericArg);
            "Struct<name, 5>")]
#[test_case("Struct", vec![user_type_arg("name"), type_arg("UninitializedFelt")]
            => Err(UnsupportedGenericArg);
            "Struct<name, UninitializedFelt>")]
#[test_case("Struct", vec![type_arg("uint128"), type_arg("felt")] => Err(UnsupportedGenericArg);
            "Struct<uint128, felt>")]
fn find_type_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreType::by_id(&id.into())
        .ok_or(UnsupportedId)?
        .specialize(&MockSpecializationContext::new(), &generic_args)
        .map(|_| ())
}

#[test_case("NoneExistent", vec![] => Err(UnsupportedId); "NoneExistent")]
#[test_case("function_call", vec![GenericArg::UserFunc("UnregisteredFunction".into())]
            => Err(MissingFunction("UnregisteredFunction".into()));
            "function_call<&UnregisteredFunction>")]
#[test_case("function_call", vec![GenericArg::UserFunc("RegisteredFunction".into())]
            => Ok(()); "function_call<&RegisteredFunction>")]
#[test_case("function_call", vec![] => Err(UnsupportedGenericArg); "function_call")]
#[test_case("array_new", vec![] => Err(WrongNumberOfGenericArgs); "array_new")]
#[test_case("array_new", vec![type_arg("uint128")] => Ok(()); "array_new<uint128>")]
#[test_case("array_append", vec![] => Err(WrongNumberOfGenericArgs); "array_append")]
#[test_case("array_append", vec![type_arg("uint128")] => Ok(()); "array_append<uint128>")]
#[test_case("get_gas", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "get_gas<0>")]
#[test_case("get_gas", vec![] => Ok(()); "get_gas")]
#[test_case("refund_gas", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "refund_gas<0>")]
#[test_case("refund_gas", vec![] => Ok(()); "refund_gas")]
#[test_case("felt_add", vec![] => Ok(()); "felt_add")]
#[test_case("felt_add", vec![value_arg(0)] =>  Ok(()); "felt_add<0>")]
#[test_case("felt_mul", vec![] => Ok(()); "felt_mul")]
#[test_case("felt_mul", vec![value_arg(0)] =>  Ok(()); "felt_mul<0>")]
#[test_case("felt_jump_nz", vec![] => Ok(()); "felt_jump_nz<>")]
#[test_case("felt_jump_nz", vec![type_arg("felt")]
            => Err(WrongNumberOfGenericArgs); "felt_jump_nz<int>")]
#[test_case("uint128_wrapping_add", vec![] => Ok(()); "uint128_wrapping_add")]
#[test_case("uint128_wrapping_sub", vec![] => Ok(()); "uint128_wrapping_sub")]
#[test_case("uint128_wrapping_mul", vec![] => Ok(()); "uint128_wrapping_mul")]
#[test_case("uint128_div", vec![] => Ok(()); "uint128_div")]
#[test_case("uint128_mod", vec![] => Ok(()); "uint128_mod")]
#[test_case("uint128_wrapping_add", vec![value_arg(2)] => Ok(()); "int_add<2>")]
#[test_case("uint128_wrapping_sub", vec![value_arg(5)] => Ok(()); "int_sub<5>")]
#[test_case("uint128_wrapping_mul", vec![value_arg(7)] => Ok(()); "int_mul<7>")]
#[test_case("uint128_div", vec![value_arg(9)] => Ok(()); "uint128_div<9>")]
#[test_case("uint128_div", vec![value_arg(0)] => Err(UnsupportedGenericArg); "uint128_div<0>")]
#[test_case("uint128_mod", vec![value_arg(1)] => Ok(()); "uint128_mod<1>")]
#[test_case("uint128_mod", vec![value_arg(0)] => Err(UnsupportedGenericArg); "uint128_mod<0>")]
#[test_case("uint128_const", vec![value_arg(8)] => Ok(()); "uint128_const<8>")]
#[test_case("uint128_const", vec![] => Err(UnsupportedGenericArg); "uint128_const")]
#[test_case("drop", vec![type_arg("uint128")] => Ok(()); "drop<uint128>")]
#[test_case("drop", vec![] => Err(WrongNumberOfGenericArgs); "drop<>")]
#[test_case("drop", vec![type_arg("GasBuiltin")] => Err(UnsupportedGenericArg); "drop<GasBuiltin>")]
#[test_case("dup", vec![type_arg("uint128")] => Ok(()); "dup<uint128>")]
#[test_case("dup", vec![] => Err(WrongNumberOfGenericArgs); "dup<>")]
#[test_case("dup", vec![type_arg("GasBuiltin")] => Err(UnsupportedGenericArg); "dup<GasBuiltin>")]
#[test_case("uint128_jump_nz", vec![] => Ok(()); "uint128_jump_nz<>")]
#[test_case("uint128_jump_nz", vec![type_arg("uint128")]
            => Err(WrongNumberOfGenericArgs); "uint128_jump_nz<uint128>")]
#[test_case("unwrap_nz", vec![type_arg("uint128")] => Ok(()); "unwrap_nz<uint128>")]
#[test_case("unwrap_nz", vec![] => Err(WrongNumberOfGenericArgs); "unwrap_nz")]
#[test_case("store_temp", vec![type_arg("uint128")] => Ok(()); "store_temp<uint128>")]
#[test_case("store_temp", vec![] => Err(WrongNumberOfGenericArgs); "store_temp")]
#[test_case("align_temps", vec![type_arg("uint128")] => Ok(()); "align_temps<uint128>")]
#[test_case("align_temps", vec![value_arg(3)] => Err(UnsupportedGenericArg); "align_temps<3>")]
#[test_case("align_temps", vec![] => Err(WrongNumberOfGenericArgs); "align_temps")]
#[test_case("store_local", vec![type_arg("uint128")] => Ok(()); "store_local<uint128>")]
#[test_case("store_local", vec![] => Err(WrongNumberOfGenericArgs); "store_local")]
#[test_case("finalize_locals", vec![] => Ok(()); "finalize_locals")]
#[test_case("finalize_locals", vec![type_arg("uint128")]
            => Err(WrongNumberOfGenericArgs); "finalize_locals<uint128>")]
#[test_case("alloc_local", vec![type_arg("uint128")] => Ok(()); "alloc_local<uint128>")]
#[test_case("alloc_local", vec![] => Err(WrongNumberOfGenericArgs); "alloc_local<>")]
#[test_case("rename", vec![type_arg("uint128")] => Ok(()); "rename<uint128>")]
#[test_case("rename", vec![] => Err(WrongNumberOfGenericArgs); "rename")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "jump<T>")]
#[test_case("revoke_ap_tracking", vec![] => Ok(()); "revoke_ap_tracking")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(0)] => Ok(()); "enum_init<Option,0>")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(1)] => Ok(());"enum_init<Option,1>")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(2)]
            => Err(IndexOutOfRange{index: BigInt::from(2), range_size: 2}); "enum_init<Option,2>")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(-3)]
            => Err(IndexOutOfRange{index: BigInt::from(-3), range_size: 2}); "enum_init<Option,-3>")]
#[test_case("enum_init", vec![type_arg("Option")]
            => Err(WrongNumberOfGenericArgs); "enum_init<Option>")]
#[test_case("enum_init", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "enum_init<0>")]
#[test_case("enum_init", vec![] => Err(WrongNumberOfGenericArgs); "enum_init")]
#[test_case("enum_init", vec![value_arg(0),type_arg("Option")]
            => Err(UnsupportedGenericArg); "enum_init<0,Option>")]
#[test_case("enum_init", vec![type_arg("Option"), type_arg("Option")]
            => Err(UnsupportedGenericArg); "enum_init<Option,Option>")]
#[test_case("enum_init", vec![value_arg(0), value_arg(0)]
            => Err(UnsupportedGenericArg); "enum_init<0,0>")]
#[test_case("enum_match", vec![type_arg("Option")] => Ok(()); "enum_match<Option>")]
#[test_case("enum_match", vec![value_arg(4)] => Err(UnsupportedGenericArg); "enum_match<4>")]
#[test_case("enum_match", vec![] => Err(WrongNumberOfGenericArgs); "enum_match")]
#[test_case("struct_construct", vec![type_arg("Uint128AndFelt")] => Ok(());
            "struct_construct<Uint128AndFelt>")]
#[test_case("struct_construct", vec![value_arg(4)] => Err(UnsupportedGenericArg);
            "struct_construct<4>")]
#[test_case("struct_deconstruct", vec![type_arg("Uint128AndFelt")] => Ok(());
            "struct_deconstruct<Uint128AndFelt>")]
#[test_case("struct_deconstruct", vec![value_arg(4)] => Err(UnsupportedGenericArg);
            "struct_deconstruct<4>")]
fn find_libfunc_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreLibFunc::by_id(&id.into())
        .ok_or(UnsupportedId)?
        .specialize(&MockSpecializationContext::new(), &generic_args)
        .map(|_| ())
}
