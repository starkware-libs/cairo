use bimap::BiMap;
use num_bigint::BigInt;
use test_case::test_case;

use super::core::{CoreLibfunc, CoreType};
use super::lib_func::{SierraApChange, SignatureSpecializationContext, SpecializationContext};
use super::types::TypeInfo;
use super::SpecializationError::{
    self, IndexOutOfRange, MissingFunction, UnsupportedGenericArg, UnsupportedId,
    WrongNumberOfGenericArgs,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::{GenericLibfunc, GenericType};
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
            || id == "felt252".into()
            || id == "u128".into()
            || id == "Option".into()
            || id == "NonZeroFelt252".into()
            || id == "NonZeroInt".into()
            || id == "Tuple<>".into()
            || id == "U128AndFelt252".into()
            || id == "StorageAddress".into()
            || id == "ContractAddress".into()
        {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: true,
                duplicatable: true,
                zero_sized: false,
            })
        } else if id == "ArrayFelt252".into() || id == "ArrayU128".into() {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: true,
                duplicatable: false,
                zero_sized: false,
            })
        } else if id == "UninitializedFelt252".into() || id == "UninitializedU128".into() {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: false,
                droppable: true,
                duplicatable: false,
                zero_sized: true,
            })
        } else if id == "GasBuiltin".into()
            || id == "System".into()
            || id == "RangeCheck".into()
            || id == "NonDupEnum".into()
            || id == "NonDupStruct".into()
        {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: false,
                duplicatable: false,
                zero_sized: false,
            })
        } else if id == "SnapshotRangeCheck".into() || id == "SnapshotArrayU128".into() {
            Some(TypeInfo {
                long_id: self.mapping.get_by_left(&id)?.clone(),
                storable: true,
                droppable: true,
                duplicatable: true,
                zero_sized: false,
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

    fn try_get_function_ap_change(&self, _function_id: &FunctionId) -> Option<SierraApChange> {
        Some(SierraApChange::Unknown)
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

#[test_case("NoneExistent", vec![] => Err(UnsupportedId("NoneExistent".into())); "NoneExistent")]
#[test_case("GasBuiltin", vec![] => Ok(()); "GasBuiltin")]
#[test_case("GasBuiltin", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "GasBuiltin<T>")]
#[test_case("RangeCheck", vec![] => Ok(()); "RangeCheck")]
#[test_case("RangeCheck", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "RangeCheck<T>")]
#[test_case("felt252", vec![] => Ok(()); "felt252")]
#[test_case("felt252", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "felt252<T>")]
#[test_case("u128", vec![] => Ok(()); "u128")]
#[test_case("u128", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "u128<T>")]
#[test_case("Array", vec![type_arg("u128")] => Ok(()); "Array<u128>")]
#[test_case("Array", vec![] => Err(WrongNumberOfGenericArgs); "Array")]
#[test_case("Array", vec![value_arg(5)] => Err(UnsupportedGenericArg); "Array<5>")]
#[test_case("Array", vec![user_type_arg("Unit")] => Err(UnsupportedGenericArg); "Array<Unit>")]
#[test_case("Array", vec![type_arg("UninitializedFelt252")] => Err(UnsupportedGenericArg);
            "Array<UninitializedFelt252>")]
#[test_case("NonZero", vec![type_arg("T")] => Ok(()); "NonZero<T>")]
#[test_case("NonZero", vec![] => Err(WrongNumberOfGenericArgs); "NonZero")]
#[test_case("NonZero", vec![value_arg(5)] => Err(UnsupportedGenericArg); "NonZero<5>")]
#[test_case("Box", vec![type_arg("T")] => Ok(()); "Box<T>")]
#[test_case("Box", vec![] => Err(WrongNumberOfGenericArgs); "Box<>")]
#[test_case("Box", vec![value_arg(5)] => Err(UnsupportedGenericArg); "Box<5>")]
#[test_case("Uninitialized", vec![type_arg("T")] => Ok(()); "Uninitialized<T>")]
#[test_case("Enum", vec![user_type_arg("name")] => Ok(()); "Enum<name>")]
#[test_case("Enum", vec![user_type_arg("name"), type_arg("u128")] => Ok(());
            "Enum<name, u128>")]
#[test_case("Enum", vec![user_type_arg("name"), type_arg("u128"), type_arg("felt252")] => Ok(());
            "Enum<name, u128, felt252>")]
#[test_case("Enum", vec![user_type_arg("name"), value_arg(5)] => Err(UnsupportedGenericArg);
            "Enum<name, 5>")]
#[test_case("Enum", vec![user_type_arg("name"), type_arg("UninitializedFelt252")]
            => Err(UnsupportedGenericArg);
            "Enum<name, UninitializedFelt252>")]
#[test_case("Enum", vec![type_arg("u128"), type_arg("felt252")] => Err(UnsupportedGenericArg);
            "Enum<u128, felt252>")]
#[test_case("Struct", vec![user_type_arg("Unit")] => Ok(()); "Struct<Unit>")]
#[test_case("Struct", vec![user_type_arg("Wrap"), type_arg("u128")] => Ok(());
            "Struct<Wrap, u128>")]
#[test_case("Struct", vec![user_type_arg("Pair"), type_arg("u128"), type_arg("felt252")] => Ok(());
            "Struct<Pair, u128, felt252>")]
#[test_case("Struct", vec![user_type_arg("name"), value_arg(5)] => Err(UnsupportedGenericArg);
            "Struct<name, 5>")]
#[test_case("Struct", vec![user_type_arg("name"), type_arg("UninitializedFelt252")]
            => Err(UnsupportedGenericArg);
            "Struct<name, UninitializedFelt252>")]
#[test_case("Struct", vec![type_arg("u128"), type_arg("felt252")] => Err(UnsupportedGenericArg);
            "Struct<u128, felt252>")]
#[test_case("System", vec![] => Ok(()); "System")]
#[test_case("StorageBaseAddress", vec![] => Ok(()); "StorageBaseAddress")]
#[test_case("Snapshot", vec![type_arg("RangeCheck")] => Ok(()); "Snapshot<RangeCheck>")]
#[test_case("Snapshot", vec![type_arg("felt252")] => Err(UnsupportedGenericArg); "Snapshot<felt252>")]
#[test_case("Snapshot", vec![type_arg("UninitializedFelt252")] => Err(UnsupportedGenericArg);
            "Snapshot<UninitializedFelt252>")]
#[test_case("Snapshot", vec![type_arg("SnapshotRangeCheck")] => Err(UnsupportedGenericArg);
            "Snapshot<SnapshotRangeCheck>")]
fn find_type_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreType::by_id(&id.into())
        .ok_or(UnsupportedId(id.into()))?
        .specialize(&MockSpecializationContext::new(), &generic_args)
        .map(|_| ())
}

#[test_case("NoneExistent", vec![] => Err(UnsupportedId("NoneExistent".into())); "NoneExistent")]
#[test_case("function_call", vec![GenericArg::UserFunc("UnregisteredFunction".into())]
            => Err(MissingFunction("UnregisteredFunction".into()));
            "function_call<&UnregisteredFunction>")]
#[test_case("function_call", vec![GenericArg::UserFunc("RegisteredFunction".into())]
            => Ok(()); "function_call<&RegisteredFunction>")]
#[test_case("function_call", vec![] => Err(UnsupportedGenericArg); "function_call")]
#[test_case("array_new", vec![] => Err(WrongNumberOfGenericArgs); "array_new")]
#[test_case("array_new", vec![type_arg("u128")] => Ok(()); "array_new<u128>")]
#[test_case("array_append", vec![] => Err(WrongNumberOfGenericArgs); "array_append")]
#[test_case("array_append", vec![type_arg("u128")] => Ok(()); "array_append<u128>")]
#[test_case("array_get", vec![] => Err(WrongNumberOfGenericArgs); "array_get")]
#[test_case("array_get", vec![type_arg("u128")] => Ok(()); "array_get<u128>")]
#[test_case("array_len", vec![] => Err(WrongNumberOfGenericArgs); "array_len")]
#[test_case("array_len", vec![type_arg("u128")] => Ok(()); "array_len<u128>")]
#[test_case("withdraw_gas", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "withdraw_gas<0>")]
#[test_case("withdraw_gas", vec![] => Ok(()); "withdraw_gas")]
#[test_case("redeposit_gas", vec![value_arg(0)] => Err(WrongNumberOfGenericArgs); "redeposit_gas<0>")]
#[test_case("redeposit_gas", vec![] => Ok(()); "redeposit_gas")]
#[test_case("felt252_add", vec![] => Ok(()); "felt252_add")]
#[test_case("felt252_add_const", vec![value_arg(0)] =>  Ok(()); "felt252_add_const<0>")]
#[test_case("felt252_mul", vec![] => Ok(()); "felt252_mul")]
#[test_case("felt252_mul_const", vec![value_arg(0)] =>  Ok(()); "felt252_mul_const<0>")]
#[test_case("felt252_is_zero", vec![] => Ok(()); "felt252_is_zero<>")]
#[test_case("felt252_is_zero", vec![type_arg("felt252")]
            => Err(WrongNumberOfGenericArgs); "felt252_is_zero<int>")]
#[test_case("u128_overflowing_add", vec![] => Ok(()); "u128_overflowing_add")]
#[test_case("u128_overflowing_sub", vec![] => Ok(()); "u128_overflowing_sub")]
#[test_case("u128_safe_divmod", vec![] => Ok(()); "u128_safe_divmod")]
#[test_case("u128_const", vec![value_arg(8)] => Ok(()); "u128_const<8>")]
#[test_case("u128_const", vec![] => Err(UnsupportedGenericArg); "u128_const")]
#[test_case("storage_base_address_const", vec![value_arg(8)] => Ok(()); "storage_base_address_const<8>")]
#[test_case("storage_base_address_const", vec![] => Err(UnsupportedGenericArg);
"storage_base_address_const")]
#[test_case("contract_address_const", vec![value_arg(8)] => Ok(()); "contract_address_const<8>")]
#[test_case("contract_address_const", vec![] => Err(UnsupportedGenericArg);
"contract_address_const")]
#[test_case("drop", vec![type_arg("u128")] => Ok(()); "drop<u128>")]
#[test_case("drop", vec![] => Err(WrongNumberOfGenericArgs); "drop<>")]
#[test_case("drop", vec![type_arg("GasBuiltin")] => Err(UnsupportedGenericArg);
"drop<GasBuiltin>")]
#[test_case("dup", vec![type_arg("u128")] => Ok(()); "dup<u128>")]
#[test_case("dup", vec![] => Err(WrongNumberOfGenericArgs); "dup<>")]
#[test_case("dup", vec![type_arg("GasBuiltin")] => Err(UnsupportedGenericArg);
"dup<GasBuiltin>")]
#[test_case("u128_is_zero", vec![] => Ok(()); "u128_is_zero<>")]
#[test_case("u128_is_zero", vec![type_arg("u128")]
            => Err(WrongNumberOfGenericArgs); "u128_is_zero<u128>")]
#[test_case("unwrap_non_zero", vec![type_arg("u128")] => Ok(()); "unwrap_non_zero<u128>")]
#[test_case("unwrap_non_zero", vec![] => Err(WrongNumberOfGenericArgs); "unwrap_non_zero")]
#[test_case("store_temp", vec![type_arg("u128")] => Ok(()); "store_temp<u128>")]
#[test_case("store_temp", vec![] => Err(WrongNumberOfGenericArgs); "store_temp")]
#[test_case("store_local", vec![type_arg("u128")] => Ok(()); "store_local<u128>")]
#[test_case("store_local", vec![] => Err(WrongNumberOfGenericArgs); "store_local")]
#[test_case("finalize_locals", vec![] => Ok(()); "finalize_locals")]
#[test_case("finalize_locals", vec![type_arg("u128")]
            => Err(WrongNumberOfGenericArgs); "finalize_locals<u128>")]
#[test_case("alloc_local", vec![type_arg("u128")] => Ok(()); "alloc_local<u128>")]
#[test_case("alloc_local", vec![] => Err(WrongNumberOfGenericArgs); "alloc_local<>")]
#[test_case("rename", vec![type_arg("u128")] => Ok(()); "rename<u128>")]
#[test_case("rename", vec![] => Err(WrongNumberOfGenericArgs); "rename")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(WrongNumberOfGenericArgs); "jump<T>")]
#[test_case("revoke_ap_tracking", vec![] => Ok(()); "revoke_ap_tracking")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(0)] => Ok(());
"enum_init<Option,0>")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(1)] =>
Ok(());"enum_init<Option,1>")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(2)]
            => Err(IndexOutOfRange{index: BigInt::from(2), range_size: 2});
"enum_init<Option,2>")]
#[test_case("enum_init", vec![type_arg("Option"), value_arg(-3)]
            => Err(IndexOutOfRange{index: BigInt::from(-3), range_size: 2});
"enum_init<Option,-3>")]
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
#[test_case("enum_snapshot_match", vec![type_arg("Option")] => Ok(()); "enum_snapshot_match<Option>")]
#[test_case("enum_snapshot_match", vec![type_arg("NonDupEnum")] => Ok(()); "enum_snapshot_match<NonDupEnum>")]
#[test_case("struct_construct", vec![type_arg("U128AndFelt252")] => Ok(());
            "struct_construct<U128AndFelt252>")]
#[test_case("struct_construct", vec![value_arg(4)] => Err(UnsupportedGenericArg);
            "struct_construct<4>")]
#[test_case("struct_deconstruct", vec![type_arg("U128AndFelt252")] => Ok(());
            "struct_deconstruct<U128AndFelt252>")]
#[test_case("struct_deconstruct", vec![value_arg(4)] => Err(UnsupportedGenericArg);
            "struct_deconstruct<4>")]
#[test_case("struct_snapshot_deconstruct", vec![type_arg("U128AndFelt252")] => Ok(());
            "struct_snapshot_deconstruct<U128AndFelt252>")]
#[test_case("struct_snapshot_deconstruct", vec![type_arg("NonDupStruct")] => Ok(());
            "struct_snapshot_deconstruct<NonDupStruct>")]
#[test_case("storage_read_syscall", vec![] => Ok(()); "storage_read_syscall")]
#[test_case("storage_write_syscall", vec![] => Ok(()); "storage_write_syscall")]
#[test_case("snapshot_take", vec![type_arg("RangeCheck")] => Ok(()); "snapshot_take<RangeCheck>")]
#[test_case("snapshot_take", vec![type_arg("NonDupStruct")] => Ok(());
            "snapshot_take<NonDupStruct>")]
#[test_case("snapshot_take", vec![type_arg("NonDupEnum")] => Ok(()); "snapshot_take<NonDupEnum>")]
#[test_case("snapshot_take", vec![type_arg("felt252")] => Ok(()); "snapshot_take<felt252>")]
#[test_case("snapshot_take", vec![type_arg("SnapshotRangeCheck")] => Ok(());
            "snapshot_take<SnapshotRangeCheck>")]
fn find_libfunc_specialization(
    id: &str,
    generic_args: Vec<GenericArg>,
) -> Result<(), SpecializationError> {
    CoreLibfunc::by_id(&id.into())
        .ok_or(UnsupportedId(id.into()))?
        .specialize(&MockSpecializationContext::new(), &generic_args)
        .map(|_| ())
}
