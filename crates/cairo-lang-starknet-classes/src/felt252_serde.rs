use std::ops::{Shl, Shr};
use std::sync::LazyLock;

use cairo_lang_sierra::extensions::NamedLibfunc;
use cairo_lang_sierra::extensions::circuit::{
    CircuitFailureGuaranteeVerifyLibFunc, U96LimbsLessThanGuaranteeVerifyLibfunc,
    U96SingleLimbLessThanGuaranteeVerifyLibfunc,
};
use cairo_lang_sierra::extensions::starknet::interoperability::ContractAddressTryFromFelt252Libfunc;
use cairo_lang_sierra::extensions::starknet::secp256::Secp256GetPointFromXLibfunc;
use cairo_lang_sierra::extensions::starknet::secp256k1::Secp256k1;
use cairo_lang_sierra::extensions::starknet::secp256r1::Secp256r1;
use cairo_lang_sierra::extensions::starknet::storage::{
    StorageAddressFromBaseAndOffsetLibfunc, StorageAddressTryFromFelt252Trait,
    StorageBaseAddressFromFelt252Libfunc,
};
use cairo_lang_sierra::extensions::try_from_felt252::TryFromFelt252;
use cairo_lang_sierra::ids::{
    ConcreteLibfuncId, ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId, UserTypeId,
    VarId,
};
use cairo_lang_sierra::program::{
    BranchInfo, BranchTarget, ConcreteLibfuncLongId, ConcreteTypeLongId, DeclaredTypeInfo,
    Function, FunctionSignature, GenericArg, Invocation, LibfuncDeclaration, Param, Program,
    Statement, StatementIdx, TypeDeclaration,
};
use cairo_lang_utils::bigint::BigUintAsHex;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::require;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use num_bigint::{BigInt, BigUint};
use num_traits::{Signed, ToPrimitive};
use smol_str::SmolStr;
use thiserror::Error;

use crate::compiler_version::VersionId;
use crate::felt252_vec_compression::{compress, decompress};
use crate::keccak::starknet_keccak;

#[cfg(test)]
#[path = "felt252_serde_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum Felt252SerdeError {
    #[error("Illegal bigint value during serialization.")]
    BigIntOutOfBounds,
    #[error("Invalid input for deserialization.")]
    InvalidInputForDeserialization,
    #[error(
        "Id `{0}` is too long for serialization. It is longer than {SHORT_STRING_BOUND} \
         characters. Consider adding it in SERDE_SUPPORTED_LONG_IDS."
    )]
    GenericIdTooLong(String),
    #[error("Invalid order of type declarations for serialization.")]
    OutOfOrderTypeDeclarationsForSerialization,
    #[error("Invalid order of libfunc declarations for serialization.")]
    OutOfOrderLibfuncDeclarationsForSerialization,
    #[error("Invalid order of user functions declarations for serialization.")]
    OutOfOrderUserFunctionDeclarationsForSerialization,
    #[error("Invalid function declaration for serialization.")]
    FunctionArgumentsMismatchInSerialization,
}

/// Serializes a Sierra program and the current compiler and Sierra versions into a vector of
/// felt252s.
pub fn sierra_to_felt252s(
    sierra_version: VersionId,
    compiler_version: VersionId,
    program: &Program,
) -> Result<Vec<BigUintAsHex>, Felt252SerdeError> {
    let mut serialized_program = vec![];
    program.serialize(&mut serialized_program)?;
    let mut serialized = vec![];
    sierra_version.serialize(&mut serialized)?;
    compiler_version.serialize(&mut serialized)?;
    compress(&serialized_program, &mut serialized);
    Ok(serialized)
}

/// Partially deserializes a Sierra program represented as a slice of felt252s.
///
/// Returns (sierra_version_id, compiler_version_id, remaining),
/// where 'remaining' are all the felts other than the ones dedicated to the version, unprocessed.
/// See [crate::compiler_version].
// TODO(orizi): Validate that changing this function is ok, and make it iterator based as well.
pub fn version_id_from_felt252s(
    sierra_program: &[BigUintAsHex],
) -> Result<(VersionId, VersionId, &[BigUintAsHex]), Felt252SerdeError> {
    let mut iter = sierra_program.iter().map(|v| &v.value);
    let sierra_version_id = VersionId::deserialize(&mut iter)?;
    let compiler_version_id = VersionId::deserialize(&mut iter)?;
    Ok((sierra_version_id, compiler_version_id, &sierra_program[6..]))
}

/// Deserializes a Sierra program represented as a slice of felt252s.
///
/// Returns (sierra_version_id, compiler_version_id, program).
/// See [crate::compiler_version].
pub fn sierra_from_felt252s(
    sierra_program: &[BigUintAsHex],
) -> Result<(VersionId, VersionId, Program), Felt252SerdeError> {
    let (sierra_version_id, compiler_version_id, remaining) =
        version_id_from_felt252s(sierra_program)?;
    let mut iter =
        decompress(remaining).ok_or(Felt252SerdeError::InvalidInputForDeserialization)?.into_iter();
    Ok((sierra_version_id, compiler_version_id, Program::deserialize(&mut iter)?))
}

/// Trait for serializing and deserializing into a felt252 vector.
trait Felt252Serde: Sized {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError>;
    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError>;
}

// Impls for basic types.

impl Felt252Serde for usize {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex { value: (*self).into() });
        Ok(())
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        input
            .next()
            .and_then(|size| size.to_usize())
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)
    }
}

impl Felt252Serde for u64 {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex { value: (*self).into() });
        Ok(())
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        input
            .next()
            .and_then(|size| size.to_u64())
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)
    }
}

impl<T: Felt252Serde> Felt252Serde for Vec<T> {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        self.len().serialize(output)?;
        for e in self {
            e.serialize(output)?;
        }
        Ok(())
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        let size = usize::deserialize(input)?;
        let mut result = vec_with_bounded_capacity(size, input.len())?;
        for _ in 0..size {
            result.push(T::deserialize(input)?);
        }
        Ok(result)
    }
}

impl Felt252Serde for BigInt {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex {
            value: self.to_biguint().ok_or(Felt252SerdeError::BigIntOutOfBounds)?,
        });
        Ok(())
    }
    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        Ok(input.next().ok_or(Felt252SerdeError::InvalidInputForDeserialization)?.clone().into())
    }
}

impl Felt252Serde for StatementIdx {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        self.0.serialize(output)
    }
    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        Ok(Self(usize::deserialize(input)?))
    }
}

// Impls for generic ids.
const SHORT_STRING_BOUND: usize = 31;
/// A set of all the supported long generic ids.
static SERDE_SUPPORTED_LONG_IDS: LazyLock<OrderedHashSet<&'static str>> = LazyLock::new(|| {
    OrderedHashSet::from_iter([
        StorageAddressFromBaseAndOffsetLibfunc::STR_ID,
        ContractAddressTryFromFelt252Libfunc::STR_ID,
        StorageBaseAddressFromFelt252Libfunc::STR_ID,
        StorageAddressTryFromFelt252Trait::STR_ID,
        Secp256GetPointFromXLibfunc::<Secp256k1>::STR_ID,
        Secp256GetPointFromXLibfunc::<Secp256r1>::STR_ID,
        CircuitFailureGuaranteeVerifyLibFunc::STR_ID,
        U96LimbsLessThanGuaranteeVerifyLibfunc::STR_ID,
        U96SingleLimbLessThanGuaranteeVerifyLibfunc::STR_ID,
    ])
});
/// A mapping of all the long names when fixing them from the hashed keccak representation.
static LONG_NAME_FIX: LazyLock<UnorderedHashMap<BigUint, &'static str>> = LazyLock::new(|| {
    UnorderedHashMap::from_iter(
        SERDE_SUPPORTED_LONG_IDS.iter().map(|name| (starknet_keccak(name.as_bytes()), *name)),
    )
});

macro_rules! generic_id_serde {
    ($Obj:ident) => {
        impl Felt252Serde for $Obj {
            fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
                output.push(BigUintAsHex {
                    value: if self.0.len() <= SHORT_STRING_BOUND {
                        BigUint::from_bytes_be(self.0.as_bytes())
                    } else {
                        if !SERDE_SUPPORTED_LONG_IDS.contains(self.0.as_str()) {
                            return Err(Felt252SerdeError::GenericIdTooLong(self.0.to_string()));
                        }
                        starknet_keccak(self.0.as_bytes())
                    },
                });
                Ok(())
            }
            fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
                input: &mut I,
            ) -> Result<Self, Felt252SerdeError> {
                input
                    .next()
                    .and_then(|id| {
                        LONG_NAME_FIX.get(&id).map(|s| Self(SmolStr::new(s))).or_else(|| {
                            std::str::from_utf8(&id.to_bytes_be()).ok().map(|s| Self(s.into()))
                        })
                    })
                    .ok_or(Felt252SerdeError::InvalidInputForDeserialization)
            }
        }
    };
}

generic_id_serde!(GenericTypeId);
generic_id_serde!(GenericLibfuncId);

impl Felt252Serde for UserTypeId {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex { value: self.id.clone() });
        Ok(())
    }
    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        let first = input.next().ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        Ok(Self { id: first.clone(), debug_name: None })
    }
}

// Impls for other ids.

macro_rules! id_serde {
    ($Obj:ident) => {
        impl Felt252Serde for $Obj {
            fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
                self.id.serialize(output)
            }
            fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
                input: &mut I,
            ) -> Result<Self, Felt252SerdeError> {
                Ok(Self::new(u64::deserialize(input)?))
            }
        }
    };
}

id_serde!(ConcreteTypeId);
id_serde!(ConcreteLibfuncId);
id_serde!(VarId);
id_serde!(FunctionId);

// Impls for structs.

macro_rules! struct_serialize_impl {
    ($obj:ident, $output:ident, { $($field_name:ident),* }) => {
        {
            let __obj = $obj;
            let __output = $output;
            $(Felt252Serde::serialize(&__obj. $field_name, __output)?;)*
            Ok(())
        }
    };
}

macro_rules! struct_deserialize_impl {
    ($input:ident, { $($field_name:ident : $field_type:ty),* }) => {
        $(
            let $field_name = <$field_type>::deserialize($input)?;
        )*
    };
}

macro_rules! struct_serialize {
    ($($field_name:ident),*) => {
        fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
            struct_serialize_impl!(self, output, { $($field_name),* })
        }
    };
}

macro_rules! struct_deserialize {
    ($Obj:ident { $($field_name:ident : $field_type:ty),* }) => {
        fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
            input: &mut I,
        ) -> Result<Self, Felt252SerdeError> {
            struct_deserialize_impl!(input, {$($field_name : $field_type),*});
            Ok(Self {$($field_name),*})
        }
    };
}

macro_rules! struct_serde {
    ($Obj:ident { $($field_name:ident : $field_type:ty),* $(,)? }) => {
        impl Felt252Serde for $Obj {
            struct_serialize! { $($field_name),* }
            struct_deserialize! { $Obj { $($field_name : $field_type),* } }
        }
    }
}

impl Felt252Serde for Program {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        // Type declarations.
        self.type_declarations.len().serialize(output)?;
        for (i, e) in self.type_declarations.iter().enumerate() {
            require(i as u64 == e.id.id)
                .ok_or(Felt252SerdeError::OutOfOrderTypeDeclarationsForSerialization)?;
            ConcreteTypeInfo {
                long_id: e.long_id.clone(),
                declared_type_info: e.declared_type_info.clone(),
            }
            .serialize(output)?;
        }
        // Libfunc declarations.
        self.libfunc_declarations.len().serialize(output)?;
        for (i, e) in self.libfunc_declarations.iter().enumerate() {
            require(i as u64 == e.id.id)
                .ok_or(Felt252SerdeError::OutOfOrderLibfuncDeclarationsForSerialization)?;
            e.long_id.serialize(output)?;
        }
        // Statements.
        Felt252Serde::serialize(&self.statements, output)?;
        // Function declarations.
        self.funcs.len().serialize(output)?;
        for (i, f) in self.funcs.iter().enumerate() {
            require(i as u64 == f.id.id)
                .ok_or(Felt252SerdeError::OutOfOrderUserFunctionDeclarationsForSerialization)?;
            f.signature.serialize(output)?;
            require(f.signature.param_types.len() == f.params.len())
                .ok_or(Felt252SerdeError::FunctionArgumentsMismatchInSerialization)?;
            for (param, ty) in f.params.iter().zip(f.signature.param_types.iter()) {
                require(param.ty == *ty)
                    .ok_or(Felt252SerdeError::FunctionArgumentsMismatchInSerialization)?;
                param.id.serialize(output)?;
            }
            f.entry_point.serialize(output)?;
        }
        Ok(())
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        // Type declarations.
        let size = usize::deserialize(input)?;
        let mut type_declarations = vec_with_bounded_capacity(size, input.len())?;
        for i in 0..size {
            let info = ConcreteTypeInfo::deserialize(input)?;
            type_declarations.push(TypeDeclaration {
                id: ConcreteTypeId::new(i as u64),
                long_id: info.long_id,
                declared_type_info: info.declared_type_info,
            });
        }
        // Libfunc declaration.
        let size = usize::deserialize(input)?;
        let mut libfunc_declarations = vec_with_bounded_capacity(size, input.len())?;
        for i in 0..size {
            let long_id = ConcreteLibfuncLongId::deserialize(input)?;
            libfunc_declarations
                .push(LibfuncDeclaration { id: ConcreteLibfuncId::new(i as u64), long_id });
        }
        // Statements.
        let statements = Felt252Serde::deserialize(input)?;
        // Function declaration.
        let size = usize::deserialize(input)?;
        let mut funcs = vec_with_bounded_capacity(size, input.len())?;
        for i in 0..size {
            let signature = FunctionSignature::deserialize(input)?;
            let params = signature
                .param_types
                .iter()
                .map(|ty| Ok(Param { id: VarId::deserialize(input)?, ty: ty.clone() }))
                .collect::<Result<Vec<_>, _>>()?;
            let entry_point = StatementIdx::deserialize(input)?;
            funcs.push(Function { id: FunctionId::new(i as u64), signature, params, entry_point });
        }
        Ok(Self { type_declarations, libfunc_declarations, statements, funcs })
    }
}

/// Helper struct to serialize and deserialize a `ConcreteTypeLongId` and its optional
/// `DeclaredTypeInfo`.
struct ConcreteTypeInfo {
    long_id: ConcreteTypeLongId,
    declared_type_info: Option<DeclaredTypeInfo>,
}

const TYPE_INFO_MARKER: u64 = 0x8000000000000000;
const TYPE_STORABLE: u64 = 0b0001;
const TYPE_DROPPABLE: u64 = 0b0010;
const TYPE_DUPLICATABLE: u64 = 0b0100;
const TYPE_ZERO_SIZED: u64 = 0b1000;

impl Felt252Serde for ConcreteTypeInfo {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        self.long_id.generic_id.serialize(output)?;
        let len = self.long_id.generic_args.len() as u128;
        let decl_ti_value: u64 = match &self.declared_type_info {
            Some(info) => {
                // The marker guarantees that the type info value is not 0.
                TYPE_INFO_MARKER
                    | (if info.storable { TYPE_STORABLE } else { 0 })
                    | (if info.droppable { TYPE_DROPPABLE } else { 0 })
                    | (if info.duplicatable { TYPE_DUPLICATABLE } else { 0 })
                    | (if info.zero_sized { TYPE_ZERO_SIZED } else { 0 })
            }
            None => 0,
        };
        ((BigInt::from(len) + BigInt::from(decl_ti_value).shl(128)) as BigInt).serialize(output)?;
        for arg in &self.long_id.generic_args {
            arg.serialize(output)?;
        }
        Ok(())
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        let generic_id = GenericTypeId::deserialize(input)?;
        let len_and_decl_ti_value = BigInt::deserialize(input)?;
        let len = (len_and_decl_ti_value.clone() & BigInt::from(u128::MAX))
            .to_usize()
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        let decl_ti_value = (len_and_decl_ti_value.shr(128) as BigInt)
            .to_u64()
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        let mut generic_args = vec_with_bounded_capacity(len, input.len())?;
        for _ in 0..len {
            generic_args.push(GenericArg::deserialize(input)?);
        }
        Ok(Self {
            long_id: ConcreteTypeLongId { generic_id, generic_args },
            declared_type_info: if decl_ti_value == 0 {
                None
            } else {
                Some(DeclaredTypeInfo {
                    storable: (decl_ti_value & TYPE_STORABLE) != 0,
                    droppable: (decl_ti_value & TYPE_DROPPABLE) != 0,
                    duplicatable: (decl_ti_value & TYPE_DUPLICATABLE) != 0,
                    zero_sized: (decl_ti_value & TYPE_ZERO_SIZED) != 0,
                })
            },
        })
    }
}

struct_serde! {
    ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId,
        generic_args: Vec<GenericArg>,
    }
}

struct_serde! {
    FunctionSignature {
        param_types:  Vec<ConcreteTypeId>,
        ret_types:  Vec<ConcreteTypeId>,
    }
}

struct_serde! {
    Invocation {
        libfunc_id: ConcreteLibfuncId,
        args: Vec<VarId>,
        branches: Vec<BranchInfo>,
    }
}

struct_serde! {
    BranchInfo {
        target: BranchTarget,
        results: Vec<VarId>,
    }
}

struct_serde!(VersionId { major: usize, minor: usize, patch: usize });

// Impls for enums.

macro_rules! enum_serialize_impl {
    ($obj:ident, $output:ident, $Obj:ident { $($variant_name:ident = $variant_id:literal),* }) => {
        {
            let __output = $output;
            match $obj {
                $(
                    $Obj::$variant_name(value) => {
                        u64::serialize(&$variant_id, __output)?;
                        Felt252Serde::serialize(value, __output)
                    }
                ),*
            }
        }
    };
}

macro_rules! enum_serialize {
    ($($variant_name:ident = $variant_id:literal),*) => {
        fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
            enum_serialize_impl!(self, output, Self { $($variant_name = $variant_id),* })
        }
    };
}

macro_rules! enum_deserialize {
    ($($variant_name:ident ( $variant_type:ty ) = $variant_id:literal),*) => {
        fn deserialize<'a, I: ExactSizeIterator<Item=&'a BigUint>>(
            input: &mut I
        ) -> Result<Self, Felt252SerdeError> {
            match u64::deserialize(input)? {
                $($variant_id => {
                    Ok(Self::$variant_name(<$variant_type>::deserialize(input)?))
                },)*
                _ => Err(Felt252SerdeError::InvalidInputForDeserialization),
            }
        }
    };
}

macro_rules! enum_serde {
    ($Obj:ident { $($variant_name:ident ( $variant_type:ty ) = $variant_id:literal),* $(,)? }) => {
        impl Felt252Serde for $Obj {
            enum_serialize! { $($variant_name = $variant_id),* }
            enum_deserialize! { $($variant_name($variant_type) = $variant_id),* }
        }
    }
}

enum_serde! {
    Statement {
        Invocation(Invocation) = 0,
        Return(Vec::<VarId>) = 1,
    }
}

/// Custom serialization for `GenericArg` to support negatives in `GenericArg::Value`.
impl Felt252Serde for GenericArg {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        match self {
            GenericArg::UserType(id) => {
                0usize.serialize(output)?;
                id.serialize(output)
            }
            GenericArg::Type(id) => {
                1usize.serialize(output)?;
                id.serialize(output)
            }
            GenericArg::Value(value) if !value.is_negative() => {
                2usize.serialize(output)?;
                value.serialize(output)
            }
            GenericArg::UserFunc(id) => {
                3usize.serialize(output)?;
                id.serialize(output)
            }
            GenericArg::Libfunc(id) => {
                4usize.serialize(output)?;
                id.serialize(output)
            }
            GenericArg::Value(value) => {
                5usize.serialize(output)?;
                (-value).serialize(output)
            }
        }
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        match usize::deserialize(input)? {
            0 => Ok(Self::UserType(UserTypeId::deserialize(input)?)),
            1 => Ok(Self::Type(ConcreteTypeId::deserialize(input)?)),
            2 => Ok(Self::Value(BigInt::deserialize(input)?)),
            3 => Ok(Self::UserFunc(FunctionId::deserialize(input)?)),
            4 => Ok(Self::Libfunc(ConcreteLibfuncId::deserialize(input)?)),
            5 => Ok(Self::Value(-BigInt::deserialize(input)?)),
            _ => Err(Felt252SerdeError::InvalidInputForDeserialization),
        }
    }
}

impl Felt252Serde for BranchTarget {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        match self {
            Self::Fallthrough => usize::MAX.serialize(output),
            Self::Statement(idx) => idx.serialize(output),
        }
    }

    fn deserialize<'a, I: ExactSizeIterator<Item = &'a BigUint>>(
        input: &mut I,
    ) -> Result<Self, Felt252SerdeError> {
        let idx = usize::deserialize(input)?;
        Ok(if idx == usize::MAX { Self::Fallthrough } else { Self::Statement(StatementIdx(idx)) })
    }
}

/// Helper for allocating a vector with capacity given an upper bound on the remaining possibly read
/// data.
fn vec_with_bounded_capacity<T>(
    size: usize,
    max_remaining_size: usize,
) -> Result<Vec<T>, Felt252SerdeError> {
    if max_remaining_size < size {
        Err(Felt252SerdeError::InvalidInputForDeserialization)
    } else {
        Ok(Vec::with_capacity(size))
    }
}
