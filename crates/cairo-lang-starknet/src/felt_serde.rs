use cairo_lang_sierra::extensions::starknet::storage::StorageAddressFromBaseAndOffsetLibfunc;
use cairo_lang_sierra::extensions::NamedLibfunc;
use cairo_lang_sierra::ids::{
    ConcreteLibfuncId, ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId, UserTypeId,
    VarId,
};
use cairo_lang_sierra::program::{
    BranchInfo, BranchTarget, ConcreteLibfuncLongId, ConcreteTypeLongId, Function,
    FunctionSignature, GenericArg, Invocation, LibfuncDeclaration, Param, Program, Statement,
    StatementIdx, TypeDeclaration,
};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use lazy_static::lazy_static;
use num_bigint::{BigInt, BigUint, ToBigInt};
use num_traits::ToPrimitive;
use thiserror::Error;

use crate::casm_contract_class::BigIntAsHex;
use crate::contract::starknet_keccak;
use crate::sierra_version::VersionId;

#[cfg(test)]
#[path = "felt_serde_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum FeltSerdeError {
    #[error("Illegal bigint value during serialization.")]
    BigIntOutOfBounds,
    #[error("Invalid input for deserialization.")]
    InvalidInputForDeserialization,
    #[error("Invalid generic id for serialization.")]
    InvalidGenericIdForSerialization,
    #[error("Invalid order of type declarations for serialization.")]
    OutOfOrderTypeDeclarationsForSerialization,
    #[error("Invalid order of libfunc declarations for serialization.")]
    OutOfOrderLibfuncDeclarationsForSerialization,
    #[error("Invalid order of user functions declarations for serialization.")]
    OutOfOrderUserFunctionDeclarationsForSerialization,
    #[error("Invalid function declaration for serialization.")]
    FunctionArgumentsMismatchInSerialization,
    #[error("The sierra version is too long and can not fit within a felt.")]
    VersionIdTooLongForSerialization,
}

/// Serializes a Sierra program into a vector of felts.
pub fn sierra_to_felts(
    sierra_version: VersionId,
    program: &Program,
) -> Result<Vec<BigIntAsHex>, FeltSerdeError> {
    let mut serialized = vec![];
    sierra_version.serialize(&mut serialized)?;
    program.serialize(&mut serialized)?;
    Ok(serialized)
}

/// Deserializes a Sierra program from a slice of felts.
pub fn sierra_from_felts(felts: &[BigIntAsHex]) -> Result<(VersionId, Program), FeltSerdeError> {
    let (version_id, program_part) = VersionId::deserialize(felts)?;
    Ok((version_id, Program::deserialize(program_part)?.0))
}

/// Trait for serializing and deserializing into a felt vector.
trait FeltSerde: Sized {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError>;
    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError>;
}

// Impls for basic types.

impl FeltSerde for usize {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        output.push(BigIntAsHex { value: (*self).into() });
        Ok(())
    }

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let head = input
            .first()
            .and_then(|size| size.value.to_usize())
            .ok_or(FeltSerdeError::InvalidInputForDeserialization)?;
        Ok((head, &input[1..]))
    }
}

impl FeltSerde for u64 {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        output.push(BigIntAsHex { value: (*self).into() });
        Ok(())
    }

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let head = input
            .first()
            .and_then(|size| size.value.to_u64())
            .ok_or(FeltSerdeError::InvalidInputForDeserialization)?;
        Ok((head, &input[1..]))
    }
}

impl<T: FeltSerde> FeltSerde for Vec<T> {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        self.len().serialize(output)?;
        for e in self {
            e.serialize(output)?;
        }
        Ok(())
    }

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let (size, mut input) = usize::deserialize(input)?;
        let mut result = Vec::with_capacity(size);
        for _ in 0..size {
            let (value, next) = T::deserialize(input)?;
            result.push(value);
            input = next;
        }
        Ok((result, input))
    }
}

impl FeltSerde for BigInt {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        output.push(BigIntAsHex {
            value: self.to_biguint().ok_or(FeltSerdeError::BigIntOutOfBounds)?,
        });
        Ok(())
    }
    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let first = input.first().ok_or(FeltSerdeError::InvalidInputForDeserialization)?;
        Ok((
            first.value.to_bigint().expect("Unsigned should always be convertable to signed."),
            &input[1..],
        ))
    }
}

impl FeltSerde for StatementIdx {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        self.0.serialize(output)
    }
    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let (value, input) = usize::deserialize(input)?;
        Ok((Self(value), input))
    }
}

// Impls for generic ids.
const SHORT_STRING_BOUND: usize = 31;
lazy_static! {
    /// A set of all the supported long generic ids.
    static ref LONG_IDS: OrderedHashSet<&'static str> = {
        OrderedHashSet::from_iter([StorageAddressFromBaseAndOffsetLibfunc::STR_ID].into_iter())
    };
    /// A mapping of all the long names when fixing them from the hashed keccak representation.
    static ref LONG_NAME_FIX: UnorderedHashMap<BigUint, &'static str> = {
        UnorderedHashMap::from_iter(LONG_IDS.iter().map(|name|{
            (starknet_keccak(name.as_bytes()), *name)
        }))
    };
}
macro_rules! generic_id_serde {
    ($Obj:ident) => {
        impl FeltSerde for $Obj {
            fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
                output.push(BigIntAsHex {
                    value: if self.0.len() <= SHORT_STRING_BOUND {
                        BigUint::from_bytes_be(self.0.as_bytes())
                    } else {
                        if !LONG_IDS.contains(self.0.as_str()) {
                            return Err(FeltSerdeError::InvalidGenericIdForSerialization);
                        }
                        starknet_keccak(self.0.as_bytes())
                    },
                });
                Ok(())
            }
            fn deserialize(
                input: &[BigIntAsHex],
            ) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
                let head = input
                    .first()
                    .and_then(|id| {
                        LONG_NAME_FIX.get(&id.value).map(|s| Self(s.into())).or_else(|| {
                            std::str::from_utf8(&id.value.to_bytes_be())
                                .ok()
                                .map(|s| Self(s.into()))
                        })
                    })
                    .ok_or(FeltSerdeError::InvalidInputForDeserialization)?;
                Ok((head, &input[1..]))
            }
        }
    };
}

generic_id_serde!(GenericTypeId);
generic_id_serde!(GenericLibfuncId);

impl FeltSerde for UserTypeId {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        output.push(BigIntAsHex { value: self.id.clone() });
        Ok(())
    }
    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let first = input.first().ok_or(FeltSerdeError::InvalidInputForDeserialization)?;
        Ok((Self { id: first.value.clone(), debug_name: None }, &input[1..]))
    }
}

// Impls for other ids.

macro_rules! id_serde {
    ($Obj:ident) => {
        impl FeltSerde for $Obj {
            fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
                self.id.serialize(output)
            }
            fn deserialize(
                input: &[BigIntAsHex],
            ) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
                let (id, input) = u64::deserialize(input)?;
                Ok((Self::new(id), input))
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
            $(FeltSerde::serialize(&__obj. $field_name, __output)?;)*
            Ok(())
        }
    };
}

macro_rules! struct_deserialize_impl {
    ($input:ident, { $($field_name:ident : $field_type:ty),* }) => {
        let __input = $input;
        $(
            let ($field_name, __input) = <$field_type>::deserialize(__input)?;
        )*
        $input = __input;
    };
}

macro_rules! struct_serialize {
    ($($field_name:ident),*) => {
        fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
            struct_serialize_impl!(self, output, { $($field_name),* })
        }
    };
}

macro_rules! struct_deserialize {
    ($Obj:ident { $($field_name:ident : $field_type:ty),* }) => {
        fn deserialize(
            mut input: &[BigIntAsHex],
        ) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
            struct_deserialize_impl!(input, {$($field_name : $field_type),*});
            Ok((Self {$($field_name),*}, input))
        }
    };
}

macro_rules! struct_serde {
    ($Obj:ident { $($field_name:ident : $field_type:ty),* $(,)? }) => {
        impl FeltSerde for $Obj {
            struct_serialize! { $($field_name),* }
            struct_deserialize! { $Obj { $($field_name : $field_type),* } }
        }
    }
}

impl FeltSerde for Program {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        // Type declarations.
        self.type_declarations.len().serialize(output)?;
        for (i, e) in self.type_declarations.iter().enumerate() {
            if i as u64 != e.id.id {
                return Err(FeltSerdeError::OutOfOrderTypeDeclarationsForSerialization);
            }
            e.long_id.serialize(output)?;
        }
        // Libfunc declaration.
        self.libfunc_declarations.len().serialize(output)?;
        for (i, e) in self.libfunc_declarations.iter().enumerate() {
            if i as u64 != e.id.id {
                return Err(FeltSerdeError::OutOfOrderLibfuncDeclarationsForSerialization);
            }
            e.long_id.serialize(output)?;
        }
        // Statements.
        FeltSerde::serialize(&self.statements, output)?;
        // Function declaration.
        self.funcs.len().serialize(output)?;
        for (i, f) in self.funcs.iter().enumerate() {
            if i as u64 != f.id.id {
                return Err(FeltSerdeError::OutOfOrderUserFunctionDeclarationsForSerialization);
            }
            f.signature.serialize(output)?;
            if f.signature.param_types.len() != f.params.len() {
                return Err(FeltSerdeError::FunctionArgumentsMismatchInSerialization);
            }
            for (param, ty) in f.params.iter().zip(f.signature.param_types.iter()) {
                if param.ty != *ty {
                    return Err(FeltSerdeError::FunctionArgumentsMismatchInSerialization);
                }
                param.id.serialize(output)?;
            }
            f.entry_point.serialize(output)?;
        }
        Ok(())
    }

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        // Type declarations.
        let (size, mut input) = usize::deserialize(input)?;
        let mut type_declarations = Vec::with_capacity(size);
        for i in 0..size {
            let (long_id, next) = ConcreteTypeLongId::deserialize(input)?;
            type_declarations.push(TypeDeclaration { id: ConcreteTypeId::new(i as u64), long_id });
            input = next;
        }
        // Libfunc declaration.
        let (size, mut input) = usize::deserialize(input)?;
        let mut libfunc_declarations = Vec::with_capacity(size);
        for i in 0..size {
            let (long_id, next) = ConcreteLibfuncLongId::deserialize(input)?;
            libfunc_declarations
                .push(LibfuncDeclaration { id: ConcreteLibfuncId::new(i as u64), long_id });
            input = next;
        }
        // Statements.
        let (statements, input) = FeltSerde::deserialize(input)?;
        // Function declaration.
        let (size, mut input) = usize::deserialize(input)?;
        let mut funcs = Vec::with_capacity(size);
        for i in 0..size {
            let (signature, next) = FunctionSignature::deserialize(input)?;
            input = next;
            let params = signature
                .param_types
                .iter()
                .cloned()
                .map(|ty| -> Result<Param, FeltSerdeError> {
                    let (id, next) = VarId::deserialize(input)?;
                    input = next;
                    Ok(Param { id, ty })
                })
                .collect::<Result<Vec<_>, _>>()?;
            let (entry_point, next) = StatementIdx::deserialize(input)?;
            funcs.push(Function { id: FunctionId::new(i as u64), signature, params, entry_point });
            input = next;
        }
        Ok((Self { type_declarations, libfunc_declarations, statements, funcs }, input))
    }
}

struct_serde! {
    ConcreteTypeLongId {
        generic_id: GenericTypeId,
        generic_args: Vec<GenericArg>,
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

// Impls for enums.

macro_rules! enum_serialize_impl {
    ($obj:ident, $output:ident, $Obj:ident { $($variant_name:ident = $variant_id:literal),* }) => {
        {
            let __output = $output;
            match $obj {
                $(
                    $Obj::$variant_name(value) => {
                        u64::serialize(&$variant_id, __output)?;
                        FeltSerde::serialize(value, __output)
                    }
                ),*
            }
        }
    };
}

macro_rules! enum_serialize {
    ($($variant_name:ident = $variant_id:literal),*) => {
        fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
            enum_serialize_impl!(self, output, Self { $($variant_name = $variant_id),* })
        }
    };
}

macro_rules! enum_deserialize {
    ($($variant_name:ident ( $variant_type:ty ) = $variant_id:literal),*) => {
        fn deserialize(
            input: &[BigIntAsHex],
        ) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
            let (id, input) = u64::deserialize(input)?;
            match id {
                $($variant_id => {
                    let (value, input) = <$variant_type>::deserialize(input)?;
                    Ok((Self::$variant_name(value), input))
                },)*
                _ => Err(FeltSerdeError::InvalidInputForDeserialization),
            }
        }
    };
}

macro_rules! enum_serde {
    ($Obj:ident { $($variant_name:ident ( $variant_type:ty ) = $variant_id:literal),* $(,)? }) => {
        impl FeltSerde for $Obj {
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

enum_serde! {
    GenericArg {
        UserType(UserTypeId) = 0,
        Type(ConcreteTypeId) = 1,
        Value(BigInt) = 2,
        UserFunc(FunctionId) = 3,
        Libfunc(ConcreteLibfuncId) = 4,
    }
}

impl FeltSerde for BranchTarget {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        match self {
            Self::Fallthrough => usize::MAX.serialize(output),
            Self::Statement(idx) => idx.serialize(output),
        }
    }

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let (idx, input) = usize::deserialize(input)?;
        Ok((
            if idx == usize::MAX { Self::Fallthrough } else { Self::Statement(StatementIdx(idx)) },
            input,
        ))
    }
}

impl FeltSerde for VersionId {
    fn serialize(&self, output: &mut Vec<BigIntAsHex>) -> Result<(), FeltSerdeError> {
        if self.version.len() < SHORT_STRING_BOUND {
            output.push(BigIntAsHex { value: BigUint::from_bytes_be(self.version.as_bytes()) });
            Ok(())
        } else {
            Err(FeltSerdeError::VersionIdTooLongForSerialization)
        }
    }
    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let head = input
            .first()
            .and_then(|id| {
                std::str::from_utf8(&id.value.to_bytes_be())
                    .ok()
                    .map(|s| Self { version: s.into() })
            })
            .ok_or(FeltSerdeError::InvalidInputForDeserialization)?;
        Ok((head, &input[1..]))
    }
}
