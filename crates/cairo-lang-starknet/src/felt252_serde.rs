use cairo_lang_sierra::extensions::starknet::interoperability::ContractAddressTryFromFelt252Libfunc;
use cairo_lang_sierra::extensions::starknet::storage::{
    StorageAddressFromBaseAndOffsetLibfunc, StorageBaseAddressFromFelt252Libfunc,
};
use cairo_lang_sierra::extensions::try_from_felt252::TryFromFelt252;
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
use cairo_lang_utils::bigint::BigUintAsHex;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use num_bigint::{BigInt, BigUint, ToBigInt};
use num_traits::ToPrimitive;
use once_cell::sync::Lazy;
use thiserror::Error;

use crate::contract::starknet_keccak;
use crate::sierra_version::VersionId;

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
        "Id `{0}` is too long for serialization. It is longer than {} characters. Consider adding \
         it in SERDE_SUPPORTED_LONG_IDS.",
        SHORT_STRING_BOUND
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
    #[error("The sierra version is too long and can not fit within a felt252.")]
    VersionIdTooLongForSerialization,
}

/// Serializes a Sierra program into a vector of felt252s.
pub fn sierra_to_felt252s(
    sierra_version: VersionId,
    program: &Program,
) -> Result<Vec<BigUintAsHex>, Felt252SerdeError> {
    let mut serialized = vec![];
    sierra_version.serialize(&mut serialized)?;
    program.serialize(&mut serialized)?;
    Ok(serialized)
}

/// Deserializes a Sierra program from a slice of felt252s.
pub fn sierra_from_felt252s(
    felts: &[BigUintAsHex],
) -> Result<(VersionId, Program), Felt252SerdeError> {
    let (version_id, program_part) = VersionId::deserialize(felts)?;
    Ok((version_id, Program::deserialize(program_part)?.0))
}

/// Trait for serializing and deserializing into a felt252 vector.
trait Felt252Serde: Sized {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError>;
    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError>;
}

// Impls for basic types.

impl Felt252Serde for usize {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex { value: (*self).into() });
        Ok(())
    }

    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let head = input
            .first()
            .and_then(|size| size.value.to_usize())
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        Ok((head, &input[1..]))
    }
}

impl Felt252Serde for u64 {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex { value: (*self).into() });
        Ok(())
    }

    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let head = input
            .first()
            .and_then(|size| size.value.to_u64())
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        Ok((head, &input[1..]))
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

    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
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

impl Felt252Serde for BigInt {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        output.push(BigUintAsHex {
            value: self.to_biguint().ok_or(Felt252SerdeError::BigIntOutOfBounds)?,
        });
        Ok(())
    }
    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let first = input.first().ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        Ok((
            first.value.to_bigint().expect("Unsigned should always be convertable to signed."),
            &input[1..],
        ))
    }
}

impl Felt252Serde for StatementIdx {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        self.0.serialize(output)
    }
    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let (value, input) = usize::deserialize(input)?;
        Ok((Self(value), input))
    }
}

// Impls for generic ids.
const SHORT_STRING_BOUND: usize = 31;
/// A set of all the supported long generic ids.
static SERDE_SUPPORTED_LONG_IDS: Lazy<OrderedHashSet<&'static str>> = Lazy::new(|| {
    OrderedHashSet::from_iter(
        [
            StorageAddressFromBaseAndOffsetLibfunc::STR_ID,
            ContractAddressTryFromFelt252Libfunc::STR_ID,
            StorageBaseAddressFromFelt252Libfunc::STR_ID,
        ]
        .into_iter(),
    )
});
/// A mapping of all the long names when fixing them from the hashed keccak representation.
static LONG_NAME_FIX: Lazy<UnorderedHashMap<BigUint, &'static str>> = Lazy::new(|| {
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
            fn deserialize(
                input: &[BigUintAsHex],
            ) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
                let head = input
                    .first()
                    .and_then(|id| {
                        LONG_NAME_FIX.get(&id.value).map(|s| Self(s.into())).or_else(|| {
                            std::str::from_utf8(&id.value.to_bytes_be())
                                .ok()
                                .map(|s| Self(s.into()))
                        })
                    })
                    .ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
                Ok((head, &input[1..]))
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
    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let first = input.first().ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        Ok((Self { id: first.value.clone(), debug_name: None }, &input[1..]))
    }
}

// Impls for other ids.

macro_rules! id_serde {
    ($Obj:ident) => {
        impl Felt252Serde for $Obj {
            fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
                self.id.serialize(output)
            }
            fn deserialize(
                input: &[BigUintAsHex],
            ) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
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
            $(Felt252Serde::serialize(&__obj. $field_name, __output)?;)*
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
        fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
            struct_serialize_impl!(self, output, { $($field_name),* })
        }
    };
}

macro_rules! struct_deserialize {
    ($Obj:ident { $($field_name:ident : $field_type:ty),* }) => {
        fn deserialize(
            mut input: &[BigUintAsHex],
        ) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
            struct_deserialize_impl!(input, {$($field_name : $field_type),*});
            Ok((Self {$($field_name),*}, input))
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
            if i as u64 != e.id.id {
                return Err(Felt252SerdeError::OutOfOrderTypeDeclarationsForSerialization);
            }
            e.long_id.serialize(output)?;
        }
        // Libfunc declaration.
        self.libfunc_declarations.len().serialize(output)?;
        for (i, e) in self.libfunc_declarations.iter().enumerate() {
            if i as u64 != e.id.id {
                return Err(Felt252SerdeError::OutOfOrderLibfuncDeclarationsForSerialization);
            }
            e.long_id.serialize(output)?;
        }
        // Statements.
        Felt252Serde::serialize(&self.statements, output)?;
        // Function declaration.
        self.funcs.len().serialize(output)?;
        for (i, f) in self.funcs.iter().enumerate() {
            if i as u64 != f.id.id {
                return Err(Felt252SerdeError::OutOfOrderUserFunctionDeclarationsForSerialization);
            }
            f.signature.serialize(output)?;
            if f.signature.param_types.len() != f.params.len() {
                return Err(Felt252SerdeError::FunctionArgumentsMismatchInSerialization);
            }
            for (param, ty) in f.params.iter().zip(f.signature.param_types.iter()) {
                if param.ty != *ty {
                    return Err(Felt252SerdeError::FunctionArgumentsMismatchInSerialization);
                }
                param.id.serialize(output)?;
            }
            f.entry_point.serialize(output)?;
        }
        Ok(())
    }

    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        // Type declarations.
        let (size, mut input) = usize::deserialize(input)?;
        let mut type_declarations = Vec::with_capacity(size);
        for i in 0..size {
            let (long_id, next) = ConcreteTypeLongId::deserialize(input)?;
            type_declarations.push(TypeDeclaration {
                id: ConcreteTypeId::new(i as u64),
                long_id,
                declared_type_info: None,
            });
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
        let (statements, input) = Felt252Serde::deserialize(input)?;
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
                .map(|ty| -> Result<Param, Felt252SerdeError> {
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
        fn deserialize(
            input: &[BigUintAsHex],
        ) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
            let (id, input) = u64::deserialize(input)?;
            match id {
                $($variant_id => {
                    let (value, input) = <$variant_type>::deserialize(input)?;
                    Ok((Self::$variant_name(value), input))
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

enum_serde! {
    GenericArg {
        UserType(UserTypeId) = 0,
        Type(ConcreteTypeId) = 1,
        Value(BigInt) = 2,
        UserFunc(FunctionId) = 3,
        Libfunc(ConcreteLibfuncId) = 4,
    }
}

impl Felt252Serde for BranchTarget {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        match self {
            Self::Fallthrough => usize::MAX.serialize(output),
            Self::Statement(idx) => idx.serialize(output),
        }
    }

    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let (idx, input) = usize::deserialize(input)?;
        Ok((
            if idx == usize::MAX { Self::Fallthrough } else { Self::Statement(StatementIdx(idx)) },
            input,
        ))
    }
}

impl Felt252Serde for VersionId {
    fn serialize(&self, output: &mut Vec<BigUintAsHex>) -> Result<(), Felt252SerdeError> {
        if self.version.len() < SHORT_STRING_BOUND {
            output.push(BigUintAsHex { value: BigUint::from_bytes_be(self.version.as_bytes()) });
            Ok(())
        } else {
            Err(Felt252SerdeError::VersionIdTooLongForSerialization)
        }
    }
    fn deserialize(input: &[BigUintAsHex]) -> Result<(Self, &[BigUintAsHex]), Felt252SerdeError> {
        let head = input
            .first()
            .and_then(|id| {
                std::str::from_utf8(&id.value.to_bytes_be())
                    .ok()
                    .map(|s| Self { version: s.into() })
            })
            .ok_or(Felt252SerdeError::InvalidInputForDeserialization)?;
        Ok((head, &input[1..]))
    }
}
