use num_bigint::{BigInt, ToBigInt};
use num_traits::ToPrimitive;
use sierra::ids::{
    ConcreteLibFuncId, ConcreteTypeId, FunctionId, GenericLibFuncId, GenericTypeId, UserTypeId,
    VarId,
};
use sierra::program::{
    BranchInfo, BranchTarget, ConcreteLibFuncLongId, ConcreteTypeLongId, Function,
    FunctionSignature, GenericArg, Invocation, LibFuncDeclaration, Param, Program, Statement,
    StatementIdx, TypeDeclaration,
};
use thiserror::Error;

use crate::casm_contract_class::BigIntAsHex;

#[cfg(test)]
#[path = "felt_serde_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum FeltSerdeError {
    #[error("Illegal bigint value during serialization.")]
    BigIntOutOfBounds,
    #[error("Invalid input for deserialization.")]
    InvalidInputForDeserialization,
}

/// Serializes a Sierra program into a vector of felts.
pub fn sierra_to_felts(program: &Program) -> Result<Vec<BigIntAsHex>, FeltSerdeError> {
    let mut serialized = vec![];
    program.serialize(&mut serialized)?;
    Ok(serialized)
}

/// Deserializes a Sierra program from a slice of felts.
pub fn sierra_from_felts(felts: &[BigIntAsHex]) -> Result<Program, FeltSerdeError> {
    Ok(Program::deserialize(felts)?.0)
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

// Impls for ids.

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

id_serde!(GenericTypeId);
id_serde!(ConcreteTypeId);
id_serde!(GenericLibFuncId);
id_serde!(ConcreteLibFuncId);
id_serde!(VarId);
id_serde!(UserTypeId);
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

struct_serde! {
    Program {
        type_declarations: Vec<TypeDeclaration>,
        libfunc_declarations: Vec<LibFuncDeclaration>,
        statements: Vec<Statement>,
        funcs: Vec<Function>,
    }
}

struct_serde! {
    TypeDeclaration {
        id: ConcreteTypeId,
        long_id: ConcreteTypeLongId,
    }
}

struct_serde! {
    LibFuncDeclaration {
        id:  ConcreteLibFuncId,
        long_id:  ConcreteLibFuncLongId,
    }
}

struct_serde! {
    Function {
        id: FunctionId,
        signature: FunctionSignature,
        params: Vec<Param>,
        entry_point: StatementIdx,
    }
}

struct_serde! {
    ConcreteTypeLongId {
        generic_id: GenericTypeId,
        generic_args: Vec<GenericArg>,
    }
}

struct_serde! {
    ConcreteLibFuncLongId {
        generic_id: GenericLibFuncId,
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
    Param {
        id:  VarId,
        ty:  ConcreteTypeId,
    }
}

struct_serde! {
    Invocation {
        libfunc_id: ConcreteLibFuncId,
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

impl FeltSerde for Statement {
    enum_serialize! {Invocation = 0, Return = 1}

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let (id, input) = u64::deserialize(input)?;
        match id {
            0 => {
                let (invocation, input) = Invocation::deserialize(input)?;
                Ok((Self::Invocation(invocation), input))
            }
            1 => {
                let (vars, input) = Vec::<VarId>::deserialize(input)?;
                Ok((Self::Return(vars), input))
            }
            _ => Err(FeltSerdeError::InvalidInputForDeserialization),
        }
    }
}

impl FeltSerde for GenericArg {
    enum_serialize! {
        UserType = 0,
        Type = 1,
        Value = 2,
        UserFunc = 3,
        LibFunc = 4
    }

    fn deserialize(input: &[BigIntAsHex]) -> Result<(Self, &[BigIntAsHex]), FeltSerdeError> {
        let (id, input) = u64::deserialize(input)?;
        match id {
            0 => {
                let (id, input) = UserTypeId::deserialize(input)?;
                Ok((Self::UserType(id), input))
            }
            1 => {
                let (id, input) = ConcreteTypeId::deserialize(input)?;
                Ok((Self::Type(id), input))
            }
            2 => {
                let (value, input) = BigInt::deserialize(input)?;
                Ok((Self::Value(value), input))
            }
            3 => {
                let (id, input) = FunctionId::deserialize(input)?;
                Ok((Self::UserFunc(id), input))
            }
            4 => {
                let (id, input) = ConcreteLibFuncId::deserialize(input)?;
                Ok((Self::LibFunc(id), input))
            }
            _ => Err(FeltSerdeError::InvalidInputForDeserialization),
        }
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
