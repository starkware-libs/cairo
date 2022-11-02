//! Sierra example:
//! ```ignore
//! type felt = felt;
//! type Tuple<felt, felt> = Struct<ut@Tuple, felt, felt>;
//! libfunc tuple_construct = struct_construct<Tuple<felt, felt>>;
//! libfunc tuple_deconstruct = struct_deconstruct<Tuple<felt, felt>>;
//! ...
//! felt_const<0>() -> (felt0);
//! felt_const<1>() -> (felt1);
//! tuple_construct(felt0, felt1) -> (tup);
//! tuple_deconstruct(tup) -> (felt0, felt1);
//! ```

use utils::try_extract_matches;

use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyConcreteLibFunc, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

/// Type representing a struct.
#[derive(Default)]
pub struct StructType {}
impl NamedType for StructType {
    type Concrete = StructConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Struct");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct StructConcreteType {
    pub info: TypeInfo,
    pub members: Vec<ConcreteTypeId>,
}
impl StructConcreteType {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let mut args_iter = args.iter();
        args_iter
            .next()
            .and_then(|arg| try_extract_matches!(arg, GenericArg::UserType))
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let mut duplicatable = true;
        let mut droppable = true;
        let mut members: Vec<ConcreteTypeId> = Vec::new();
        for arg in args_iter {
            let ty = try_extract_matches!(arg, GenericArg::Type)
                .ok_or(SpecializationError::UnsupportedGenericArg)?
                .clone();
            let info = context.get_type_info(ty.clone())?;
            if !info.storable {
                return Err(SpecializationError::UnsupportedGenericArg);
            }
            if !info.duplicatable {
                duplicatable = false;
            }
            if !info.droppable {
                droppable = false;
            }
            members.push(ty);
        }
        Ok(StructConcreteType {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "Struct".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable,
                droppable,
                storable: true,
            },
            members,
        })
    }
}
impl ConcreteType for StructConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum StructLibFunc {
        Construct(StructConstructLibFunc),
        Deconstruct(StructDeconstructLibFunc),
    }, StructConcreteLibFunc
}

/// LibFunc for constructing a struct.
#[derive(Default)]
pub struct StructConstructLibFunc {}
impl NamedLibFunc for StructConstructLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("struct_construct");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let struct_type = as_single_type(args)?;
        let generic_args = context.get_type_info(struct_type.clone())?.long_id.generic_args;
        let member_types =
            StructConcreteType::new(context.as_type_specialization_context(), &generic_args)?
                .members;
        Ok(LibFuncSignature::new_non_branch(
            member_types,
            vec![OutputVarInfo {
                ty: struct_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// LibFunc for deconstructing a struct.
#[derive(Default)]
pub struct StructDeconstructLibFunc {}
impl NamedLibFunc for StructDeconstructLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("struct_deconstruct");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let struct_type = as_single_type(args)?;
        let generic_args = context.get_type_info(struct_type.clone())?.long_id.generic_args;
        let member_types =
            StructConcreteType::new(context.as_type_specialization_context(), &generic_args)?
                .members;
        Ok(LibFuncSignature::new_non_branch(
            vec![struct_type],
            member_types
                .into_iter()
                .map(|ty| OutputVarInfo {
                    ty,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                })
                .collect(),
            SierraApChange::Known,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}
