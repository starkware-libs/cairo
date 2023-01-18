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

use cairo_lang_utils::try_extract_matches;

use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
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
        let mut size = 0;
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
            size += info.size;
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
                size,
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
    pub enum StructLibfunc {
        Construct(StructConstructLibfunc),
        Deconstruct(StructDeconstructLibfunc),
    }, StructConcreteLibfunc
}

/// Libfunc for constructing a struct.
#[derive(Default)]
pub struct StructConstructLibfunc {}
impl SignatureOnlyGenericLibfunc for StructConstructLibfunc {
    const STR_ID: &'static str = "struct_construct";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let struct_type = args_as_single_type(args)?;
        let generic_args = context.get_type_info(struct_type.clone())?.long_id.generic_args;
        let member_types =
            StructConcreteType::new(context.as_type_specialization_context(), &generic_args)?
                .members;
        Ok(LibfuncSignature::new_non_branch_ex(
            member_types
                .into_iter()
                .map(|ty| ParamSignature {
                    ty,
                    allow_deferred: true,
                    allow_add_const: true,
                    allow_const: true,
                })
                .collect(),
            vec![OutputVarInfo {
                ty: struct_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for deconstructing a struct.
#[derive(Default)]
pub struct StructDeconstructLibfunc {}
impl SignatureOnlyGenericLibfunc for StructDeconstructLibfunc {
    const STR_ID: &'static str = "struct_deconstruct";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let struct_type = args_as_single_type(args)?;
        let generic_args = context.get_type_info(struct_type.clone())?.long_id.generic_args;
        let member_types =
            StructConcreteType::new(context.as_type_specialization_context(), &generic_args)?
                .members;
        Ok(LibfuncSignature::new_non_branch(
            vec![struct_type],
            member_types
                .into_iter()
                .map(|ty| OutputVarInfo {
                    ty,
                    // All memory of the deconstruction would have the same lifetime as the first
                    // param - as it is its deconstruction.
                    ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                })
                .collect(),
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
