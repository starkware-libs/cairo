//! Sierra example:
//! ```ignore
//! type felt252 = felt252;
//! type Tuple<felt252, felt252> = Struct<ut@Tuple, felt252, felt252>;
//! libfunc tuple_construct = struct_construct<Tuple<felt252, felt252>>;
//! libfunc tuple_deconstruct = struct_deconstruct<Tuple<felt252, felt252>>;
//! ...
//! felt252_const<0>() -> (felt0);
//! felt252_const<1>() -> (felt1);
//! tuple_construct(felt0, felt1) -> (tup);
//! tuple_deconstruct(tup) -> (felt0, felt1);
//! ```

use cairo_lang_utils::try_extract_matches;

use super::snapshot::snapshot_ty;
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
        let mut zero_sized = true;
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
            zero_sized = zero_sized && info.zero_sized;
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
                zero_sized,
            },
            members,
        })
    }

    /// Returns the StructConcreteType of the given long id, or a specialization error if not
    /// possible.
    fn try_from_long_id(
        context: &dyn SignatureSpecializationContext,
        long_id: &ConcreteTypeLongId,
    ) -> Result<Self, SpecializationError> {
        if long_id.generic_id != StructType::ID {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        Self::new(context.as_type_specialization_context(), &long_id.generic_args)
    }

    /// Returns the StructConcreteType of the given type, or a specialization error if not possible.
    pub fn try_from_concrete_type(
        context: &dyn SignatureSpecializationContext,
        ty: &ConcreteTypeId,
    ) -> Result<Self, SpecializationError> {
        let long_id = context.get_type_info(ty.clone())?.long_id;
        Self::try_from_long_id(context, &long_id)
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
        SnapshotDeconstruct(StructSnapshotDeconstructLibfunc),
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
        let type_info = context.get_type_info(struct_type.clone())?;
        let member_types =
            StructConcreteType::try_from_long_id(context, &type_info.long_id)?.members;

        let mut opt_same_as_param_idx = None;
        for (idx, ty) in member_types.iter().cloned().enumerate() {
            if !context.get_type_info(ty)?.zero_sized {
                if opt_same_as_param_idx.is_some() {
                    // There are multiple non-zero sized items, can't use the same param.
                    opt_same_as_param_idx = None;
                    break;
                }
                opt_same_as_param_idx = Some(idx);
            }
        }

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
                ref_info: if type_info.zero_sized {
                    OutputVarReferenceInfo::ZeroSized
                } else if let Some(param_idx) = opt_same_as_param_idx {
                    OutputVarReferenceInfo::SameAsParam { param_idx }
                } else {
                    OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic)
                },
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
        let member_types =
            StructConcreteType::try_from_concrete_type(context, &struct_type)?.members;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: struct_type,
                allow_deferred: true,
                allow_add_const: false,
                allow_const: true,
            }],
            member_types
                .into_iter()
                .map(|ty| {
                    Ok(OutputVarInfo {
                        ty: ty.clone(),
                        ref_info: if context.get_type_info(ty)?.zero_sized {
                            OutputVarReferenceInfo::ZeroSized
                        } else {
                            // All memory of the deconstruction would have the same lifetime as the
                            // first param - as it is its deconstruction.
                            OutputVarReferenceInfo::PartialParam { param_idx: 0 }
                        },
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for deconstructing a struct snapshot.
#[derive(Default)]
pub struct StructSnapshotDeconstructLibfunc {}
impl SignatureOnlyGenericLibfunc for StructSnapshotDeconstructLibfunc {
    const STR_ID: &'static str = "struct_snapshot_deconstruct";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let struct_type = args_as_single_type(args)?;
        let member_types =
            StructConcreteType::try_from_concrete_type(context, &struct_type)?.members;
        Ok(LibfuncSignature::new_non_branch(
            vec![snapshot_ty(context, struct_type)?],
            member_types
                .into_iter()
                .map(|ty| {
                    Ok(OutputVarInfo {
                        ty: snapshot_ty(context, ty.clone())?,
                        ref_info: if context.get_type_info(ty)?.zero_sized {
                            OutputVarReferenceInfo::ZeroSized
                        } else {
                            // All memory of the deconstruction would have the same lifetime as the
                            // first param - as it is its deconstruction.
                            OutputVarReferenceInfo::PartialParam { param_idx: 0 }
                        },
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
