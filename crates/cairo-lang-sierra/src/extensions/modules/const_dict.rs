use super::felt252::Felt252Type;
use super::range_check::RangeCheckType;
use super::snapshot::snapshot_ty;
use super::starknet::getter::boxed_ty;
use super::uint::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::uint128::Uint128Type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing an const_dict.
#[derive(Default)]
pub struct ConstDictType {}
impl NamedType for ConstDictType {
    const ID: GenericTypeId = GenericTypeId::new_inline("ConstDict");
    type Concrete = ConstDictConcreteType;

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let long_id = Self::concrete_type_long_id(args);
        let (k_ty, v_ty) = extract_k_and_v(args)?;
        let k_info = context.get_type_info(k_ty.clone())?;
        let v_info = context.get_type_info(v_ty.clone())?;

        // List of specific types allowed as dictionary values.
        // TODO(Gil): Check in the higher level compiler and raise proper diagnostic (when we'll
        // have a 'where' equivalent).
        // TODO(Gil): Allow any type of size 1 which implement the 'Default' trait.
        let allowed_types = [
            Felt252Type::id(),
            Uint8Type::id(),
            Uint16Type::id(),
            Uint32Type::id(),
            Uint64Type::id(),
            Uint128Type::id(),
        ];
        if allowed_types.contains(&k_info.long_id.generic_id)
            && k_info.storable
            && k_info.droppable
            && k_info.duplicatable
            && v_info.storable
        {
            Ok(ConstDictConcreteType {
                info: TypeInfo {
                    long_id,
                    duplicatable: false,
                    droppable: v_info.droppable,
                    storable: true,
                    size: 2,
                },
                k_ty,
                v_ty,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}

pub struct ConstDictConcreteType {
    pub info: TypeInfo,
    pub k_ty: ConcreteTypeId,
    pub v_ty: ConcreteTypeId,
}
impl ConcreteType for ConstDictConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum ConstDictLibfunc {
        New(ConstDictNewLibfunc),
        Insert(ConstDictInsertLibfunc),
    }, ConstDictConcreteLibfunc
}

/// Libfunc for creating a new const_dict.
#[derive(Default)]
pub struct ConstDictNewLibfunc {}
impl SignatureOnlyGenericLibfunc for ConstDictNewLibfunc {
    const STR_ID: &'static str = "const_dict_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (_k_ty, _v_ty) = extract_k_and_v(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(ConstDictType::id(), args)?,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: None },
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for pushing a value into the end of an const_dict.
#[derive(Default)]
pub struct ConstDictInsertLibfunc {}
impl NamedLibfunc for ConstDictInsertLibfunc {
    const STR_ID: &'static str = "const_dict_insert";

    type Concrete = ConstDictInsertConcreteLibfunc;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (k_ty, v_ty) = extract_k_and_v(args)?;
        let const_dict_ty = context.get_concrete_type(ConstDictType::id(), args)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()),
            ParamSignature::new(const_dict_ty.clone()),
            ParamSignature::new(k_ty),
            ParamSignature::new(v_ty.clone()),
        ];
        let branch_signatures = vec![
            // Some.
            BranchSignature {
                vars: vec![
                    OutputVarInfo {
                        ty: range_check_type.clone(),
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty: const_dict_ty.clone(),
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 1,
                        }),
                    },
                    OutputVarInfo {
                        ty: boxed_ty(context, snapshot_ty(context, v_ty)?)?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
            // None.
            BranchSignature {
                vars: vec![
                    OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty: const_dict_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
        ];
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }

    fn specialize(
        &self,
        context: &dyn crate::extensions::lib_func::SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(ConstDictInsertConcreteLibfunc {
            signature: <Self as NamedLibfunc>::specialize_signature(self, context.upcast(), args)?,
        })
    }
}
pub struct ConstDictInsertConcreteLibfunc {
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for ConstDictInsertConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

// Helpers.
fn extract_k_and_v(
    args: &[GenericArg],
) -> Result<(ConcreteTypeId, ConcreteTypeId), SpecializationError> {
    match args {
        [GenericArg::Type(k_ty), GenericArg::Type(v_ty)] => Ok((k_ty.clone(), v_ty.clone())),
        [_] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}
