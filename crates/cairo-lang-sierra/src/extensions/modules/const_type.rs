use cairo_lang_utils::try_extract_matches;

use super::boxing::BoxType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::utils::extract_bounds;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_single_type,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

/// Type representing a const.
#[derive(Default)]
pub struct ConstType {}
impl NamedType for ConstType {
    type Concrete = ConstConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("const");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConstConcreteType {
    pub info: TypeInfo,
    pub inner_ty: ConcreteTypeId,
    // Should be either a single value, or an inner const type.
    pub inner_data: Vec<GenericArg>,
}

impl ConstConcreteType {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let mut args_iter = args.iter();
        let inner_ty = args_iter
            .next()
            .and_then(|arg| try_extract_matches!(arg, GenericArg::Type))
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        // Extract the rest of the arguments as the inner data.
        let inner_data = args_iter.cloned().collect::<Vec<_>>();
        validate_const_data(context, inner_ty, &inner_data)?;
        let storable = false;
        let duplicatable = false;
        let droppable = false;
        let zero_sized = false;
        let info = TypeInfo {
            long_id: ConcreteTypeLongId { generic_id: "const".into(), generic_args: args.to_vec() },
            storable,
            duplicatable,
            droppable,
            zero_sized,
        };
        Ok(ConstConcreteType { info, inner_ty: inner_ty.clone(), inner_data })
    }
}

/// Validates that the inner data is valid for the inner type.
fn validate_const_data(
    context: &dyn TypeSpecializationContext,
    inner_ty: &ConcreteTypeId,
    inner_data: &[GenericArg],
) -> Result<(), SpecializationError> {
    let inner_type_info = context.get_type_info(inner_ty.clone())?;
    // For now, only simple types, i.e. types with range, are supported.
    let type_range = extract_bounds(&inner_type_info)?;
    if let [GenericArg::Value(value)] = inner_data {
        if value < &type_range.lower || value >= &type_range.upper {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    } else {
        return Err(SpecializationError::WrongNumberOfGenericArgs);
    };
    Ok(())
}

impl ConcreteType for ConstConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

pub struct ConstAsBoxConcreteLibfunc {
    pub const_type: ConcreteTypeId,
    pub signature: LibfuncSignature,
}

impl SignatureBasedConcreteLibfunc for ConstAsBoxConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

define_libfunc_hierarchy! {
    pub enum ConstLibfunc {
        AsBox(ConstAsBoxLibfuncWrapped),
    }, ConstConcreteLibfunc
}
#[derive(Default)]
pub struct ConstAsBoxLibfuncWrapped {}
impl ConstAsBoxLibfuncWrapped {
    // A zero-input function that returns a box of the const value according to the generic arg type
    // of the function.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<ConstAsBoxConcreteLibfunc, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let type_info = context.get_type_info(ty.clone())?;
        if type_info.long_id.generic_id != ConstType::ID {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let generic_args = context.get_type_info(ty.clone())?.long_id.generic_args;
        let [GenericArg::Type(inner_ty), ..] = generic_args.as_slice() else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };
        let boxed_inner_ty = context.get_wrapped_concrete_type(BoxType::id(), inner_ty.clone())?;
        Ok(ConstAsBoxConcreteLibfunc {
            signature: LibfuncSignature::new_non_branch(
                vec![],
                vec![OutputVarInfo {
                    ty: boxed_inner_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                }],
                SierraApChange::Known { new_vars_only: true },
            ),
            const_type: ty,
        })
    }
}

impl NamedLibfunc for ConstAsBoxLibfuncWrapped {
    type Concrete = ConstAsBoxConcreteLibfunc;
    const STR_ID: &'static str = "const_as_box";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(self.specialize_concrete_lib_func(context, args)?.signature)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        self.specialize_concrete_lib_func(context.upcast(), args)
    }
}
