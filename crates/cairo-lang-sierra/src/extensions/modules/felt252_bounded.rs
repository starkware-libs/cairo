use num_bigint::BigInt;

use super::felt252::Felt252Type;
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Type for felt252_bounded.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct Felt252BoundedType {}
impl NamedType for Felt252BoundedType {
    type Concrete = Felt252BoundedConcreteType;

    const ID: GenericTypeId = GenericTypeId::new_inline("felt252_bounded");
    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (min, max) = match args {
            [GenericArg::Value(min), GenericArg::Value(max)] => (min.clone(), max.clone()),
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };
        if min != 0.into() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if min >= max {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if max > BigInt::from(u128::MAX) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let long_id = Self::concrete_type_long_id(args);
        let ty_info = TypeInfo {
            long_id,
            zero_sized: false,
            storable: true,
            droppable: true,
            duplicatable: true,
        };

        Ok(Self::Concrete { info: ty_info, min, max })
    }
}
pub struct Felt252BoundedConcreteType {
    pub info: TypeInfo,
    pub min: BigInt,
    pub max: BigInt,
}
impl ConcreteType for Felt252BoundedConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum Felt252BoundedLibfunc {
        FromFelt(Felt252BoundedFromFelt252Libfunc),
    }, Felt252BoundedConcreteLibfunc
}

pub struct Felt252BoundedFromFelt252ConcreteLibfunc {
    pub signature: LibfuncSignature,
    pub min: BigInt,
    pub max: BigInt,
}
impl SignatureBasedConcreteLibfunc for Felt252BoundedFromFelt252ConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
#[derive(Default)]
pub struct Felt252BoundedFromFelt252Libfunc {}
impl Felt252BoundedFromFelt252Libfunc {
    /// Creates the specialization of the Felt252Bounded-from-felt libfunc with the given template
    /// arguments.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Felt252BoundedFromFelt252ConcreteLibfunc, SpecializationError> {
        let (min, max) = match args {
            [GenericArg::Value(min), GenericArg::Value(max)] => (min.clone(), max.clone()),
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };
        if min != 0.into() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if min >= max {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if max > BigInt::from(u128::MAX) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let ty = context.get_concrete_type(Felt252BoundedType::id(), args)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        Ok(Felt252BoundedFromFelt252ConcreteLibfunc {
            signature: LibfuncSignature {
                param_signatures: vec![
                    ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                    // TODO(tomerstarkware): Consider changing input to u128.
                    ParamSignature::new(context.get_concrete_type(Felt252Type::id(), &[])?),
                ],
                branch_signatures: vec![
                    BranchSignature {
                        vars: vec![
                            OutputVarInfo {
                                ty: range_check_type.clone(),
                                ref_info: OutputVarReferenceInfo::Deferred(
                                    DeferredOutputKind::AddConst { param_idx: 0 },
                                ),
                            },
                            OutputVarInfo {
                                ty,
                                ref_info: OutputVarReferenceInfo::Deferred(
                                    DeferredOutputKind::Generic,
                                ),
                            },
                        ],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                    BranchSignature {
                        vars: vec![OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        }],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                ],
                fallthrough: Some(0),
            },
            min,
            max,
        })
    }
}
impl NamedLibfunc for Felt252BoundedFromFelt252Libfunc {
    type Concrete = Felt252BoundedFromFelt252ConcreteLibfunc;
    const STR_ID: &'static str = "felt252_bounded_from_felt252";

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
