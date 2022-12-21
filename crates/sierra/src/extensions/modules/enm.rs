//! Sierra example:
//! ```ignore
//! type felt_ty = felt;
//! type unit_ty = Tuple;
//! type Option = Enum<felt_ty, unit_ty>;
//! libfunc init_option_some = enum_init<Option, 0>;
//! libfunc init_option_none = enum_init<Option, 1>;
//! libfunc match_option = enum_match<Option>;
//! ...
//! felt_const<0>() -> (felt0);
//! tuple_const() -> (unit);
//! init_option_some(felt0) -> (some_id);
//! init_option_none(unit) -> (none_id);
//! match_option(some_id) {1000(some), 2000(none)};
//! match_option(none_id) {1000(some), 2000(none)};
//! ```

use std::cmp;

use num_bigint::ToBigInt;
use num_traits::Signed;
use utils::try_extract_matches;

use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedLibFunc, NamedType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

/// Type representing an enum.
#[derive(Default)]
pub struct EnumType {}
impl NamedType for EnumType {
    type Concrete = EnumConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Enum");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct EnumConcreteType {
    pub info: TypeInfo,
    pub variants: Vec<ConcreteTypeId>,
}
impl EnumConcreteType {
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
        let mut variants: Vec<ConcreteTypeId> = Vec::new();
        let mut variant_max_size = 0;
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
            variants.push(ty);
            variant_max_size = cmp::max(variant_max_size, info.size);
        }
        Ok(EnumConcreteType {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "Enum".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable,
                droppable,
                storable: true,
                size: 1 + variant_max_size,
            },
            variants,
        })
    }
}
impl ConcreteType for EnumConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum EnumLibFunc {
        Init(EnumInitLibFunc),
        Match(EnumMatchLibFunc),
    }, EnumConcreteLibFunc
}

pub struct EnumInitConcreteLibFunc {
    pub signature: LibFuncSignature,
    /// The number of variants of the enum.
    pub num_variants: usize,
    /// The index of the relevant variant from the enum.
    pub index: usize,
}
impl SignatureBasedConcreteLibFunc for EnumInitConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for setting a value to an enum.
#[derive(Default)]
pub struct EnumInitLibFunc {}
impl EnumInitLibFunc {
    /// Creates the specialization of the enum-init libfunc with the given template arguments.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<EnumInitConcreteLibFunc, SpecializationError> {
        let (enum_type, index) = match args {
            [GenericArg::Type(enum_type), GenericArg::Value(index)] => {
                (enum_type.clone(), index.clone())
            }
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };
        let generic_args = context.get_type_info(enum_type.clone())?.long_id.generic_args;
        let variant_types =
            EnumConcreteType::new(context.as_type_specialization_context(), &generic_args)?
                .variants;
        let num_variants = variant_types.len();
        if index.is_negative() || index >= num_variants.to_bigint().unwrap() {
            return Err(SpecializationError::IndexOutOfRange { index, range_size: num_variants });
        }
        let index: usize = index.try_into().unwrap();
        let variant_type = variant_types[index].clone();
        Ok(EnumInitConcreteLibFunc {
            signature: LibFuncSignature::new_non_branch_ex(
                vec![ParamSignature {
                    ty: variant_type,
                    allow_deferred: true,
                    allow_add_const: true,
                    allow_const: true,
                }],
                vec![OutputVarInfo {
                    ty: enum_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                }],
                SierraApChange::Known { new_vars_only: true },
            ),
            num_variants,
            index,
        })
    }
}
impl NamedLibFunc for EnumInitLibFunc {
    type Concrete = EnumInitConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("enum_init");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
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

/// LibFunc for matching an enum.
#[derive(Default)]
pub struct EnumMatchLibFunc {}
impl SignatureOnlyGenericLibFunc for EnumMatchLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("enum_match");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let enum_type = args_as_single_type(args)?;
        let generic_args = context.get_type_info(enum_type.clone())?.long_id.generic_args;
        let variant_types =
            EnumConcreteType::new(context.as_type_specialization_context(), &generic_args)?
                .variants;
        let branch_signatures = variant_types
            .into_iter()
            .map(|ty| BranchSignature {
                vars: vec![OutputVarInfo {
                    ty,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                }],
                ap_change: SierraApChange::Known { new_vars_only: true },
            })
            .collect();

        Ok(LibFuncSignature {
            param_signatures: vec![enum_type.into()],
            branch_signatures,
            fallthrough: None,
        })
    }
}
