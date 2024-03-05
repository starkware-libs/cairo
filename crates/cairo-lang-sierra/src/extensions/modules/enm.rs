//! Sierra example:
//! ```ignore
//! type felt252_ty = felt252;
//! type unit_ty = Tuple;
//! type Option = Enum<felt252_ty, unit_ty>;
//! libfunc init_option_some = enum_init<Option, 0>;
//! libfunc init_option_none = enum_init<Option, 1>;
//! libfunc match_option = enum_match<Option>;
//! ...
//! felt252_const<0>() -> (felt0);
//! tuple_const() -> (unit);
//! init_option_some(felt0) -> (some_id);
//! init_option_none(unit) -> (none_id);
//! match_option(some_id) {1000(some), 2000(none)};
//! match_option(none_id) {1000(some), 2000(none)};
//! ```

use cairo_lang_utils::try_extract_matches;
use num_bigint::ToBigInt;
use num_traits::Signed;

use super::bounded_int::BoundedIntType;
use super::snapshot::snapshot_ty;
use super::structure::StructType;
use super::utils::{reinterpret_cast_signature, Range};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
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
                zero_sized: false,
            },
            variants,
        })
    }

    /// Returns the EnumConcreteType of the given type, or a specialization error if not possible.
    fn try_from_concrete_type(
        context: &dyn SignatureSpecializationContext,
        ty: &ConcreteTypeId,
    ) -> Result<Self, SpecializationError> {
        let long_id = context.get_type_info(ty.clone())?.long_id;
        if long_id.generic_id != EnumType::ID {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        Self::new(context.as_type_specialization_context(), &long_id.generic_args)
    }
}

impl ConcreteType for EnumConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum EnumLibfunc {
        Init(EnumInitLibfunc),
        FromBoundedInt(EnumFromBoundedIntLibfunc),
        Match(EnumMatchLibfunc),
        SnapshotMatch(EnumSnapshotMatchLibfunc),
    }, EnumConcreteLibfunc
}

pub struct EnumInitConcreteLibfunc {
    pub signature: LibfuncSignature,
    /// The number of variants of the enum.
    pub n_variants: usize,
    /// The index of the relevant variant from the enum.
    pub index: usize,
}
impl SignatureBasedConcreteLibfunc for EnumInitConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for setting a value to an enum.
#[derive(Default)]
pub struct EnumInitLibfunc {}
impl EnumInitLibfunc {
    /// Creates the specialization of the enum-init libfunc with the given template arguments.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<EnumInitConcreteLibfunc, SpecializationError> {
        let (enum_type, index) = match args {
            [GenericArg::Type(enum_type), GenericArg::Value(index)] => {
                (enum_type.clone(), index.clone())
            }
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };
        let variant_types = EnumConcreteType::try_from_concrete_type(context, &enum_type)?.variants;
        let n_variants = variant_types.len();
        if index.is_negative() || index >= n_variants.to_bigint().unwrap() {
            return Err(SpecializationError::IndexOutOfRange { index, range_size: n_variants });
        }
        let index: usize = index.try_into().unwrap();
        let variant_type = variant_types[index].clone();
        Ok(EnumInitConcreteLibfunc {
            signature: LibfuncSignature::new_non_branch_ex(
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
            n_variants,
            index,
        })
    }
}
impl NamedLibfunc for EnumInitLibfunc {
    type Concrete = EnumInitConcreteLibfunc;
    const STR_ID: &'static str = "enum_init";

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

pub struct EnumFromBoundedIntConcreteLibfunc {
    pub signature: LibfuncSignature,
    /// The number of variants of the enum.
    pub n_variants: usize,
}
impl SignatureBasedConcreteLibfunc for EnumFromBoundedIntConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for creating an enum from a `BoundedInt` type.
/// Will only work where there are the same number of empty variants as in the range of the
/// `BoundedInt` type, and the range starts from 0.
#[derive(Default)]
pub struct EnumFromBoundedIntLibfunc {}
impl EnumFromBoundedIntLibfunc {
    /// Creates the specialization of the enum-from-bounded-int libfunc with the given template
    /// arguments.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<EnumFromBoundedIntConcreteLibfunc, SpecializationError> {
        let enum_type = args_as_single_type(args)?;
        let variant_types = EnumConcreteType::try_from_concrete_type(context, &enum_type)?.variants;
        let n_variants = variant_types.len();
        if n_variants == 0 {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        for v in variant_types {
            let long_id = context.get_type_info(v)?.long_id;
            // Only trivial empty structs are allowed as variant types.
            if !(long_id.generic_id == StructType::ID && long_id.generic_args.len() == 1) {
                return Err(SpecializationError::UnsupportedGenericArg);
            }
        }
        let input_ty = bounded_int_ty(context, Range::half_open(0, n_variants))?;
        if n_variants <= 2 {
            Ok(EnumFromBoundedIntConcreteLibfunc {
                signature: reinterpret_cast_signature(input_ty, enum_type),
                n_variants,
            })
        } else {
            Ok(EnumFromBoundedIntConcreteLibfunc {
                signature: LibfuncSignature::new_non_branch_ex(
                    vec![ParamSignature::new(input_ty)],
                    vec![OutputVarInfo {
                        ty: enum_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    }],
                    SierraApChange::Known { new_vars_only: false },
                ),
                n_variants,
            })
        }
    }
}
impl NamedLibfunc for EnumFromBoundedIntLibfunc {
    type Concrete = EnumFromBoundedIntConcreteLibfunc;
    const STR_ID: &'static str = "enum_from_bounded_int";

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

/// Libfunc for matching an enum.
#[derive(Default)]
pub struct EnumMatchLibfunc {}
impl SignatureOnlyGenericLibfunc for EnumMatchLibfunc {
    const STR_ID: &'static str = "enum_match";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let enum_type = args_as_single_type(args)?;
        let variant_types = EnumConcreteType::try_from_concrete_type(context, &enum_type)?.variants;
        let is_empty = variant_types.is_empty();
        let branch_signatures = variant_types
            .into_iter()
            .map(|ty| {
                Ok(BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: ty.clone(),
                        ref_info: if context.get_type_info(ty)?.zero_sized {
                            OutputVarReferenceInfo::ZeroSized
                        } else {
                            OutputVarReferenceInfo::PartialParam { param_idx: 0 }
                        },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(LibfuncSignature {
            param_signatures: vec![enum_type.into()],
            branch_signatures,
            fallthrough: if is_empty { None } else { Some(0) },
        })
    }
}

/// Libfunc for matching an enum snapshot.
#[derive(Default)]
pub struct EnumSnapshotMatchLibfunc {}
impl SignatureOnlyGenericLibfunc for EnumSnapshotMatchLibfunc {
    const STR_ID: &'static str = "enum_snapshot_match";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let enum_type = args_as_single_type(args)?;
        let variant_types = EnumConcreteType::try_from_concrete_type(context, &enum_type)?.variants;
        let branch_signatures = variant_types
            .into_iter()
            .map(|ty| {
                Ok(BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: snapshot_ty(context, ty.clone())?,
                        ref_info: if context.get_type_info(ty)?.zero_sized {
                            OutputVarReferenceInfo::ZeroSized
                        } else {
                            // All memory of the deconstruction would have the same lifetime as the
                            // first param - as it is its deconstruction.
                            OutputVarReferenceInfo::PartialParam { param_idx: 0 }
                        },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(LibfuncSignature {
            param_signatures: vec![snapshot_ty(context, enum_type)?.into()],
            branch_signatures,
            fallthrough: Some(0),
        })
    }
}

/// Creates a `BoundedInt` type with the given range.
fn bounded_int_ty(
    context: &dyn SignatureSpecializationContext,
    range: Range,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_concrete_type(
        BoundedIntType::id(),
        &[GenericArg::Value(range.lower), GenericArg::Value(range.upper - 1)],
    )
}
