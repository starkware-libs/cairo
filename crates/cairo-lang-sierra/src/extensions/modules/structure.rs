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

use super::boxing::box_ty;
use super::snapshot::{SnapshotType, snapshot_ty};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_single_type,
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
        let mut storable = true;
        let mut members: Vec<ConcreteTypeId> = Vec::new();
        let mut zero_sized = true;
        for arg in args_iter {
            let ty = try_extract_matches!(arg, GenericArg::Type)
                .ok_or(SpecializationError::UnsupportedGenericArg)?
                .clone();
            let info = context.get_type_info(ty.clone())?;
            if !info.storable {
                storable = false;
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
                storable,
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
        Self::new(context, &long_id.generic_args)
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
        BoxedDeconstruct(StructBoxedDeconstructLibfunc),
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

/// Concrete implementation of the boxed struct deconstruct libfunc.
pub struct ConcreteStructBoxedDeconstructLibfunc {
    /// The concrete types of the struct members (no additional snapshots and boxing) that will be
    /// extracted as boxed values.
    pub members: Vec<ConcreteTypeId>,
    signature: LibfuncSignature,
}

impl SignatureBasedConcreteLibfunc for ConcreteStructBoxedDeconstructLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for deconstructing a boxed struct into boxes of its members.
#[derive(Default)]
pub struct StructBoxedDeconstructLibfunc {}

impl StructBoxedDeconstructLibfunc {
    /// Analyzes a struct type to extract member types and snapshot information.
    ///
    /// This method handles both regular structs and snapshot-wrapped structs. For snapshot-wrapped
    /// structs (e.g., `@StructType`), it unwraps the snapshot to get the underlying struct type,
    /// then extracts the member types and indicates the snapshot status.
    ///
    /// # Returns
    /// - `Vec<ConcreteTypeId>`: The concrete types of each struct member
    /// - `bool`: Whether the input struct was wrapped in a snapshot
    fn analyze_struct_type(
        &self,
        context: &dyn SignatureSpecializationContext,
        mut ty: ConcreteTypeId,
    ) -> Result<(Vec<ConcreteTypeId>, bool), SpecializationError> {
        let arg_type_info = context.get_type_info(ty.clone())?;
        let is_snapshot = arg_type_info.long_id.generic_id == SnapshotType::id();
        if is_snapshot {
            ty = match &arg_type_info.long_id.generic_args[0] {
                GenericArg::Type(ty) => ty.clone(),
                _ => return Err(SpecializationError::UnsupportedGenericArg),
            }
        }
        let struct_type = StructConcreteType::try_from_concrete_type(context, &ty)?;
        Ok((struct_type.members, is_snapshot))
    }

    /// Creates the libfunc signature for boxed struct deconstruction.
    ///
    /// # Parameters
    /// - `ty`: The concrete type ID of the struct being deconstructed
    /// - `member_types`: The concrete types of each struct member
    /// - `is_snapshot`: Whether the struct was originally wrapped in a snapshot
    ///
    /// # Returns
    /// A libfunc signature that takes a boxed struct as input and returns boxed versions
    /// of each member. If `is_snapshot` is true, the members are also wrapped in snapshots.
    fn inner_specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
        member_types: Vec<ConcreteTypeId>,
        is_snapshot: bool,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(box_ty(context, ty)?).with_allow_add_const()],
            member_types
                .into_iter()
                .map(|member_ty| {
                    let inner_type =
                        if is_snapshot { snapshot_ty(context, member_ty)? } else { member_ty };
                    Ok(OutputVarInfo {
                        ty: box_ty(context, inner_type)?,
                        ref_info: OutputVarReferenceInfo::Deferred(
                            crate::extensions::lib_func::DeferredOutputKind::Generic,
                        ),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

impl NamedLibfunc for StructBoxedDeconstructLibfunc {
    type Concrete = ConcreteStructBoxedDeconstructLibfunc;

    const STR_ID: &'static str = "struct_boxed_deconstruct";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let (member_types, is_snapshot) = self.analyze_struct_type(context, ty.clone())?;
        self.inner_specialize_signature(context, ty, member_types, is_snapshot)
    }

    fn specialize(
        &self,
        context: &dyn crate::extensions::lib_func::SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let (members, is_snapshot) = self.analyze_struct_type(context, ty.clone())?;
        let signature =
            self.inner_specialize_signature(context, ty, members.clone(), is_snapshot)?;
        Ok(ConcreteStructBoxedDeconstructLibfunc { members, signature })
    }
}
