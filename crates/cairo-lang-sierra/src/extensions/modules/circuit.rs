use std::ops::Shl;
use std::sync::LazyLock;

use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{extract_matches, require};
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};

use super::range_check::RangeCheck96Type;
use super::structure::StructType;
use super::utils::{Range, reinterpret_cast_signature};
use crate::extensions::bounded_int::bounded_int_ty;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureAndTypeGenericLibfunc, SignatureSpecializationContext,
    SpecializationContext, WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
    args_as_single_type, args_as_single_value, args_as_two_types, extract_type_generic_args,
};
use crate::ids::{ConcreteTypeId, GenericTypeId, UserTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

/// The set of types that are considered circuit components.
/// A circuit it defined as Circuit<(Output0, Output1, ...)> where all the outputs are
/// circuit components (recursively).
static CIRCUIT_COMPONENTS: LazyLock<UnorderedHashSet<GenericTypeId>> = LazyLock::new(|| {
    UnorderedHashSet::from_iter([
        CircuitInput::ID,
        AddModGate::ID,
        InverseGate::ID,
        MulModGate::ID,
        SubModGate::ID,
    ])
});

/// The number of limbs used to represent a single value in the circuit.
pub const VALUE_SIZE: usize = 4;
/// The size of the AddMod and MulMod builtin instances.
pub const MOD_BUILTIN_INSTANCE_SIZE: usize = 7;
/// A gate is defined by 3 offsets, the first two are the inputs and the third is the output.
pub const OFFSETS_PER_GATE: usize = 3;
/// The offset of the values in the values array.
pub const ONE_OFFSET: usize = 0;

define_type_hierarchy! {
    pub enum CircuitType {
        AddMod(AddModType),
        MulMod(MulModType),
        AddModGate(AddModGate),
        Circuit(Circuit),
        CircuitData(CircuitData),
        CircuitOutputs(CircuitOutputs),
        CircuitPartialOutputs(CircuitPartialOutputs),
        CircuitDescriptor(CircuitDescriptor),
        CircuitFailureGuarantee(CircuitFailureGuarantee),
        CircuitInput(CircuitInput),
        CircuitInputAccumulator(CircuitInputAccumulator),
        CircuitModulus(CircuitModulus),
        InverseGate(InverseGate),
        MulModGate(MulModGate),
        SubModGate(SubModGate),
        U96Guarantee(U96Guarantee),
        U96LimbsLessThanGuarantee(U96LimbsLessThanGuarantee),
    }, CircuitTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CircuitLibFunc {
        AddInput(AddCircuitInputLibFunc),
        Eval(EvalCircuitLibFunc),
        GetDescriptor(GetCircuitDescriptorLibFunc),
        InitCircuitData(InitCircuitDataLibFunc),
        GetOutput(GetOutputLibFunc),
        TryIntoCircuitModulus(TryIntoCircuitModulusLibFunc),
        FailureGuaranteeVerify(CircuitFailureGuaranteeVerifyLibFunc),
        IntoU96Guarantee(IntoU96GuaranteeLibFunc),
        U96GuaranteeVerify(U96GuaranteeVerifyLibFunc),
        U96LimbsLessThanGuaranteeVerify(U96LimbsLessThanGuaranteeVerifyLibfunc),
        U96SingleLimbLessThanGuaranteeVerify(U96SingleLimbLessThanGuaranteeVerifyLibfunc),
    }, CircuitConcreteLibfunc
}

/// Returns true if `generic_arg` is a type that is considered a circuit component.
fn is_circuit_component(
    context: &dyn TypeSpecializationContext,
    generic_arg: &GenericArg,
) -> Result<bool, SpecializationError> {
    let GenericArg::Type(ty) = generic_arg else {
        return Err(SpecializationError::UnsupportedGenericArg);
    };

    let long_id = context.get_type_info(ty.clone())?.long_id;
    Ok(CIRCUIT_COMPONENTS.contains(&long_id.generic_id))
}

/// Circuit input type.
#[derive(Default)]
pub struct CircuitInput {}
impl NamedType for CircuitInput {
    type Concrete = ConcreteCircuitInput;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitInput");

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let idx = args_as_single_value(args)?
            .to_usize()
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        Ok(Self::Concrete { info: circuit_component_type_info(Self::ID, args), idx })
    }
}

/// Defines an input for a circuit.
pub struct ConcreteCircuitInput {
    // The type info of the concrete type.
    pub info: TypeInfo,
    // The index of the circuit input.
    pub idx: usize,
}

impl ConcreteType for ConcreteCircuitInput {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// Validate gate generic arguments.
fn validate_gate_generic_args(
    context: &dyn TypeSpecializationContext,
    args: &[GenericArg],
) -> Result<(), SpecializationError> {
    if args.len() != 2 {
        return Err(SpecializationError::WrongNumberOfGenericArgs);
    }
    validate_args_are_circuit_components(context, args.iter())
}

/// Represents the action of adding two fields elements in the circuits builtin.
#[derive(Default)]
pub struct AddModGate {}
impl NamedType for AddModGate {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("AddModGate");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        validate_gate_generic_args(context, args)?;
        Ok(Self::Concrete { info: circuit_component_type_info(Self::ID, args) })
    }
}

// Represents the action of adding two fields elements in the circuits builtin.
#[derive(Default)]
pub struct SubModGate {}
impl NamedType for SubModGate {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("SubModGate");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        validate_gate_generic_args(context, args)?;
        Ok(Self::Concrete { info: circuit_component_type_info(Self::ID, args) })
    }
}

/// Represents the action of multiplying two fields elements in the circuits builtin.
#[derive(Default)]
pub struct MulModGate {}
impl NamedType for MulModGate {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("MulModGate");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        validate_gate_generic_args(context, args)?;
        Ok(Self::Concrete { info: circuit_component_type_info(Self::ID, args) })
    }
}

/// Represents the action of computing the inverse of a fields element in the circuits builtin.
#[derive(Default)]
pub struct InverseGate {}
impl NamedType for InverseGate {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("InverseGate");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        if args.len() != 1 {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }
        validate_args_are_circuit_components(context, args.iter())?;
        Ok(Self::Concrete { info: circuit_component_type_info(Self::ID, args) })
    }
}

/// Type for accumulating inputs into the circuit instance's data.
#[derive(Default)]
pub struct CircuitInputAccumulator {}
impl NamedType for CircuitInputAccumulator {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitInputAccumulator");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

/// A type that can be used as a circuit modulus (a u384 that is not zero or one)
#[derive(Default)]
pub struct CircuitModulus {}
impl NoGenericArgsGenericType for CircuitModulus {
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitModulus");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// A type representing a circuit instance data with all the inputs added.
#[derive(Default)]
pub struct CircuitData {}
impl NamedType for CircuitData {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitData");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

/// A type representing a circuit instance where the outputs is filled.
#[derive(Default)]
pub struct CircuitOutputs {}
impl NamedType for CircuitOutputs {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitOutputs");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
                duplicatable: true,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

/// A type representing a circuit instance where the outputs are partially filled as
/// the evaluation of one of the inverse gates failed.
#[derive(Default)]
pub struct CircuitPartialOutputs {}
impl NamedType for CircuitPartialOutputs {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitPartialOutputs");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

/// A type whose destruction guarantees that the circuit instance invocation failed.
#[derive(Default)]
pub struct CircuitFailureGuarantee {}
impl NoGenericArgsGenericType for CircuitFailureGuarantee {
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitFailureGuarantee");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

/// A type whose destruction guarantees that u96-limbs based value is smaller than another.
#[derive(Default)]
pub struct U96LimbsLessThanGuarantee {}
impl NamedType for U96LimbsLessThanGuarantee {
    type Concrete = ConcreteU96LimbsLessThanGuarantee;
    // Shortened name to fit in the 23 bytes limit.
    const ID: GenericTypeId = GenericTypeId::new_inline("U96LimbsLtGuarantee");

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let limb_count = args_as_single_value(args)?
            .to_usize()
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        require(limb_count >= 1).ok_or(SpecializationError::UnsupportedGenericArg)?;
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: <Self as NamedType>::ID,
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: false,
                storable: true,
                zero_sized: false,
            },
            limb_count,
        })
    }
}

pub struct ConcreteU96LimbsLessThanGuarantee {
    pub info: TypeInfo,
    pub limb_count: usize,
}

impl ConcreteType for ConcreteU96LimbsLessThanGuarantee {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// A value that is guaranteed to fit in a u96.
/// This value can only be dropped by being written to a 96bit range check.
#[derive(Default)]
pub struct U96Guarantee {}
impl NoGenericArgsGenericType for U96Guarantee {
    const ID: GenericTypeId = GenericTypeId::new_inline("U96Guarantee");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

/// A type representing the circuit add and mul tables.
#[derive(Default)]
pub struct CircuitDescriptor {}
impl NamedType for CircuitDescriptor {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitDescriptor");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty.clone())?;
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
                duplicatable: true,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

/// A type that creates a circuit from a tuple of outputs.
#[derive(Default)]
pub struct Circuit {}
impl NamedType for Circuit {
    type Concrete = ConcreteCircuit;
    const ID: GenericTypeId = GenericTypeId::new_inline("Circuit");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteCircuit {
    pub info: TypeInfo,
    pub circuit_info: CircuitInfo,
}

impl ConcreteCircuit {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let output_tuple = args_as_single_type(args)?;

        let circuit_info = get_circuit_info(context, &output_tuple)?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "Circuit".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: false,
                storable: false,
                zero_sized: true,
            },
            circuit_info,
        })
    }
}

impl ConcreteType for ConcreteCircuit {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// Validate that `circ_ty` is a circuit type.
fn validate_is_circuit(
    context: &dyn TypeSpecializationContext,
    circ_ty: ConcreteTypeId,
) -> Result<(), SpecializationError> {
    if context.get_type_info(circ_ty.clone())?.long_id.generic_id != Circuit::ID {
        return Err(SpecializationError::UnsupportedGenericArg);
    }
    Ok(())
}

/// Validate that all the generic arguments are circuit components.
fn validate_args_are_circuit_components<'a>(
    context: &dyn TypeSpecializationContext,
    generic_args: impl Iterator<Item = &'a GenericArg>,
) -> Result<(), SpecializationError> {
    for generic_arg in generic_args {
        // Note that its enough to check the topmost types as they validate their children.
        if !is_circuit_component(context, generic_arg)? {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    }

    Ok(())
}

/// Libfunc for initializing the input data for running an instance of the circuit.
#[derive(Default)]
pub struct InitCircuitDataLibFuncWrapped {}
impl SignatureAndTypeGenericLibfunc for InitCircuitDataLibFuncWrapped {
    const STR_ID: &'static str = "init_circuit_data";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check96_type = context.get_concrete_type(RangeCheck96Type::id(), &[])?;
        let circuit_input_accumulator_ty =
            context.get_concrete_type(CircuitInputAccumulator::id(), &[GenericArg::Type(ty)])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(range_check96_type.clone()).with_allow_add_const()],
            vec![
                OutputVarInfo {
                    ty: range_check96_type.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: circuit_input_accumulator_ty.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

pub type InitCircuitDataLibFunc = WrapSignatureAndTypeGenericLibfunc<InitCircuitDataLibFuncWrapped>;

/// libfunc for adding an input in the circuit instance's data.
#[derive(Default)]
pub struct AddCircuitInputLibFuncWrapped {}
impl SignatureAndTypeGenericLibfunc for AddCircuitInputLibFuncWrapped {
    const STR_ID: &'static str = "add_circuit_input";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let circuit_input_accumulator_ty = context
            .get_concrete_type(CircuitInputAccumulator::id(), &[GenericArg::Type(ty.clone())])?;

        let circuit_data_ty =
            context.get_concrete_type(CircuitData::id(), &[GenericArg::Type(ty)])?;

        let u96_guarantee_ty = context.get_concrete_type(U96Guarantee::id(), &[])?;

        let val_ty = context.get_concrete_type(
            StructType::id(),
            &[
                GenericArg::UserType(UserTypeId::from_string("Tuple")),
                GenericArg::Type(u96_guarantee_ty.clone()),
                GenericArg::Type(u96_guarantee_ty.clone()),
                GenericArg::Type(u96_guarantee_ty.clone()),
                GenericArg::Type(u96_guarantee_ty),
            ],
        )?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(circuit_input_accumulator_ty.clone()),
                ParamSignature::new(val_ty),
            ],
            branch_signatures: vec![
                // All inputs were added.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: circuit_data_ty,
                        ref_info: OutputVarReferenceInfo::SimpleDerefs,
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // More inputs to add.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: circuit_input_accumulator_ty,
                        ref_info: OutputVarReferenceInfo::SimpleDerefs,
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

pub type AddCircuitInputLibFunc = WrapSignatureAndTypeGenericLibfunc<AddCircuitInputLibFuncWrapped>;

/// A zero-input function that returns an handle to the offsets of a circuit.
#[derive(Default)]
pub struct GetCircuitDescriptorLibFuncWrapped {}
impl SignatureAndTypeGenericLibfunc for GetCircuitDescriptorLibFuncWrapped {
    const STR_ID: &'static str = "get_circuit_descriptor";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let circuit_descriptor_ty =
            context.get_concrete_type(CircuitDescriptor::id(), &[GenericArg::Type(ty.clone())])?;

        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: circuit_descriptor_ty,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Helper for u96 type def.
fn get_u96_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    bounded_int_ty(context, BigInt::zero(), BigInt::one().shl(96) - 1)
}

/// Helper for u384 type def.
fn get_u384_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let u96_ty = get_u96_type(context)?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::circuit::u384")),
            GenericArg::Type(u96_ty.clone()),
            GenericArg::Type(u96_ty.clone()),
            GenericArg::Type(u96_ty.clone()),
            GenericArg::Type(u96_ty),
        ],
    )
}

pub type GetCircuitDescriptorLibFunc =
    WrapSignatureAndTypeGenericLibfunc<GetCircuitDescriptorLibFuncWrapped>;

/// A zero-input function that returns an handle to the offsets of a circuit.
#[derive(Default)]
pub struct EvalCircuitLibFuncWrapped {}
impl SignatureAndTypeGenericLibfunc for EvalCircuitLibFuncWrapped {
    const STR_ID: &'static str = "eval_circuit";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let add_mod_builtin_ty = context.get_concrete_type(AddModType::id(), &[])?;
        let mul_mod_builtin_ty = context.get_concrete_type(MulModType::id(), &[])?;

        let circuit_descriptor_ty =
            context.get_concrete_type(CircuitDescriptor::id(), &[GenericArg::Type(ty.clone())])?;
        let circuit_data_ty =
            context.get_concrete_type(CircuitData::id(), &[GenericArg::Type(ty.clone())])?;

        let zero = bounded_int_ty(context, BigInt::zero(), BigInt::zero())?;
        let one = bounded_int_ty(context, BigInt::one(), BigInt::one())?;

        Ok(LibfuncSignature {
            param_signatures: [
                add_mod_builtin_ty.clone(),
                mul_mod_builtin_ty.clone(),
                circuit_descriptor_ty,
                circuit_data_ty,
                context.get_concrete_type(CircuitModulus::id(), &[])?,
                zero,
                one,
            ]
            .into_iter()
            .map(|ty| ParamSignature::new(ty.clone()))
            .collect(),
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(add_mod_builtin_ty.clone(), 0),
                        OutputVarInfo {
                            ty: mul_mod_builtin_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                        OutputVarInfo {
                            ty: context.get_concrete_type(
                                CircuitOutputs::id(),
                                &[GenericArg::Type(ty.clone())],
                            )?,
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                    ],

                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure (inverse of non-invertible).
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(add_mod_builtin_ty, 0),
                        OutputVarInfo {
                            ty: mul_mod_builtin_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                        OutputVarInfo {
                            ty: context.get_concrete_type(
                                CircuitPartialOutputs::id(),
                                &[GenericArg::Type(ty)],
                            )?,
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                        OutputVarInfo {
                            ty: context.get_concrete_type(CircuitFailureGuarantee::id(), &[])?,
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                    ],

                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

pub type EvalCircuitLibFunc = WrapSignatureAndTypeGenericLibfunc<EvalCircuitLibFuncWrapped>;

/// Converts 'T' into a 'U96Guarantee'.
/// 'T' must be a value that fits inside a u96, for example: u8, u96 or BoundedInt<0, 12>.
#[derive(Default)]
pub struct IntoU96GuaranteeLibFuncWrapped {}
impl SignatureAndTypeGenericLibfunc for IntoU96GuaranteeLibFuncWrapped {
    const STR_ID: &'static str = "into_u96_guarantee";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range = Range::from_type(context, ty.clone())?;
        require(!range.lower.is_negative() && range.upper <= BigInt::one().shl(96))
            .ok_or(SpecializationError::UnsupportedGenericArg)?;

        let guarantee_ty = context.get_concrete_type(U96Guarantee::id(), &[])?;
        Ok(reinterpret_cast_signature(ty, guarantee_ty))
    }
}

pub type IntoU96GuaranteeLibFunc =
    WrapSignatureAndTypeGenericLibfunc<IntoU96GuaranteeLibFuncWrapped>;

/// Libfunc for verifying and dropping a `U96Guarantee`.
#[derive(Default)]
pub struct U96GuaranteeVerifyLibFunc {}
impl NoGenericArgsGenericLibfunc for U96GuaranteeVerifyLibFunc {
    const STR_ID: &'static str = "u96_guarantee_verify";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check96_type = context.get_concrete_type(RangeCheck96Type::id(), &[])?;
        let guarantee_ty = context.get_concrete_type(U96Guarantee::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check96_type.clone()).with_allow_add_const(),
                ParamSignature::new(guarantee_ty),
            ],
            vec![OutputVarInfo::new_builtin(range_check96_type, 0)],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for getting an output of a circuit.
#[derive(Default)]
pub struct GetOutputLibFunc {}
impl NamedLibfunc for GetOutputLibFunc {
    const STR_ID: &'static str = "get_circuit_output";

    type Concrete = ConcreteGetOutputLibFunc;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (circ_ty, output_ty) = args_as_two_types(args)?;
        if !CIRCUIT_COMPONENTS.contains(&context.get_type_info(output_ty)?.long_id.generic_id) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let outputs_ty =
            context.get_concrete_type(CircuitOutputs::id(), &[GenericArg::Type(circ_ty)])?;

        let u384_ty = get_u384_type(context)?;
        let guarantee_ty = u384_less_than_guarantee_ty(context)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![outputs_ty],
            vec![
                OutputVarInfo { ty: u384_ty, ref_info: OutputVarReferenceInfo::SimpleDerefs },
                OutputVarInfo { ty: guarantee_ty, ref_info: OutputVarReferenceInfo::SimpleDerefs },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (circuit_ty, output_ty) = args_as_two_types(args)?;

        // TODO(ilya): Fail if `circuit_ty` does not contain output_ty.
        Ok(ConcreteGetOutputLibFunc {
            signature: self.specialize_signature(context.upcast(), args)?,
            circuit_ty,
            output_ty,
        })
    }
}

/// Struct the data for a multi pop action.
pub struct ConcreteGetOutputLibFunc {
    pub signature: LibfuncSignature,
    pub circuit_ty: ConcreteTypeId,
    pub output_ty: ConcreteTypeId,
}
impl SignatureBasedConcreteLibfunc for ConcreteGetOutputLibFunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Verifies the circuit evaluation has failed.
#[derive(Default)]
pub struct CircuitFailureGuaranteeVerifyLibFunc {}
impl NoGenericArgsGenericLibfunc for CircuitFailureGuaranteeVerifyLibFunc {
    const STR_ID: &'static str = "circuit_failure_guarantee_verify";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check96_type = context.get_concrete_type(RangeCheck96Type::id(), &[])?;
        let mul_mod_builtin_ty = context.get_concrete_type(MulModType::id(), &[])?;
        let guarantee_ty = context.get_concrete_type(CircuitFailureGuarantee::id(), &[])?;

        let zero = bounded_int_ty(context, BigInt::zero(), BigInt::zero())?;
        let one = bounded_int_ty(context, BigInt::one(), BigInt::one())?;

        Ok(LibfuncSignature::new_non_branch(
            vec![range_check96_type.clone(), mul_mod_builtin_ty.clone(), guarantee_ty, zero, one],
            vec![
                OutputVarInfo::new_builtin(range_check96_type, 0),
                OutputVarInfo::new_builtin(mul_mod_builtin_ty, 1),
                OutputVarInfo {
                    ty: u384_less_than_guarantee_ty(context)?,
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Verifies that numbers with u96 limbs are one larger than the other.
#[derive(Default)]
pub struct U96LimbsLessThanGuaranteeVerifyLibfunc {}
impl NamedLibfunc for U96LimbsLessThanGuaranteeVerifyLibfunc {
    const STR_ID: &'static str = "u96_limbs_less_than_guarantee_verify";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let limb_count = args_as_single_value(args)?
            .to_usize()
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        require(limb_count > 1).ok_or(SpecializationError::UnsupportedGenericArg)?;
        let in_guarantee_ty = u96_limbs_less_than_guarantee_ty(context, limb_count)?;
        let u96_lt_guarantee_ty = context.get_concrete_type(U96Guarantee::id(), &[])?;
        let eq_guarantee_ty = u96_limbs_less_than_guarantee_ty(context, limb_count - 1)?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(in_guarantee_ty)],
            branch_signatures: vec![
                // Most significant limbs are equal - move to the next smaller guarantee.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: eq_guarantee_ty,
                        ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Most significant limbs are different - checking the diff is in u96 range.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: u96_lt_guarantee_ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }

    type Concrete = ConcreteU96LimbsLessThanGuaranteeVerifyLibfunc;

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let limb_count = args_as_single_value(args)?
            .to_usize()
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        Ok(Self::Concrete {
            signature: self.specialize_signature(context.upcast(), args)?,
            limb_count,
        })
    }
}
pub struct ConcreteU96LimbsLessThanGuaranteeVerifyLibfunc {
    signature: LibfuncSignature,
    pub limb_count: usize,
}
impl SignatureBasedConcreteLibfunc for ConcreteU96LimbsLessThanGuaranteeVerifyLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Verifies that numbers with a single u96 limb are one larger than the other.
#[derive(Default)]
pub struct U96SingleLimbLessThanGuaranteeVerifyLibfunc {}
impl NoGenericArgsGenericLibfunc for U96SingleLimbLessThanGuaranteeVerifyLibfunc {
    const STR_ID: &'static str = "u96_single_limb_less_than_guarantee_verify";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let in_guarantee_ty = u96_limbs_less_than_guarantee_ty(context, 1)?;
        let u96_lt_guarantee_ty = context.get_concrete_type(U96Guarantee::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![in_guarantee_ty],
            vec![OutputVarInfo {
                ty: u96_lt_guarantee_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Type for add mod builtin.
#[derive(Default)]
pub struct AddModType {}
impl NoGenericArgsGenericType for AddModType {
    const ID: GenericTypeId = GenericTypeId::new_inline("AddMod");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

/// Type for mul mod builtin.
#[derive(Default)]
pub struct MulModType {}
impl NoGenericArgsGenericType for MulModType {
    const ID: GenericTypeId = GenericTypeId::new_inline("MulMod");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

/// Libfunc for checking whether the given `u384` (given as 4 limbs of `u96`) is zero.
#[derive(Default)]
pub struct TryIntoCircuitModulusLibFunc {}
impl NoGenericArgsGenericLibfunc for TryIntoCircuitModulusLibFunc {
    const STR_ID: &'static str = "try_into_circuit_modulus";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u96_ty = get_u96_type(context)?;
        let value_type = context.get_concrete_type(
            StructType::id(),
            &[
                GenericArg::UserType(UserTypeId::from_string("Tuple")),
                GenericArg::Type(u96_ty.clone()),
                GenericArg::Type(u96_ty.clone()),
                GenericArg::Type(u96_ty.clone()),
                GenericArg::Type(u96_ty),
            ],
        )?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(value_type)],
            branch_signatures: vec![
                // Success (value >= 2).
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: context.get_concrete_type(CircuitModulus::id(), &[])?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure (value is zero or one).
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Gets a concrete type, if it is a const type returns a vector of the values to be stored in
/// the const segment.
fn get_circuit_info(
    context: &dyn TypeSpecializationContext,
    outputs_tuple_ty: &ConcreteTypeId,
) -> Result<CircuitInfo, SpecializationError> {
    let struct_generic_args = extract_type_generic_args::<StructType>(context, outputs_tuple_ty)?;
    let mut generic_args = struct_generic_args.iter();
    if !matches!(
        generic_args.next(),
        Some(GenericArg::UserType(ut))
        if (*ut == UserTypeId::from_string("Tuple"))
    ) {
        return Err(SpecializationError::UnsupportedGenericArg);
    }

    let circ_outputs = generic_args;

    validate_args_are_circuit_components(context, circ_outputs.clone())?;

    let ParsedInputs { reduced_inputs: mut values, mut mul_offsets } =
        parse_circuit_inputs(context, circ_outputs.clone())?;
    let n_inputs = values.len();
    require(n_inputs >= 1).ok_or(SpecializationError::UnsupportedGenericArg)?;

    let mut add_offsets = vec![];

    // We visit each gate in the circuit twice.
    // In the first visit of a gate, push all its inputs to the stack.
    // In the second visit of a gate, we assume that all the inputs were already visited (twice)
    // and we can allocate a value for the output and update `add_offsets` or `mul_offsets`.
    //
    // The stack contains pairs of (type, first_visit).
    let mut stack: Vec<(ConcreteTypeId, bool)> = circ_outputs
        .map(|generic_arg| (extract_matches!(generic_arg, GenericArg::Type).clone(), true))
        .collect();

    while let Some((ty, first_visit)) = stack.pop() {
        let long_id = context.get_type_info(ty.clone())?.long_id;
        let generic_id = long_id.generic_id;

        if values.contains_key(&ty) {
            // The value was already processed.
            continue;
        }

        let gate_inputs = long_id
            .generic_args
            .iter()
            .map(|generic_arg| extract_matches!(generic_arg, GenericArg::Type));

        if first_visit {
            stack.push((ty, false));
            stack.extend(gate_inputs.map(|ty| (ty.clone(), true)))
        } else {
            let output_offset = 1 + n_inputs + values.len();
            let mut input_offsets = gate_inputs.map(|ty| values[ty]);

            if generic_id == AddModGate::ID {
                add_offsets.push(GateOffsets {
                    lhs: input_offsets.next().unwrap(),
                    rhs: input_offsets.next().unwrap(),
                    output: output_offset,
                });
            } else if generic_id == SubModGate::ID {
                // output = sub_lhs - sub_rhs => output + sub_rhs = sub_lhs.
                let sub_lhs = input_offsets.next().unwrap();
                let sub_rhs = input_offsets.next().unwrap();
                add_offsets.push(GateOffsets { lhs: output_offset, rhs: sub_rhs, output: sub_lhs });
            } else if generic_id == MulModGate::ID {
                mul_offsets.push(GateOffsets {
                    lhs: input_offsets.next().unwrap(),
                    rhs: input_offsets.next().unwrap(),
                    output: output_offset,
                });
            } else if generic_id == InverseGate::ID {
                // output = 1 / input => 1 = output * input.
                // Note that the gate will fail if the input is not invertible.
                // Evaluating this gate successfully implies that input is invertible.
                mul_offsets.push(GateOffsets {
                    lhs: output_offset,
                    rhs: input_offsets.next().unwrap(),
                    output: ONE_OFFSET,
                });
            } else {
                return Err(SpecializationError::UnsupportedGenericArg);
            };

            // Make sure all the gate inputs were consumed.
            assert!(input_offsets.next().is_none());
            values.insert(ty.clone(), output_offset);
        }
    }

    Ok(CircuitInfo { n_inputs, values, add_offsets, mul_offsets })
}

/// Parses the circuit inputs and returns `ParsedInputs`.
fn parse_circuit_inputs<'a>(
    context: &dyn TypeSpecializationContext,
    circuit_outputs: impl Iterator<Item = &'a GenericArg>,
) -> Result<ParsedInputs, SpecializationError> {
    let mut stack: Vec<ConcreteTypeId> = circuit_outputs
        .map(|generic_arg| extract_matches!(generic_arg, GenericArg::Type).clone())
        .collect();

    let mut inputs: UnorderedHashMap<usize, ConcreteTypeId> = Default::default();

    let mut visited = UnorderedHashSet::<_>::default();

    while let Some(ty) = stack.pop() {
        if !visited.insert(ty.clone()) {
            // Already visited.
            continue;
        }

        let long_id = context.get_type_info(ty.clone())?.long_id;
        let generic_id = long_id.generic_id;
        if generic_id == CircuitInput::ID {
            let idx = args_as_single_value(&long_id.generic_args)?
                .to_usize()
                .ok_or(SpecializationError::UnsupportedGenericArg)?;
            assert!(inputs.insert(idx, ty).is_none());
        } else {
            // generic_id must be a gate. This was validated in `validate_output_tuple`.
            stack.extend(
                long_id
                    .generic_args
                    .iter()
                    .map(|generic_arg| extract_matches!(generic_arg, GenericArg::Type).clone()),
            );
        }
    }

    let mut reduced_inputs: UnorderedHashMap<ConcreteTypeId, usize> = Default::default();
    let n_inputs = inputs.len();

    // The reduced_inputs start at `1 + n_inputs` since we need to reserve slot 0 for the constant
    // value `1`.
    let reduced_input_start = 1 + n_inputs;
    let mut mul_offsets = vec![];

    for (idx, (input_idx, ty)) in inputs.iter_sorted().enumerate() {
        // Validate that the input indices are `[0, 1, ..., n_inputs - 1]`.
        require(idx == *input_idx).ok_or(SpecializationError::UnsupportedGenericArg)?;
        assert!(reduced_inputs.insert(ty.clone(), reduced_input_start + idx).is_none());
        // Add the gate `1 * input = result` to reduce the input module the modulus.
        mul_offsets.push(GateOffsets {
            lhs: ONE_OFFSET,
            rhs: 1 + idx,
            output: reduced_input_start + idx,
        });
    }

    Ok(ParsedInputs { reduced_inputs, mul_offsets })
}

/// Describes the offset that define a gate in a circuit.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GateOffsets {
    pub lhs: usize,
    pub rhs: usize,
    pub output: usize,
}

/// Describes a circuit in the program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CircuitInfo {
    /// The number of circuit inputs.
    pub n_inputs: usize,

    /// Maps a concrete type to its offset (measured in number of elements) in the values array.
    /// The values mapping does not include the constant input `1` which is stored at offset `0`.
    pub values: UnorderedHashMap<ConcreteTypeId, usize>,
    /// The offsets for the add gates. This includes AddModGate and SubModGate.
    pub add_offsets: Vec<GateOffsets>,
    /// The offsets for the mul gates. This includes MulModGate, InverseGate, and input reductions.
    pub mul_offsets: Vec<GateOffsets>,
}

impl CircuitInfo {
    /// Returns the number of 96-bit range checks used by the circuit.
    ///
    /// We use:
    /// * 1 slot for the const 1,
    /// * `n_inputs` slots for the original unreduced inputs,
    /// * `self.values.len()` for all the intermediate values and outputs (including the reduced
    ///   inputs).
    pub fn rc96_usage(&self) -> usize {
        (1 + self.n_inputs + self.values.len()) * VALUE_SIZE
    }
}

struct ParsedInputs {
    /// Maps a concrete input type to its offset in the values array.
    reduced_inputs: UnorderedHashMap<ConcreteTypeId, usize>,
    /// The offsets for the mul gates that are used to reduce the inputs.
    mul_offsets: Vec<GateOffsets>,
}

/// Returns the guarantee type for u384 values comparison.
fn u384_less_than_guarantee_ty(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    u96_limbs_less_than_guarantee_ty(context, 4)
}

/// Returns the guarantee type for a number with u96 limbs values comparison.
fn u96_limbs_less_than_guarantee_ty(
    context: &dyn SignatureSpecializationContext,
    limb_count: usize,
) -> Result<ConcreteTypeId, SpecializationError> {
    context
        .get_concrete_type(U96LimbsLessThanGuarantee::id(), &[GenericArg::Value(limb_count.into())])
}

/// Returns the type info of the circuit component.
fn circuit_component_type_info(generic_id: GenericTypeId, args: &[GenericArg]) -> TypeInfo {
    TypeInfo {
        long_id: ConcreteTypeLongId { generic_id, generic_args: args.to_vec() },
        duplicatable: false,
        droppable: false,
        storable: false,
        zero_sized: true,
    }
}
