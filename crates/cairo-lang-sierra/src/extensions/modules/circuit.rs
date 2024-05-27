use std::ops::Shl;

use cairo_lang_utils::extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

use super::non_zero::nonzero_ty;
use super::range_check::RangeCheck96Type;
use super::structure::StructType;
use crate::extensions::bounded_int::bounded_int_ty;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureAndTypeGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_single_type, args_as_single_value, extract_type_generic_args, ConcreteType, NamedType,
    NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId, UserTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

/// The number of limbs used to represent a single value in the circuit.
pub const VALUE_SIZE: usize = 4;
/// The size of a builtin instance.
pub const BUILTIN_INSTANCE_SIZE: usize = 7;
/// A gate is defined by 3 offsets, the first two are the inputs and the third is the output.
pub const OFFSETS_PER_GATE: usize = 3;

define_type_hierarchy! {
    pub enum CircuitType {
        AddMod(AddModType),
        MulMod(MulModType),
        AddModGate(AddModGate),
        Circuit(Circuit),
        CircuitData(CircuitData),
        CircuitOutputs(CircuitOutputs),
        CircuitDescriptor(CircuitDescriptor),
        CircuitInput(CircuitInput),
        CircuitInputAccumulator(CircuitInputAccumulator),
        InverseGate(InverseGate),
    }, CircuitTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CircuitLibFunc {
         FillInput(FillCircuitInputLibFunc),
         Eval(EvalCircuitLibFunc),
         GetDescriptor(GetCircuitDescriptorLibFunc),
         InitCircuitData(InitCircuitDataLibFunc),
         U384IsZero(U384IsZeroLibfunc),
    }, CircuitConcreteLibfunc
}

/// Returns true if `garg` is a type that is considered a circuit component.
fn is_circuit_component(
    context: &dyn TypeSpecializationContext,
    garg: &GenericArg,
) -> Result<bool, SpecializationError> {
    let GenericArg::Type(ty) = garg else {
        return Err(SpecializationError::UnsupportedGenericArg);
    };

    let long_id = context.get_type_info(ty.clone())?.long_id;
    Ok([CircuitInput::ID, AddModGate::ID, InverseGate::ID].contains(&long_id.generic_id))
}

/// Circuit input type.
#[derive(Default)]
pub struct CircuitInput {}
impl NamedType for CircuitInput {
    type Concrete = ConcreteCircuitInput;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitInput");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

/// Defines an input for a circuit.
pub struct ConcreteCircuitInput {
    // The type info of the concrete type.
    pub info: TypeInfo,
    // The index of the circuit input.
    pub idx: usize,
}

impl ConcreteCircuitInput {
    fn new(
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let idx = args_as_single_value(args)?
            .to_usize()
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "CircuitInput".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: false,
                storable: false,
                zero_sized: false,
            },
            idx,
        })
    }
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
    type Concrete = ConcreteAddModGate;
    const ID: GenericTypeId = GenericTypeId::new_inline("AddModGate");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteAddModGate {
    pub info: TypeInfo,
}

impl ConcreteAddModGate {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        validate_gate_generic_args(context, args)?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "AddModGate".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: false,
                storable: false,
                zero_sized: false,
            },
        })
    }
}

impl ConcreteType for ConcreteAddModGate {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// Represents the action of computing the inverse of a fields element in the circuits builtin.
#[derive(Default)]
pub struct InverseGate {}
impl NamedType for InverseGate {
    type Concrete = ConcreteInverseGate;
    const ID: GenericTypeId = GenericTypeId::new_inline("InverseGate");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteInverseGate {
    pub info: TypeInfo,
}

impl ConcreteInverseGate {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        if args.len() != 1 {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }
        validate_args_are_circuit_components(context, args.iter())?;

        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "InverseGate".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: false,
                storable: false,
                zero_sized: false,
            },
        })
    }
}

impl ConcreteType for ConcreteInverseGate {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// Type for accumulating inputs into the circuit instance's data.
#[derive(Default)]
pub struct CircuitInputAccumulator {}
impl NamedType for CircuitInputAccumulator {
    type Concrete = ConcreteCircuitInputAccumulator;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitInputAccumulator");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteCircuitInputAccumulator {
    pub info: TypeInfo,
}

impl ConcreteCircuitInputAccumulator {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "CircuitInputAccumulator".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

impl ConcreteType for ConcreteCircuitInputAccumulator {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// A type representing a circuit instance data with all the inputs filled.
#[derive(Default)]
pub struct CircuitData {}
impl NamedType for CircuitData {
    type Concrete = ConcreteCircuitData;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitData");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteCircuitData {
    pub info: TypeInfo,
}

impl ConcreteCircuitData {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "CircuitData".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

impl ConcreteType for ConcreteCircuitData {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// A type representing a circuit instance where the outputs is filled.
#[derive(Default)]
pub struct CircuitOutputs {}
impl NamedType for CircuitOutputs {
    type Concrete = ConcreteCircuitOutputs;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitOutputs");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteCircuitOutputs {
    pub info: TypeInfo,
}

impl ConcreteCircuitOutputs {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty)?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "CircuitOutputs".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

impl ConcreteType for ConcreteCircuitOutputs {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// A type representing a circuit instance data with all the inputs filled.
#[derive(Default)]
pub struct CircuitDescriptor {}
impl NamedType for CircuitDescriptor {
    type Concrete = ConcreteCircuitDescriptor;
    const ID: GenericTypeId = GenericTypeId::new_inline("CircuitDescriptor");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct ConcreteCircuitDescriptor {
    pub info: TypeInfo,
}

impl ConcreteCircuitDescriptor {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let circ_ty = args_as_single_type(args)?;
        validate_is_circuit(context, circ_ty.clone())?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "CircuitDescriptor".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: true,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
        })
    }
}

impl ConcreteType for ConcreteCircuitDescriptor {
    fn info(&self) -> &TypeInfo {
        &self.info
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
        let outputs_tuple = args_as_single_type(args)?;
        validate_outputs_tuple(context, outputs_tuple.clone())?;
        Ok(Self {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "Circuit".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: true,
                droppable: true,
                storable: true,
                zero_sized: false,
            },
            circuit_info: get_circuit_info(context, &outputs_tuple)?,
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

/// Validate that `outputs_tuple_ty` is a tuple of circuit components.
fn validate_outputs_tuple(
    context: &dyn TypeSpecializationContext,
    outputs_tuple_ty: ConcreteTypeId,
) -> Result<(), SpecializationError> {
    let struct_generic_args = extract_type_generic_args::<StructType>(context, &outputs_tuple_ty)?;

    let mut gargs = struct_generic_args.iter();
    if !matches!(
        gargs.next(),
        Some(GenericArg::UserType(ut))
        if (*ut == UserTypeId::from_string("Tuple"))

    ) {
        return Err(SpecializationError::UnsupportedGenericArg);
    }

    validate_args_are_circuit_components(context, gargs)
}

/// Validate that all the generic arguments are circuit components.
fn validate_args_are_circuit_components<'a>(
    context: &dyn TypeSpecializationContext,
    gargs: impl Iterator<Item = &'a GenericArg>,
) -> Result<(), SpecializationError> {
    for garg in gargs {
        // Note that its enough to check the topmost types as they validate their children.
        if !is_circuit_component(context, garg)? {
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

/// libfunc for filling an input in the circuit instance's data.
#[derive(Default)]
pub struct FillCircuitInputLibFuncWrapped {}
impl SignatureAndTypeGenericLibfunc for FillCircuitInputLibFuncWrapped {
    const STR_ID: &'static str = "fill_circuit_input";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let circuit_input_accumulator_ty = context
            .get_concrete_type(CircuitInputAccumulator::id(), &[GenericArg::Type(ty.clone())])?;

        let circuit_data_ty =
            context.get_concrete_type(CircuitData::id(), &[GenericArg::Type(ty)])?;

        let u96_ty = bounded_int_ty(context, BigInt::zero(), BigInt::one().shl(96) - 1)?;

        let val_ty = context.get_concrete_type(
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
            param_signatures: vec![
                ParamSignature::new(circuit_input_accumulator_ty.clone()),
                ParamSignature::new(val_ty),
            ],
            branch_signatures: vec![
                // All inputs were filled.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: circuit_data_ty,
                        ref_info: OutputVarReferenceInfo::SimpleDerefs,
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // More inputs to fill.
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

pub type FillCircuitInputLibFunc =
    WrapSignatureAndTypeGenericLibfunc<FillCircuitInputLibFuncWrapped>;

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

/// Helper for u384 type def.
fn get_u384_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let u96_ty = bounded_int_ty(context, BigInt::zero(), BigInt::one().shl(96) - 1)?;
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

        Ok(LibfuncSignature::new_non_branch(
            vec![
                add_mod_builtin_ty.clone(),
                mul_mod_builtin_ty.clone(),
                circuit_descriptor_ty,
                circuit_data_ty,
                nonzero_ty(context, &get_u384_type(context)?)?,
                zero,
                one,
            ],
            vec![
                OutputVarInfo {
                    ty: add_mod_builtin_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: mul_mod_builtin_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 1,
                    }),
                },
                OutputVarInfo {
                    ty: context.get_concrete_type(CircuitOutputs::id(), &[GenericArg::Type(ty)])?,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

pub type EvalCircuitLibFunc = WrapSignatureAndTypeGenericLibfunc<EvalCircuitLibFuncWrapped>;

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

/// Libfunc for checking whether the given `u384` is the zero point.
#[derive(Default)]
pub struct U384IsZeroLibfunc {}
impl NoGenericArgsGenericLibfunc for U384IsZeroLibfunc {
    const STR_ID: &'static str = "u384_is_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u384_ty = get_u384_type(context)?;
        let nonzero_u384_ty = nonzero_ty(context, &u384_ty)?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(u384_ty.clone())],
            branch_signatures: vec![
                // Zero.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // NonZero.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_u384_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
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
    ty: &ConcreteTypeId,
) -> Result<CircuitInfo, SpecializationError> {
    let ty_info = context.get_type_info(ty.clone())?;

    // Skip user type.
    let circ_outputs = ty_info.long_id.generic_args.iter().skip(1);

    let ParsedInputs { mut values, mut mul_offsets } =
        parse_circuit_inputs(context, circ_outputs.clone())?;
    let n_inputs = values.len();
    let mut add_offsets = vec![];

    let mut stack = circ_outputs
        .map(|garg| (extract_matches!(garg, GenericArg::Type).clone(), true))
        .collect::<Vec<_>>();

    // The offset of the input that has the value `1`.
    let one_offset = n_inputs;

    // We visit each gate in the circuit twice, in the first visit push all its inputs
    // and in the second visit we assume that all the inputs were already visited and we can
    // allocate a value for the outputs and prepare the offsets in the relevant builtin.
    while let Some((ty, first_visit)) = stack.pop() {
        let long_id = context.get_type_info(ty.clone())?.long_id;
        let generic_id = long_id.generic_id;

        if generic_id == CircuitInput::ID {
        } else {
            let gate_inputs =
                long_id.generic_args.iter().map(|garg| extract_matches!(garg, GenericArg::Type));

            if first_visit {
                if ![AddModGate::ID, InverseGate::ID].contains(&generic_id) {
                    return Err(SpecializationError::UnsupportedGenericArg);
                }

                stack.push((ty, false));
                stack.extend(gate_inputs.map(|ty| (ty.clone(), true)))
            } else {
                let output_offset = n_inputs + 1 + values.len();
                let mut input_offsets = gate_inputs.map(|ty| *values.get(ty).unwrap());

                if generic_id == AddModGate::ID {
                    add_offsets.push(GateOffsets {
                        lhs: input_offsets.next().unwrap(),
                        rhs: input_offsets.next().unwrap(),
                        output: output_offset,
                    });

                    values.insert(ty.clone(), output_offset);
                } else if generic_id == InverseGate::ID {
                    mul_offsets.push(GateOffsets {
                        lhs: output_offset,
                        rhs: input_offsets.next().unwrap(),
                        output: one_offset,
                    });
                } else {
                    return Err(SpecializationError::UnsupportedGenericArg);
                };
            }
        }
    }

    Ok(CircuitInfo { n_inputs, values, add_offsets, mul_offsets })
}

/// Parses the circuit inputs and returns `ParsedInputs`.
/// Inputs that feed a addmod gate are require reduction and are fed to a mul gate.
fn parse_circuit_inputs<'a>(
    context: &dyn TypeSpecializationContext,
    circuit_outputs: impl Iterator<Item = &'a GenericArg>,
) -> Result<ParsedInputs, SpecializationError> {
    let mut stack = circuit_outputs
        .map(|garg| extract_matches!(garg, GenericArg::Type).clone())
        .collect::<Vec<_>>();

    let mut inputs: UnorderedHashMap<usize, ConcreteTypeId> = Default::default();

    while let Some(ty) = stack.pop() {
        let long_id = context.get_type_info(ty.clone())?.long_id;
        let generic_id = long_id.generic_id;
        if generic_id == CircuitInput::ID {
            let idx = args_as_single_value(&long_id.generic_args)?
                .to_usize()
                .ok_or(SpecializationError::UnsupportedGenericArg)?;
            inputs.insert(idx, ty);
        } else if [AddModGate::ID, InverseGate::ID].contains(&generic_id) {
            stack.extend(
                long_id
                    .generic_args
                    .iter()
                    .map(|garg| extract_matches!(garg, GenericArg::Type).clone()),
            );
        } else {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    }

    let mut values: UnorderedHashMap<ConcreteTypeId, usize> = Default::default();
    let n_inputs = inputs.len();

    // The reduced_inputs start at n_inputs + 1 since we need to reserve a slot for the value 1.
    let mut reduced_input_offset = n_inputs + 1;
    let mut mul_offsets = vec![];

    for (input_idx, ty) in inputs.iter_sorted() {
        // Add the gate result = 1 * input to reduce the input module the modulus.
        mul_offsets.push(GateOffsets { lhs: n_inputs, rhs: *input_idx, output: 1 + values.len() });
        values.insert(ty.clone(), reduced_input_offset);
        reduced_input_offset += 1;
    }

    Ok(ParsedInputs { values, mul_offsets })
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

    /// Maps a concrete type to it's offset in the values array.
    /// The values mapping does not include the optional 1 input which is stored at the
    /// the index n_inputs.
    /// The input 1 is located at offset n_inputs and is not part of this mapping.
    pub values: UnorderedHashMap<ConcreteTypeId, usize>,
    /// The offsets for the add gates.
    pub add_offsets: Vec<GateOffsets>,
    /// The offsets for the mul gates.
    pub mul_offsets: Vec<GateOffsets>,
}

struct ParsedInputs {
    /// Maps a concrete type to it's offset in the values array.
    values: UnorderedHashMap<ConcreteTypeId, usize>,
    /// The offsets for the mul gates that are used to reduce the inputs.
    mul_offsets: Vec<GateOffsets>,
}
