use std::ops::Shl;

use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

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
    OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId, UserTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum CircuitType {
        AddModGate(AddModGate),
        CircuitData(CircuitData),
        CircuitInput(CircuitInput),
        CircuitInputAccumulator(CircuitInputAccumulator),
    }, CircuitTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CircuitLibFunc {
         FillInput(FillCircuitInputLibFunc),
         InitCircuitData(InitCircuitDataLibFunc),
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
    Ok([CircuitInput::ID, AddModGate::ID].contains(&long_id.generic_id))
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

/// Validate that `circ_ty` is a circuit type.
fn validate_is_circuit(
    context: &dyn TypeSpecializationContext,
    circ_ty: ConcreteTypeId,
) -> Result<(), SpecializationError> {
    let struct_generic_args = extract_type_generic_args::<StructType>(context, &circ_ty)?;

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
                // More inputs to fill.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: circuit_input_accumulator_ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // All inputs were filled.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: circuit_data_ty,
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
