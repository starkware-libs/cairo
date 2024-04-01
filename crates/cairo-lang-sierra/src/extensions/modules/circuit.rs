use num_bigint::BigInt;

use super::range_check::RangeCheck96Type;
use super::structure::StructType;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
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
        CircuitInput(CircuitInput),
        CircuitInputAccumulator(CircuitInputAccumulator),
    }, CircuitTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CircuitLibFunc {
         InitCircuitData(InitCircuitData),
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
    let generic_id = long_id.generic_id;
    if generic_id == CircuitInput::ID {
        return Ok(true);
    }
    Ok(false)
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

pub struct ConcreteCircuitInput {
    // The type info of the concrete type.
    pub info: TypeInfo,
    // The index of the circuit input.
    pub idx: BigInt,
}

impl ConcreteCircuitInput {
    fn new(
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let idx = args_as_single_value(args)?;
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

    for garg in gargs {
        // Note that its enough to check the topmost types as they validate their children.
        if !is_circuit_component(context, garg)? {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    }

    Ok(())
}

#[derive(Default)]
pub struct InitCircuitData {}
impl SignatureOnlyGenericLibfunc for InitCircuitData {
    const STR_ID: &'static str = "init_circuit_data";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check96_type = context.get_concrete_type(RangeCheck96Type::id(), &[])?;
        let circuit_input_accumulator_ty =
            context.get_concrete_type(CircuitInputAccumulator::id(), generic_args)?;
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
