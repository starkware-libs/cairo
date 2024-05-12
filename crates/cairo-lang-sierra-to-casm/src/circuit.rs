use cairo_lang_sierra::extensions::circuit::{
    CircuitTypeConcrete, ConcreteCircuit, ConcreteCircuitInput,
};
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType, CoreTypeConcrete};
use cairo_lang_sierra::extensions::structure::StructConcreteType;
use cairo_lang_sierra::extensions::{args_as_single_type, ConcreteType};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::GenericArg;
use cairo_lang_sierra::program_registry::ProgramRegistry;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::compiler::CompilationError;

/// The number of limbs used to represent a single value in the circuit.
pub const VALUE_SIZE: usize = 4;

/// Describes a circuit in the program.
/// The information about the circuits used in the program.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CircuitsInfo {
    pub circuits: OrderedHashMap<ConcreteTypeId, CircuitInfo>,
}

impl CircuitsInfo {
    pub fn new<'a>(
        registry: &ProgramRegistry<CoreType, CoreLibfunc>,
        type_ids: impl Iterator<Item = &'a ConcreteTypeId>,
    ) -> Result<Self, CompilationError> {
        let mut res = Self::default();
        for ty in type_ids {
            if let CoreTypeConcrete::Circuit(CircuitTypeConcrete::Circuit { .. }) =
                registry.get_type(ty).unwrap()
            {
                res.circuits
                    .entry(ty.clone())
                    .or_insert_with(|| get_circuit_info(registry, ty).unwrap());
            }
        }
        Ok(res)
    }
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
    /// The number of circuit inputs (including the input 1 if needed).
    pub n_inputs: usize,

    /// The circuit requires the input 1 to be present.
    /// we put this 1 as the first value after the inputs.
    pub one_needed: bool,

    /// Maps a concrete type to it's offset in the values array.
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
    /// The circuit requires the input 1 to be present.
    one_needed: bool,
}

/// Parses the circuit inputs and returns `ParsedInputs`.
/// Inputs that feed a addmod gate are require reduction and are fed to a mul gate.
fn parse_circuit_inputs(
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    outputs_tuple: &StructConcreteType,
) -> Result<ParsedInputs, CompilationError> {
    let mut stack = outputs_tuple.members.iter().map(|m| (m.clone(), false)).collect::<Vec<_>>();

    let mut inputs: UnorderedHashMap<usize, (ConcreteTypeId, bool)> = Default::default();
    let mut one_needed = false;

    while let Some((ty, needs_reduction)) = stack.pop() {
        match registry.get_type(&ty).map_err(CompilationError::ProgramRegistryError)? {
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInput(
                ConcreteCircuitInput { info: _info, idx },
            )) => {
                one_needed |= needs_reduction;
                inputs.insert(*idx, (ty, needs_reduction));
            }
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddModGate(gate)) => {
                stack.extend(
                    gate.info()
                        .long_id
                        .generic_args
                        .iter()
                        .map(|garg| (extract_matches!(garg, GenericArg::Type).clone(), true)),
                );
            }
            _ => return Err(CompilationError::UnsupportedCircuitType),
        }
    }

    let mut values: UnorderedHashMap<ConcreteTypeId, usize> = Default::default();
    let n_inputs = inputs.len();

    // The reduced_inputs start at n_inputs + 1 since we need to reserve a slot for the value 1.
    let mut reduced_input_offset = n_inputs + 1;
    let mut mul_offsets = vec![];

    for (input_idx, (ty, needs_reduction)) in inputs.iter_sorted() {
        if *needs_reduction {
            // Add the gate result = 1 * input to reduce the input module the modulus.
            mul_offsets.push(GateOffsets {
                lhs: n_inputs,
                rhs: *input_idx,
                output: reduced_input_offset,
            });
            values.insert(ty.clone(), reduced_input_offset);
            reduced_input_offset += 1;
        } else {
            values.insert(ty.clone(), *input_idx);
        }
    }

    Ok(ParsedInputs { values, mul_offsets, one_needed })
}

/// Gets a concrete type, if it is a const type returns a vector of the values to be stored in the
/// const segment.
fn get_circuit_info(
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    ty: &ConcreteTypeId,
) -> Result<CircuitInfo, CompilationError> {
    let CoreTypeConcrete::Circuit(CircuitTypeConcrete::Circuit(ConcreteCircuit { info })) =
        registry.get_type(ty).map_err(CompilationError::ProgramRegistryError)?
    else {
        return Err(CompilationError::UnsupportedCircuitType);
    };

    let output_ty = args_as_single_type(&info.long_id.generic_args).unwrap();
    let CoreTypeConcrete::Struct(outputs_tuple) =
        registry.get_type(&output_ty).map_err(CompilationError::ProgramRegistryError)?
    else {
        return Err(CompilationError::UnsupportedCircuitType);
    };

    let ParsedInputs { mut values, mul_offsets, one_needed } =
        parse_circuit_inputs(registry, outputs_tuple)?;
    let n_inputs = values.len();
    let mut add_offsets = vec![];

    let mut stack = outputs_tuple.members.iter().map(|m| (m.clone(), false)).collect::<Vec<_>>();

    // We visit each gate in the circuit twice, in the first visit push all its inputs
    // and in the second visit we assume that all the inputs were already visited and we can
    // allocate a value for the outputs and prepare the offsets in the relevant builtin.
    while let Some((ty, first_visit)) = stack.pop() {
        match registry.get_type(&ty).map_err(CompilationError::ProgramRegistryError)? {
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInput(
                ConcreteCircuitInput { .. },
            )) => {}
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddModGate(gate)) => {
                let gate_inputs = gate
                    .info()
                    .long_id
                    .generic_args
                    .iter()
                    .map(|garg| extract_matches!(garg, GenericArg::Type));

                if first_visit {
                    stack.push((ty, true));
                    stack.extend(gate_inputs.map(|ty| (ty.clone(), false)))
                } else {
                    let output_offset = values.len();
                    let mut input_offsets = gate_inputs.map(|ty| *values.get(ty).unwrap());

                    add_offsets.push(GateOffsets {
                        lhs: input_offsets.next().unwrap(),
                        rhs: input_offsets.next().unwrap(),
                        output: output_offset,
                    });

                    values.insert(ty.clone(), output_offset);
                };
            }
            _ => return Err(CompilationError::UnsupportedCircuitType),
        }
    }

    Ok(CircuitInfo { n_inputs, values, add_offsets, mul_offsets, one_needed })
}
