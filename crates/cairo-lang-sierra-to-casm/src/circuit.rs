use cairo_lang_sierra::extensions::circuit::{CircuitTypeConcrete, ConcreteCircuitInput};
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType, CoreTypeConcrete};
use cairo_lang_sierra::extensions::ConcreteType;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::GenericArg;
use cairo_lang_sierra::program_registry::ProgramRegistry;
use cairo_lang_utils::extract_matches;

use crate::compiler::CompilationError;

pub struct CircuitInfo {
    /// The number of circuit inputs.
    pub n_inputs: usize,
    /// The number of values in the circuit.
    pub n_values: usize,
}

/// Gets a concrete type, if it is a const type returns a vector of the values to be stored in the
/// const segment.
pub fn get_circuit_info(
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    ty: &ConcreteTypeId,
) -> Result<CircuitInfo, CompilationError> {
    let CoreTypeConcrete::Struct(outputs_tuple) =
        registry.get_type(ty).map_err(CompilationError::ProgramRegistryError)?
    else {
        return Err(CompilationError::UnsupportedCircuitType);
    };

    let mut stack = outputs_tuple.members.clone();
    let mut max_input_idx = 0;
    let mut n_gates = 0;

    while let Some(ty) = stack.pop() {
        match registry.get_type(&ty).map_err(CompilationError::ProgramRegistryError)? {
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInput(
                ConcreteCircuitInput { info: _info, idx },
            )) => {
                max_input_idx = max_input_idx.max(*idx);
            }
            CoreTypeConcrete::Circuit(CircuitTypeConcrete::AddModGate(gate)) => {
                n_gates += 1;
                stack.extend(
                    gate.info()
                        .long_id
                        .generic_args
                        .iter()
                        .map(|garg| extract_matches!(garg, GenericArg::Type))
                        .cloned(),
                )
            }
            _ => return Err(CompilationError::UnsupportedCircuitType),
        }
    }

    Ok(CircuitInfo { n_inputs: max_input_idx, n_values: max_input_idx + n_gates })
}
