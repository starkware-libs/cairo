use cairo_lang_sierra::extensions::circuit::{
    CircuitConcreteLibfunc, CircuitTypeConcrete, ConcreteCircuitInput,
};
use cairo_lang_sierra::extensions::core::{
    CoreConcreteLibfunc, CoreLibfunc, CoreType, CoreTypeConcrete,
};
use cairo_lang_sierra::extensions::ConcreteType;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId};
use cairo_lang_sierra::program::GenericArg;
use cairo_lang_sierra::program_registry::ProgramRegistry;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::compiler::CompilationError;

/// The information about the circuits used in the program.
#[derive(Default)]
pub struct CircuitsInfo {
    pub circuits: UnorderedHashMap<ConcreteTypeId, CircuitInfo>,
}

impl CircuitsInfo {
    pub fn new<'a>(
        registry: &ProgramRegistry<CoreType, CoreLibfunc>,
        libfunc_ids: impl Iterator<Item = &'a ConcreteLibfuncId>,
    ) -> Result<Self, CompilationError> {
        let mut res = Self::default();
        for libfunc_id in libfunc_ids {
            if let CoreConcreteLibfunc::Circuit(CircuitConcreteLibfunc::InitCircuitData(libfunc)) =
                registry.get_libfunc(libfunc_id).unwrap()
            {
                res.circuits
                    .entry(libfunc.ty.clone())
                    .or_insert_with(|| get_circuit_info(registry, &libfunc.ty).unwrap());
            }
        }
        Ok(res)
    }
}

pub struct CircuitInfo {
    /// The number of circuit inputs.
    pub n_inputs: usize,
    /// The number of values in the circuit.
    pub n_values: usize,
}

/// Gets a concrete type, if it is a const type returns a vector of the values to be stored in the
/// const segment.
fn get_circuit_info(
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
