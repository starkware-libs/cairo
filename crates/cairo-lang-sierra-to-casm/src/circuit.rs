use std::collections::HashMap;

use cairo_lang_sierra::extensions::circuit::{
    CircuitConcreteLibfunc, CircuitTypeConcrete, ConcreteCircuitInput,
};
use cairo_lang_sierra::extensions::core::{
    CoreConcreteLibfunc, CoreLibfunc, CoreType, CoreTypeConcrete,
};
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId};
use cairo_lang_sierra::program_registry::ProgramRegistry;

use crate::compiler::CompilationError;

#[derive(Default)]
pub struct CircuitInfos {
    pub infos: HashMap<ConcreteTypeId, CircuitInfo>,
}

impl CircuitInfos {
    pub fn new<'a>(
        registry: &ProgramRegistry<CoreType, CoreLibfunc>,
        libfunc_ids: impl Iterator<Item = &'a ConcreteLibfuncId>,
    ) -> Self {
        let mut infos = HashMap::new();
        for id in libfunc_ids {
            if let CoreConcreteLibfunc::Circuit(CircuitConcreteLibfunc::InitCircuitData(libfunc)) =
                registry.get_libfunc(id).unwrap()
            {
                infos
                    .entry(libfunc.ty.clone())
                    .or_insert_with_key(|ty| get_circuit_info(registry, ty).unwrap());
            }
        }
        Self { infos }
    }

    pub fn get(&self, ty: &ConcreteTypeId) -> &CircuitInfo {
        self.infos.get(ty).unwrap()
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

    let mut max_input_idx = 0;
    for output in &outputs_tuple.members {
        let CoreTypeConcrete::Circuit(CircuitTypeConcrete::CircuitInput(ConcreteCircuitInput {
            info: _info,
            idx,
        })) = registry.get_type(output).map_err(CompilationError::ProgramRegistryError)?
        else {
            return Err(CompilationError::UnsupportedCircuitType);
        };
        max_input_idx = max_input_idx.max(*idx);
    }

    Ok(CircuitInfo { n_inputs: max_input_idx, n_values: max_input_idx })
}
