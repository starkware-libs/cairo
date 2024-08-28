use cairo_lang_sierra::extensions::circuit::{CircuitInfo, CircuitTypeConcrete, ConcreteCircuit};
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType, CoreTypeConcrete};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program_registry::ProgramRegistry;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::compiler::CompilationError;

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
            if let CoreTypeConcrete::Circuit(CircuitTypeConcrete::Circuit(ConcreteCircuit {
                circuit_info,
                ..
            })) = registry.get_type(ty).unwrap()
            {
                res.circuits.entry(ty.clone()).or_insert_with(|| circuit_info.clone());
            }
        }
        Ok(res)
    }
}
