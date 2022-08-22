use std::collections::hash_map::Entry;
use std::collections::HashMap;

use thiserror::Error;

use crate::extensions::{CoreConcrete, CoreLibcall, ExtensionError, GenericLibcallEx};
use crate::ids::ConcreteLibcallId;
use crate::program::Program;

#[cfg(test)]
#[path = "program_registry_test.rs"]
mod test;

/// Errors encountered in the program registry.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ProgramRegistryError {
    #[error("Error during libcall specialization")]
    LibcallSpecialization { concrete_id: ConcreteLibcallId, error: ExtensionError },
    #[error("Error used the same concrete libcall id twice")]
    LibcallConcreteIdUsedTwice(ConcreteLibcallId),
    #[error("Could not find the requested libcall")]
    MissingLibcall(ConcreteLibcallId),
}

/// Registry for the data of the compiler, for all program specific data.
pub struct ProgramRegistry {
    pub concrete_libcalls: HashMap<ConcreteLibcallId, CoreConcrete>,
}
impl ProgramRegistry {
    /// Create a registry for the program.
    pub fn new(program: &Program) -> Result<ProgramRegistry, ProgramRegistryError> {
        let mut concrete_libcalls = HashMap::<ConcreteLibcallId, CoreConcrete>::new();
        for declaration in &program.libcall_declarations {
            let concrete_libcall =
                CoreLibcall::specialize_by_id(&declaration.generic_id, &declaration.args).map_err(
                    |error| ProgramRegistryError::LibcallSpecialization {
                        concrete_id: declaration.id.clone(),
                        error,
                    },
                )?;
            match concrete_libcalls.entry(declaration.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::LibcallConcreteIdUsedTwice(declaration.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(concrete_libcall)),
            }?;
        }
        Ok(ProgramRegistry { concrete_libcalls })
    }
    /// Get an libcall for a given program.
    pub fn get_libcall<'a>(
        &'a self,
        id: &ConcreteLibcallId,
    ) -> Result<&'a CoreConcrete, ProgramRegistryError> {
        self.concrete_libcalls
            .get(id)
            .ok_or_else(|| ProgramRegistryError::MissingLibcall(id.clone()))
    }
}
