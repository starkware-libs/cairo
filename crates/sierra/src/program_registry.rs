use std::collections::hash_map::Entry;
use std::collections::HashMap;

use thiserror::Error;

use crate::extensions::{
    CoreConcrete, CoreLibFunc, CoreType, CoreTypeConcrete, ExtensionError, GenericLibFuncEx,
    GenericTypeEx,
};
use crate::ids::{ConcreteLibFuncId, ConcreteTypeId};
use crate::program::Program;

#[cfg(test)]
#[path = "program_registry_test.rs"]
mod test;

/// Errors encountered in the program registry.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ProgramRegistryError {
    #[error("Error during type specialization")]
    TypeSpecialization { concrete_id: ConcreteTypeId, error: ExtensionError },
    #[error("Used the same concrete type id twice")]
    TypeConcreteIdUsedTwice(ConcreteTypeId),
    #[error("Could not find the requested type")]
    MissingType(ConcreteTypeId),
    #[error("Error during libfunc specialization")]
    LibFuncSpecialization { concrete_id: ConcreteLibFuncId, error: ExtensionError },
    #[error("Used the same concrete libfunc id twice")]
    LibFuncConcreteIdUsedTwice(ConcreteLibFuncId),
    #[error("Could not find the requested libfunc")]
    MissingLibFunc(ConcreteLibFuncId),
}

/// Registry for the data of the compiler, for all program specific data.
pub struct ProgramRegistry {
    concrete_types: HashMap<ConcreteTypeId, CoreTypeConcrete>,
    concrete_libfuncs: HashMap<ConcreteLibFuncId, CoreConcrete>,
}
impl ProgramRegistry {
    /// Create a registry for the program.
    pub fn new(program: &Program) -> Result<ProgramRegistry, ProgramRegistryError> {
        let mut concrete_types = HashMap::<ConcreteTypeId, CoreTypeConcrete>::new();
        for declaration in &program.type_declarations {
            let concrete_type =
                CoreType::specialize_by_id(&declaration.generic_id, &declaration.args).map_err(
                    |error| ProgramRegistryError::TypeSpecialization {
                        concrete_id: declaration.id.clone(),
                        error,
                    },
                )?;
            match concrete_types.entry(declaration.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::TypeConcreteIdUsedTwice(declaration.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(concrete_type)),
            }?;
        }
        let mut concrete_libfuncs = HashMap::<ConcreteLibFuncId, CoreConcrete>::new();
        for declaration in &program.libfunc_declarations {
            let concrete_libfunc =
                CoreLibFunc::specialize_by_id(&declaration.generic_id, &declaration.args).map_err(
                    |error| ProgramRegistryError::LibFuncSpecialization {
                        concrete_id: declaration.id.clone(),
                        error,
                    },
                )?;
            match concrete_libfuncs.entry(declaration.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::LibFuncConcreteIdUsedTwice(declaration.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(concrete_libfunc)),
            }?;
        }
        Ok(ProgramRegistry { concrete_types, concrete_libfuncs })
    }
    /// Get a type from the given program.
    pub fn get_type<'a>(
        &'a self,
        id: &ConcreteTypeId,
    ) -> Result<&'a CoreTypeConcrete, ProgramRegistryError> {
        self.concrete_types.get(id).ok_or_else(|| ProgramRegistryError::MissingType(id.clone()))
    }
    /// Get a libfunc from the given program.
    pub fn get_libfunc<'a>(
        &'a self,
        id: &ConcreteLibFuncId,
    ) -> Result<&'a CoreConcrete, ProgramRegistryError> {
        self.concrete_libfuncs
            .get(id)
            .ok_or_else(|| ProgramRegistryError::MissingLibFunc(id.clone()))
    }
}
