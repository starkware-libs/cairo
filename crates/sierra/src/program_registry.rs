use std::collections::hash_map::Entry;
use std::collections::HashMap;

use thiserror::Error;

use crate::extensions::{
    ConcreteTypeInfo, CoreConcrete, CoreExtension, CoreType, ExtensionError, GenericExtensionEx,
    GenericTypeEx,
};
use crate::ids::{ConcreteExtensionId, ConcreteTypeId};
use crate::program::Program;

#[cfg(test)]
#[path = "program_registry_test.rs"]
mod tests;

/// Errors encountered in the program registry.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ProgramRegistryError {
    #[error("Error during type specialization")]
    TypeSpecialization { concrete_id: ConcreteTypeId, error: ExtensionError },
    #[error("Error during extension specialization")]
    ExtensionSpecialization { concrete_id: ConcreteExtensionId, error: ExtensionError },
    #[error("Error used the same concrete type id twice")]
    TypeConcreteIdUsedTwice(ConcreteTypeId),
    #[error("Error used the same concrete extension id twice")]
    ExtensionConcreteIdUsedTwice(ConcreteExtensionId),
    #[error("Could not find the extension")]
    MissingExtension(ConcreteExtensionId),
    #[error("Could not find the type")]
    MissingType(ConcreteTypeId),
}

/// Registry for the data of the compiler, for all program specific data.
pub struct ProgramRegistry {
    concrete_extensions: HashMap<ConcreteExtensionId, CoreConcrete>,
    concrete_type_infos: HashMap<ConcreteTypeId, ConcreteTypeInfo>,
}
impl ProgramRegistry {
    /// Create a registry for the program.
    pub fn new(program: &Program) -> Result<ProgramRegistry, ProgramRegistryError> {
        let mut concrete_type_infos = HashMap::<ConcreteTypeId, ConcreteTypeInfo>::new();
        for declaration in &program.type_declarations {
            let type_info =
                CoreType::get_concrete_info_by_id(&declaration.generic_id, &declaration.args)
                    .map_err(|error| ProgramRegistryError::TypeSpecialization {
                        concrete_id: declaration.id.clone(),
                        error,
                    })?;
            match concrete_type_infos.entry(declaration.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::TypeConcreteIdUsedTwice(declaration.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(type_info)),
            }?;
        }
        let mut concrete_extensions = HashMap::<ConcreteExtensionId, CoreConcrete>::new();
        for declaration in &program.extension_declarations {
            let concrete_extension =
                CoreExtension::specialize_by_id(&declaration.generic_id, &declaration.args)
                    .map_err(|error| ProgramRegistryError::ExtensionSpecialization {
                        concrete_id: declaration.id.clone(),
                        error,
                    })?;
            match concrete_extensions.entry(declaration.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::ExtensionConcreteIdUsedTwice(declaration.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(concrete_extension)),
            }?;
        }
        Ok(ProgramRegistry { concrete_type_infos, concrete_extensions })
    }
    /// Get an extension for a given program.
    pub fn get_extension<'a>(
        &'a self,
        id: &ConcreteExtensionId,
    ) -> Result<&'a CoreConcrete, ProgramRegistryError> {
        self.concrete_extensions
            .get(id)
            .ok_or_else(|| ProgramRegistryError::MissingExtension(id.clone()))
    }
    /// Get an extension for a given program.
    pub fn get_type_info<'a>(
        &'a self,
        id: &ConcreteTypeId,
    ) -> Result<&'a ConcreteTypeInfo, ProgramRegistryError> {
        self.concrete_type_infos
            .get(id)
            .ok_or_else(|| ProgramRegistryError::MissingType(id.clone()))
    }
}
