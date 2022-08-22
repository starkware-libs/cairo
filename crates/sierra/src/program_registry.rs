use std::collections::hash_map::Entry;
use std::collections::HashMap;

use thiserror::Error;

use crate::extensions::lib_func::Registries;
use crate::extensions::{
    CoreConcrete, CoreLibFunc, CoreType, CoreTypeConcrete, ExtensionError, GenericLibFuncEx,
    GenericTypeEx,
};
use crate::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::{Function, GenericArg, Program, TypeDeclaration};

#[cfg(test)]
#[path = "program_registry_test.rs"]
mod test;

/// Errors encountered in the program registry.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ProgramRegistryError {
    #[error("used the same function id twice")]
    FunctionIdUsedTwice(FunctionId),
    #[error("Could not find the requested function")]
    MissingFunction(FunctionId),
    #[error("Error during type specialization")]
    TypeSpecialization { concrete_id: ConcreteTypeId, error: ExtensionError },
    #[error("Used the same concrete type id twice")]
    TypeConcreteIdUsedTwice(ConcreteTypeId),
    #[error("Declared the same concrete type twice")]
    TypeAlreadyDeclared(TypeDeclaration),
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
    functions: HashMap<FunctionId, Function>,
    concrete_types: HashMap<ConcreteTypeId, CoreTypeConcrete>,
    concrete_libfuncs: HashMap<ConcreteLibFuncId, CoreConcrete>,
}
impl ProgramRegistry {
    /// Create a registry for the program.
    pub fn new(program: &Program) -> Result<ProgramRegistry, ProgramRegistryError> {
        let mut functions = HashMap::<FunctionId, Function>::new();
        for func in &program.funcs {
            match functions.entry(func.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::FunctionIdUsedTwice(func.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(func.clone())),
            }?;
        }
        let mut concrete_types = HashMap::<ConcreteTypeId, CoreTypeConcrete>::new();
        let mut concrete_type_ids =
            HashMap::<(GenericTypeId, &[GenericArg]), ConcreteTypeId>::new();
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
            match concrete_type_ids.entry((declaration.generic_id.clone(), &declaration.args[..])) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::TypeAlreadyDeclared(declaration.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(declaration.id.clone())),
            }?;
        }
        let mut concrete_libfuncs = HashMap::<ConcreteLibFuncId, CoreConcrete>::new();
        for declaration in &program.libfunc_declarations {
            let concrete_libfunc = CoreLibFunc::specialize_by_id(
                Registries { functions: &functions, concrete_type_ids: &concrete_type_ids },
                &declaration.generic_id,
                &declaration.args,
            )
            .map_err(|error| ProgramRegistryError::LibFuncSpecialization {
                concrete_id: declaration.id.clone(),
                error,
            })?;
            match concrete_libfuncs.entry(declaration.id.clone()) {
                Entry::Occupied(_) => {
                    Err(ProgramRegistryError::LibFuncConcreteIdUsedTwice(declaration.id.clone()))
                }
                Entry::Vacant(entry) => Ok(entry.insert(concrete_libfunc)),
            }?;
        }
        Ok(ProgramRegistry { functions, concrete_types, concrete_libfuncs })
    }
    /// Get a function from the given program.
    pub fn get_function<'a>(
        &'a self,
        id: &FunctionId,
    ) -> Result<&'a Function, ProgramRegistryError> {
        self.functions.get(id).ok_or_else(|| ProgramRegistryError::MissingFunction(id.clone()))
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
