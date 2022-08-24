use std::collections::hash_map::Entry;
use std::collections::HashMap;

use thiserror::Error;

use crate::extensions::lib_func::{ConcreteTypeIdMap, FunctionMap, SpecializationContext};
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
    FunctionIdAlreadyExists(FunctionId),
    #[error("Could not find the requested function")]
    MissingFunction(FunctionId),
    #[error("Error during type specialization")]
    TypeSpecialization { concrete_id: ConcreteTypeId, error: ExtensionError },
    #[error("Used the same concrete type id twice")]
    TypeConcreteIdAlreadyExists(ConcreteTypeId),
    #[error("Declared the same concrete type twice")]
    TypeAlreadyDeclared(TypeDeclaration),
    #[error("Could not find the requested type")]
    MissingType(ConcreteTypeId),
    #[error("Error during libfunc specialization")]
    LibFuncSpecialization { concrete_id: ConcreteLibFuncId, error: ExtensionError },
    #[error("Used the same concrete libfunc id twice")]
    LibFuncConcreteIdAlreadyExists(ConcreteLibFuncId),
    #[error("Could not find the requested libfunc")]
    MissingLibFunc(ConcreteLibFuncId),
}

type TypeMap = HashMap<ConcreteTypeId, CoreTypeConcrete>;
type LibFuncMap = HashMap<ConcreteLibFuncId, CoreConcrete>;

/// Registry for the data of the compiler, for all program specific data.
pub struct ProgramRegistry {
    /// Mapping ids to the corresponding user function declaration from the program.
    functions: FunctionMap,
    /// Mapping ids to the concrete types reperesented by them.
    concrete_types: TypeMap,
    /// Mapping ids to the concrete libfuncs reperesented by them.
    concrete_libfuncs: LibFuncMap,
}
impl ProgramRegistry {
    /// Create a registry for the program.
    pub fn new(program: &Program) -> Result<ProgramRegistry, ProgramRegistryError> {
        let functions = get_functions(program)?;
        let (concrete_types, concrete_type_ids) = get_concrete_types_maps(program)?;
        let concrete_libfuncs = get_concrete_libfuncs(program, &functions, &concrete_type_ids)?;
        Ok(ProgramRegistry { functions, concrete_types, concrete_libfuncs })
    }
    /// Get a function from the input program.
    pub fn get_function<'a>(
        &'a self,
        id: &FunctionId,
    ) -> Result<&'a Function, ProgramRegistryError> {
        self.functions.get(id).ok_or_else(|| ProgramRegistryError::MissingFunction(id.clone()))
    }
    /// Get a type from the input program.
    pub fn get_type<'a>(
        &'a self,
        id: &ConcreteTypeId,
    ) -> Result<&'a CoreTypeConcrete, ProgramRegistryError> {
        self.concrete_types.get(id).ok_or_else(|| ProgramRegistryError::MissingType(id.clone()))
    }
    /// Get a libfunc from the input program.
    pub fn get_libfunc<'a>(
        &'a self,
        id: &ConcreteLibFuncId,
    ) -> Result<&'a CoreConcrete, ProgramRegistryError> {
        self.concrete_libfuncs
            .get(id)
            .ok_or_else(|| ProgramRegistryError::MissingLibFunc(id.clone()))
    }
}

/// Creates the functions map.
fn get_functions(program: &Program) -> Result<FunctionMap, ProgramRegistryError> {
    let mut functions = FunctionMap::new();
    for func in &program.funcs {
        match functions.entry(func.id.clone()) {
            Entry::Occupied(_) => {
                Err(ProgramRegistryError::FunctionIdAlreadyExists(func.id.clone()))
            }
            Entry::Vacant(entry) => Ok(entry.insert(func.clone())),
        }?;
    }
    Ok(functions)
}

/// Creates the type-id to concrete type map, and the reverse map from generic-id and arguments to
/// concrete-id.
fn get_concrete_types_maps(
    program: &Program,
) -> Result<(TypeMap, ConcreteTypeIdMap<'_>), ProgramRegistryError> {
    let mut concrete_types = HashMap::new();
    let mut concrete_type_ids = HashMap::<(GenericTypeId, &[GenericArg]), ConcreteTypeId>::new();
    for declaration in &program.type_declarations {
        let concrete_type = CoreType::specialize_by_id(&declaration.generic_id, &declaration.args)
            .map_err(|error| ProgramRegistryError::TypeSpecialization {
                concrete_id: declaration.id.clone(),
                error,
            })?;
        match concrete_types.entry(declaration.id.clone()) {
            Entry::Occupied(_) => {
                Err(ProgramRegistryError::TypeConcreteIdAlreadyExists(declaration.id.clone()))
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
    Ok((concrete_types, concrete_type_ids))
}

/// Creates the libfuncs map.
fn get_concrete_libfuncs(
    program: &Program,
    functions: &FunctionMap,
    concrete_type_ids: &ConcreteTypeIdMap<'_>,
) -> Result<LibFuncMap, ProgramRegistryError> {
    let mut concrete_libfuncs = HashMap::new();
    for declaration in &program.libfunc_declarations {
        let concrete_libfunc = CoreLibFunc::specialize_by_id(
            SpecializationContext { functions, concrete_type_ids },
            &declaration.generic_id,
            &declaration.args,
        )
        .map_err(|error| ProgramRegistryError::LibFuncSpecialization {
            concrete_id: declaration.id.clone(),
            error,
        })?;
        match concrete_libfuncs.entry(declaration.id.clone()) {
            Entry::Occupied(_) => {
                Err(ProgramRegistryError::LibFuncConcreteIdAlreadyExists(declaration.id.clone()))
            }
            Entry::Vacant(entry) => Ok(entry.insert(concrete_libfunc)),
        }?;
    }
    Ok(concrete_libfuncs)
}
