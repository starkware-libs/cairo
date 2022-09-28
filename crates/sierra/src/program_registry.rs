use std::collections::hash_map::Entry;
use std::collections::HashMap;

use thiserror::Error;

use crate::extensions::lib_func::{ConcreteTypeIdMap, FunctionMap, SpecializationContext};
use crate::extensions::types::{TypeInfo, TypeSpecializationContext};
use crate::extensions::{
    ConcreteType, ExtensionError, GenericLibFunc, GenericLibFuncEx, GenericType, GenericTypeEx,
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

type TypeMap<TType> = HashMap<ConcreteTypeId, TType>;
type LibFuncMap<TLibFunc> = HashMap<ConcreteLibFuncId, TLibFunc>;

/// Registry for the data of the compiler, for all program specific data.
pub struct ProgramRegistry<TType: GenericType, TLibFunc: GenericLibFunc> {
    /// Mapping ids to the corresponding user function declaration from the program.
    functions: FunctionMap,
    /// Mapping ids to the concrete types reperesented by them.
    concrete_types: TypeMap<TType::Concrete>,
    /// Mapping ids to the concrete libfuncs reperesented by them.
    concrete_libfuncs: LibFuncMap<TLibFunc::Concrete>,
}
impl<TType: GenericType, TLibFunc: GenericLibFunc> ProgramRegistry<TType, TLibFunc> {
    /// Create a registry for the program.
    pub fn new(
        program: &Program,
    ) -> Result<ProgramRegistry<TType, TLibFunc>, ProgramRegistryError> {
        let functions = get_functions(program)?;
        let (concrete_types, concrete_type_ids) = get_concrete_types_maps::<TType>(program)?;
        let concrete_libfuncs =
            get_concrete_libfuncs::<TLibFunc>(program, &functions, &concrete_type_ids)?;
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
    ) -> Result<&'a TType::Concrete, ProgramRegistryError> {
        self.concrete_types.get(id).ok_or_else(|| ProgramRegistryError::MissingType(id.clone()))
    }
    /// Get a libfunc from the input program.
    pub fn get_libfunc<'a>(
        &'a self,
        id: &ConcreteLibFuncId,
    ) -> Result<&'a TLibFunc::Concrete, ProgramRegistryError> {
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

struct TypeSpecializationContextForRegistry<'a, TType: GenericType> {
    pub concrete_types: &'a TypeMap<TType::Concrete>,
}
impl<TType: GenericType> TypeSpecializationContext
    for TypeSpecializationContextForRegistry<'_, TType>
{
    fn get_type_info(&self, id: &ConcreteTypeId) -> Option<&TypeInfo> {
        self.concrete_types.get(id).map(|ty| ty.info())
    }
}

/// Creates the type-id to concrete type map, and the reverse map from generic-id and arguments to
/// concrete-id.
fn get_concrete_types_maps<TType: GenericType>(
    program: &Program,
) -> Result<(TypeMap<TType::Concrete>, ConcreteTypeIdMap<'_>), ProgramRegistryError> {
    let mut concrete_types = HashMap::new();
    let mut concrete_type_ids = HashMap::<(GenericTypeId, &[GenericArg]), ConcreteTypeId>::new();
    for declaration in &program.type_declarations {
        let concrete_type = TType::specialize_by_id(
            &TypeSpecializationContextForRegistry::<TType> { concrete_types: &concrete_types },
            &declaration.long_id.generic_id,
            &declaration.long_id.generic_args,
        )
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
        match concrete_type_ids
            .entry((declaration.long_id.generic_id.clone(), &declaration.long_id.generic_args[..]))
        {
            Entry::Occupied(_) => {
                Err(ProgramRegistryError::TypeAlreadyDeclared(declaration.clone()))
            }
            Entry::Vacant(entry) => Ok(entry.insert(declaration.id.clone())),
        }?;
    }
    Ok((concrete_types, concrete_type_ids))
}

/// Creates the libfuncs map.
fn get_concrete_libfuncs<TLibFunc: GenericLibFunc>(
    program: &Program,
    functions: &FunctionMap,
    concrete_type_ids: &ConcreteTypeIdMap<'_>,
) -> Result<LibFuncMap<TLibFunc::Concrete>, ProgramRegistryError> {
    let mut concrete_libfuncs = HashMap::new();
    for declaration in &program.libfunc_declarations {
        let concrete_libfunc = TLibFunc::specialize_by_id(
            SpecializationContext { functions, concrete_type_ids },
            &declaration.long_id.generic_id,
            &declaration.long_id.generic_args,
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
