use std::collections::hash_map::Entry;
use std::collections::HashMap;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::chain;
use thiserror::Error;

use crate::extensions::lib_func::{
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, ExtensionError, GenericLibfunc, GenericLibfuncEx, GenericType, GenericTypeEx,
};
use crate::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::{
    DeclaredTypeInfo, Function, FunctionSignature, GenericArg, Program, TypeDeclaration,
};

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
    TypeAlreadyDeclared(Box<TypeDeclaration>),
    #[error("Could not find the requested type")]
    MissingType(ConcreteTypeId),
    #[error("Error during libfunc specialization")]
    LibfuncSpecialization { concrete_id: ConcreteLibfuncId, error: ExtensionError },
    #[error("Used the same concrete libfunc id twice")]
    LibfuncConcreteIdAlreadyExists(ConcreteLibfuncId),
    #[error("Could not find the requested libfunc")]
    MissingLibfunc(ConcreteLibfuncId),
    #[error("Type info declaration mismatch")]
    TypeInfoDeclarationMismatch(ConcreteTypeId),
    #[error("Function parameter type must be storable")]
    FunctionWithUnstorableType { func_id: FunctionId, ty: ConcreteTypeId },
}

type TypeMap<TType> = HashMap<ConcreteTypeId, TType>;
type LibfuncMap<TLibfunc> = HashMap<ConcreteLibfuncId, TLibfunc>;
type FunctionMap = HashMap<FunctionId, Function>;
/// Mapping from the arguments for generating a concrete type (the generic-id and the arguments) to
/// the concrete-id that points to it.
type ConcreteTypeIdMap<'a> = HashMap<(GenericTypeId, &'a [GenericArg]), ConcreteTypeId>;

/// Registry for the data of the compiler, for all program specific data.
pub struct ProgramRegistry<TType: GenericType, TLibfunc: GenericLibfunc> {
    /// Mapping ids to the corresponding user function declaration from the program.
    functions: FunctionMap,
    /// Mapping ids to the concrete types represented by them.
    concrete_types: TypeMap<TType::Concrete>,
    /// Mapping ids to the concrete libfuncs represented by them.
    concrete_libfuncs: LibfuncMap<TLibfunc::Concrete>,
}
impl<TType: GenericType, TLibfunc: GenericLibfunc> ProgramRegistry<TType, TLibfunc> {
    /// Create a registry for the program.
    pub fn new_with_ap_change(
        program: &Program,
        function_ap_change: OrderedHashMap<FunctionId, usize>,
    ) -> Result<ProgramRegistry<TType, TLibfunc>, Box<ProgramRegistryError>> {
        let functions = get_functions(program)?;
        let (concrete_types, concrete_type_ids) = get_concrete_types_maps::<TType>(program)?;
        let concrete_libfuncs = get_concrete_libfuncs::<TType, TLibfunc>(
            program,
            &SpecializationContextForRegistry {
                functions: &functions,
                concrete_type_ids: &concrete_type_ids,
                concrete_types: &concrete_types,
                function_ap_change,
            },
        )?;
        let registry = ProgramRegistry { functions, concrete_types, concrete_libfuncs };
        registry.validate()?;
        Ok(registry)
    }

    pub fn new(
        program: &Program,
    ) -> Result<ProgramRegistry<TType, TLibfunc>, Box<ProgramRegistryError>> {
        Self::new_with_ap_change(program, Default::default())
    }
    /// Gets a function from the input program.
    pub fn get_function<'a>(
        &'a self,
        id: &FunctionId,
    ) -> Result<&'a Function, Box<ProgramRegistryError>> {
        self.functions
            .get(id)
            .ok_or_else(|| Box::new(ProgramRegistryError::MissingFunction(id.clone())))
    }
    /// Gets a type from the input program.
    pub fn get_type<'a>(
        &'a self,
        id: &ConcreteTypeId,
    ) -> Result<&'a TType::Concrete, Box<ProgramRegistryError>> {
        self.concrete_types
            .get(id)
            .ok_or_else(|| Box::new(ProgramRegistryError::MissingType(id.clone())))
    }
    /// Gets a libfunc from the input program.
    pub fn get_libfunc<'a>(
        &'a self,
        id: &ConcreteLibfuncId,
    ) -> Result<&'a TLibfunc::Concrete, Box<ProgramRegistryError>> {
        self.concrete_libfuncs
            .get(id)
            .ok_or_else(|| Box::new(ProgramRegistryError::MissingLibfunc(id.clone())))
    }

    /// Checks the validity of the [ProgramRegistry].
    ///
    /// Checks that all the parameter and return types are storable.
    fn validate(&self) -> Result<(), Box<ProgramRegistryError>> {
        for func in self.functions.values() {
            for ty in chain!(func.signature.param_types.iter(), func.signature.ret_types.iter()) {
                if !self.get_type(ty)?.info().storable {
                    return Err(Box::new(ProgramRegistryError::FunctionWithUnstorableType {
                        func_id: func.id.clone(),
                        ty: ty.clone(),
                    }));
                }
            }
        }
        Ok(())
    }
}

/// Creates the functions map.
fn get_functions(program: &Program) -> Result<FunctionMap, Box<ProgramRegistryError>> {
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
    pub declared_type_info: &'a TypeMap<TypeInfo>,
}
impl<TType: GenericType> TypeSpecializationContext
    for TypeSpecializationContextForRegistry<'_, TType>
{
    fn try_get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        self.declared_type_info
            .get(&id)
            .or_else(|| self.concrete_types.get(&id).map(|ty| ty.info()))
            .cloned()
    }
}

/// Creates the type-id to concrete type map, and the reverse map from generic-id and arguments to
/// concrete-id.
fn get_concrete_types_maps<TType: GenericType>(
    program: &Program,
) -> Result<(TypeMap<TType::Concrete>, ConcreteTypeIdMap<'_>), Box<ProgramRegistryError>> {
    let mut concrete_types = HashMap::new();
    let mut concrete_type_ids = HashMap::<(GenericTypeId, &[GenericArg]), ConcreteTypeId>::new();
    let declared_type_info = program
        .type_declarations
        .iter()
        .filter_map(|declaration| {
            let TypeDeclaration { id, long_id, declared_type_info } = declaration;
            let DeclaredTypeInfo { storable, droppable, duplicatable, zero_sized } =
                declared_type_info.as_ref().cloned()?;
            Some((
                id.clone(),
                TypeInfo {
                    long_id: long_id.clone(),
                    storable,
                    droppable,
                    duplicatable,
                    zero_sized,
                },
            ))
        })
        .collect();
    for declaration in &program.type_declarations {
        let concrete_type = TType::specialize_by_id(
            &TypeSpecializationContextForRegistry::<TType> {
                concrete_types: &concrete_types,
                declared_type_info: &declared_type_info,
            },
            &declaration.long_id.generic_id,
            &declaration.long_id.generic_args,
        )
        .map_err(|error| {
            Box::new(ProgramRegistryError::TypeSpecialization {
                concrete_id: declaration.id.clone(),
                error,
            })
        })?;
        // Check that the info is consistent with declaration.
        if let Some(declared_info) = declared_type_info.get(&declaration.id) {
            if concrete_type.info() != declared_info {
                return Err(Box::new(ProgramRegistryError::TypeInfoDeclarationMismatch(
                    declaration.id.clone(),
                )));
            }
        }

        match concrete_types.entry(declaration.id.clone()) {
            Entry::Occupied(_) => Err(Box::new(ProgramRegistryError::TypeConcreteIdAlreadyExists(
                declaration.id.clone(),
            ))),
            Entry::Vacant(entry) => Ok(entry.insert(concrete_type)),
        }?;
        match concrete_type_ids
            .entry((declaration.long_id.generic_id.clone(), &declaration.long_id.generic_args[..]))
        {
            Entry::Occupied(_) => Err(Box::new(ProgramRegistryError::TypeAlreadyDeclared(
                Box::new(declaration.clone()),
            ))),
            Entry::Vacant(entry) => Ok(entry.insert(declaration.id.clone())),
        }?;
    }
    Ok((concrete_types, concrete_type_ids))
}

/// Context required for specialization process.
pub struct SpecializationContextForRegistry<'a, TType: GenericType> {
    pub functions: &'a FunctionMap,
    pub concrete_type_ids: &'a ConcreteTypeIdMap<'a>,
    pub concrete_types: &'a TypeMap<TType::Concrete>,
    /// AP changes information for Sierra user functions.
    pub function_ap_change: OrderedHashMap<FunctionId, usize>,
}
impl<TType: GenericType> TypeSpecializationContext for SpecializationContextForRegistry<'_, TType> {
    fn try_get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        self.concrete_types.get(&id).map(|ty| ty.info().clone())
    }
}
impl<TType: GenericType> SignatureSpecializationContext
    for SpecializationContextForRegistry<'_, TType>
{
    fn try_get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Option<ConcreteTypeId> {
        self.concrete_type_ids.get(&(id, generic_args)).cloned()
    }

    fn try_get_function_signature(&self, function_id: &FunctionId) -> Option<FunctionSignature> {
        self.try_get_function(function_id).map(|f| f.signature)
    }

    fn as_type_specialization_context(&self) -> &dyn TypeSpecializationContext {
        self
    }

    fn try_get_function_ap_change(&self, function_id: &FunctionId) -> Option<SierraApChange> {
        Some(if self.function_ap_change.contains_key(function_id) {
            SierraApChange::Known { new_vars_only: false }
        } else {
            SierraApChange::Unknown
        })
    }
}
impl<TType: GenericType> SpecializationContext for SpecializationContextForRegistry<'_, TType> {
    fn try_get_function(&self, function_id: &FunctionId) -> Option<Function> {
        self.functions.get(function_id).cloned()
    }

    fn upcast(&self) -> &dyn SignatureSpecializationContext {
        self
    }
}

/// Creates the libfuncs map.
fn get_concrete_libfuncs<TType: GenericType, TLibfunc: GenericLibfunc>(
    program: &Program,
    context: &SpecializationContextForRegistry<'_, TType>,
) -> Result<LibfuncMap<TLibfunc::Concrete>, Box<ProgramRegistryError>> {
    let mut concrete_libfuncs = HashMap::new();
    for declaration in &program.libfunc_declarations {
        let concrete_libfunc = TLibfunc::specialize_by_id(
            context,
            &declaration.long_id.generic_id,
            &declaration.long_id.generic_args,
        )
        .map_err(|error| ProgramRegistryError::LibfuncSpecialization {
            concrete_id: declaration.id.clone(),
            error,
        })?;
        match concrete_libfuncs.entry(declaration.id.clone()) {
            Entry::Occupied(_) => {
                Err(ProgramRegistryError::LibfuncConcreteIdAlreadyExists(declaration.id.clone()))
            }
            Entry::Vacant(entry) => Ok(entry.insert(concrete_libfunc)),
        }?;
    }
    Ok(concrete_libfuncs)
}
