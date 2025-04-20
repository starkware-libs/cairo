use std::collections::HashMap;
use std::collections::hash_map::Entry;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, izip};
use thiserror::Error;

use crate::extensions::lib_func::{
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteLibfunc, ConcreteType, ExtensionError, GenericLibfunc, GenericLibfuncEx, GenericType,
    GenericTypeEx,
};
use crate::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::{
    BranchTarget, DeclaredTypeInfo, Function, FunctionSignature, GenericArg, Program, Statement,
    StatementIdx, TypeDeclaration,
};

#[cfg(test)]
#[path = "program_registry_test.rs"]
mod test;

/// Errors encountered in the program registry.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ProgramRegistryError {
    #[error("used the same function id twice `{0}`.")]
    FunctionIdAlreadyExists(FunctionId),
    #[error("Could not find the requested function `{0}`.")]
    MissingFunction(FunctionId),
    #[error("Error during type specialization of `{concrete_id}`: {error}")]
    TypeSpecialization { concrete_id: ConcreteTypeId, error: ExtensionError },
    #[error("Used concrete type id `{0}` twice")]
    TypeConcreteIdAlreadyExists(ConcreteTypeId),
    #[error("Declared concrete type `{0}` twice")]
    TypeAlreadyDeclared(Box<TypeDeclaration>),
    #[error("Could not find requested type `{0}`.")]
    MissingType(ConcreteTypeId),
    #[error("Error during libfunc specialization of {concrete_id}: {error}")]
    LibfuncSpecialization { concrete_id: ConcreteLibfuncId, error: ExtensionError },
    #[error("Used concrete libfunc id `{0}` twice.")]
    LibfuncConcreteIdAlreadyExists(ConcreteLibfuncId),
    #[error("Could not find requested libfunc `{0}`.")]
    MissingLibfunc(ConcreteLibfuncId),
    #[error("Type info declaration mismatch for `{0}`.")]
    TypeInfoDeclarationMismatch(ConcreteTypeId),
    #[error("Function `{func_id}`'s parameter type `{ty}` is not storable.")]
    FunctionWithUnstorableType { func_id: FunctionId, ty: ConcreteTypeId },
    #[error("Function `{0}` points to non existing entry point statement.")]
    FunctionNonExistingEntryPoint(FunctionId),
    #[error("#{0}: Libfunc invocation input count mismatch")]
    LibfuncInvocationInputCountMismatch(StatementIdx),
    #[error("#{0}: Libfunc invocation branch count mismatch")]
    LibfuncInvocationBranchCountMismatch(StatementIdx),
    #[error("#{0}: Libfunc invocation branch #{1} result count mismatch")]
    LibfuncInvocationBranchResultCountMismatch(StatementIdx, usize),
    #[error("#{0}: Libfunc invocation branch #{1} target mismatch")]
    LibfuncInvocationBranchTargetMismatch(StatementIdx, usize),
    #[error("#{src}: Branch jump backwards to {dst}")]
    BranchBackwards { src: StatementIdx, dst: StatementIdx },
    #[error("#{src}: Branch jump to a non-branch align statement #{dst}")]
    BranchNotToBranchAlign { src: StatementIdx, dst: StatementIdx },
    #[error("#{src1}, #{src2}: Jump to the same statement #{dst}")]
    MultipleJumpsToSameStatement { src1: StatementIdx, src2: StatementIdx, dst: StatementIdx },
    #[error("#{0}: Jump out of range")]
    JumpOutOfRange(StatementIdx),
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
        registry.validate(program)?;
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

    /// Checks the validity of the [ProgramRegistry] and runs validations on the program.
    ///
    /// Later compilation stages may perform more validations as well as repeat these validations.
    fn validate(&self, program: &Program) -> Result<(), Box<ProgramRegistryError>> {
        // Check that all the parameter and return types are storable.
        for func in self.functions.values() {
            for ty in chain!(func.signature.param_types.iter(), func.signature.ret_types.iter()) {
                if !self.get_type(ty)?.info().storable {
                    return Err(Box::new(ProgramRegistryError::FunctionWithUnstorableType {
                        func_id: func.id.clone(),
                        ty: ty.clone(),
                    }));
                }
            }
            if func.entry_point.0 >= program.statements.len() {
                return Err(Box::new(ProgramRegistryError::FunctionNonExistingEntryPoint(
                    func.id.clone(),
                )));
            }
        }
        // A branch map, mapping from a destination statement to the statement that jumps to it.
        // A branch is considered a branch only if it has more than one target.
        // Assuming branches into branch alignments only, this should be a bijection.
        let mut branches: HashMap<StatementIdx, StatementIdx> =
            HashMap::<StatementIdx, StatementIdx>::default();
        for (i, statement) in program.statements.iter().enumerate() {
            self.validate_statement(program, StatementIdx(i), statement, &mut branches)?;
        }
        Ok(())
    }

    /// Checks the validity of a statement.
    fn validate_statement(
        &self,
        program: &Program,
        index: StatementIdx,
        statement: &Statement,
        branches: &mut HashMap<StatementIdx, StatementIdx>,
    ) -> Result<(), Box<ProgramRegistryError>> {
        let Statement::Invocation(invocation) = statement else {
            return Ok(());
        };
        let libfunc = self.get_libfunc(&invocation.libfunc_id)?;
        if invocation.args.len() != libfunc.param_signatures().len() {
            return Err(Box::new(ProgramRegistryError::LibfuncInvocationInputCountMismatch(index)));
        }
        let libfunc_branches = libfunc.branch_signatures();
        if invocation.branches.len() != libfunc_branches.len() {
            return Err(Box::new(ProgramRegistryError::LibfuncInvocationBranchCountMismatch(
                index,
            )));
        }
        let libfunc_fallthrough = libfunc.fallthrough();
        for (branch_index, (invocation_branch, libfunc_branch)) in
            izip!(&invocation.branches, libfunc_branches).enumerate()
        {
            if invocation_branch.results.len() != libfunc_branch.vars.len() {
                return Err(Box::new(
                    ProgramRegistryError::LibfuncInvocationBranchResultCountMismatch(
                        index,
                        branch_index,
                    ),
                ));
            }
            if matches!(libfunc_fallthrough, Some(target) if target == branch_index)
                != (invocation_branch.target == BranchTarget::Fallthrough)
            {
                return Err(Box::new(ProgramRegistryError::LibfuncInvocationBranchTargetMismatch(
                    index,
                    branch_index,
                )));
            }
            if !matches!(libfunc_branch.ap_change, SierraApChange::BranchAlign) {
                if let Some(prev) = branches.get(&index) {
                    return Err(Box::new(ProgramRegistryError::BranchNotToBranchAlign {
                        src: *prev,
                        dst: index,
                    }));
                }
            }
            let next = index.next(&invocation_branch.target);
            if next.0 >= program.statements.len() {
                return Err(Box::new(ProgramRegistryError::JumpOutOfRange(index)));
            }
            if libfunc_branches.len() > 1 {
                if next.0 < index.0 {
                    return Err(Box::new(ProgramRegistryError::BranchBackwards {
                        src: index,
                        dst: next,
                    }));
                }
                match branches.entry(next) {
                    Entry::Occupied(e) => {
                        return Err(Box::new(ProgramRegistryError::MultipleJumpsToSameStatement {
                            src1: *e.get(),
                            src2: index,
                            dst: next,
                        }));
                    }
                    Entry::Vacant(e) => {
                        e.insert(index);
                    }
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
