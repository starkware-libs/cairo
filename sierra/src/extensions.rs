use crate::{
    context::{Context, Resource},
    graph::*,
    ref_value::*,
};
use std::collections::HashMap;
use Result::*;

mod arithmetic;
mod function_call;
mod gas_station;
mod jump_nz;
mod match_nullable;
mod store;
mod tuple_obj;
mod unconditional_jump;

// Error option while using extensions.
#[derive(Debug, PartialEq)]
pub enum Error {
    UnsupportedLibCallName,
    UnsupportedTypeName,
    ArgumentsMismatch,
    WrongNumberOfTypeArgs,
    UnsupportedTypeArg,
    IllegalArgsLocation,
    IllegalApChangeValue,
    LocalMemoryAlreadyAllocated,
    LocalMemoryCantBeAllocated,
    LocalMemoryNotAllocated,
    LocationsNonCosecutive,
    UnexpectedMemoryStructure,
}

// Registry for finding information on extensions and types.
pub(crate) struct Registry {
    ext_reg: ExtensionRegistry,
    ty_reg: TypeRegistry,
}

impl Registry {
    // Creates a new registry that includes the functions from the program.
    pub(crate) fn new(prog: &Program) -> Registry {
        Registry {
            ext_reg: get_ext_registry(prog),
            ty_reg: get_type_registry(),
        }
    }

    // Get the information on a type.
    pub(crate) fn get_type_info(self: &Self, ty: &Type) -> Result<TypeInfo, Error> {
        get_info(&self.ty_reg, ty)
    }

    // Given a state and an extension returns all the possible states for different possible
    // branches, as well as info on the fallthrough.
    pub(crate) fn transform(
        self: &Self,
        ext: &Extension,
        state: PartialStateInfo,
    ) -> Result<(Vec<PartialStateInfo>, Option<usize>), Error> {
        let e = match self.ext_reg.get(&ext.name) {
            None => Err(Error::UnsupportedLibCallName),
            Some(e) => Ok(e),
        }?;
        let sign = e.get_signature(&ext.tmpl_args)?;
        if state.vars.iter().map(|v| &v.ty).ne(sign.args.iter()) {
            return Err(Error::ArgumentsMismatch);
        }
        let ref_vals = e.mem_change(
            &ext.tmpl_args,
            &self.ty_reg,
            state.context,
            state.vars.into_iter().map(|v| v.ref_val).collect(),
        )?;
        Ok((
            ref_vals
                .into_iter()
                .zip(sign.results.into_iter())
                .map(|((m, refs), tys)| PartialStateInfo {
                    vars: refs
                        .into_iter()
                        .zip(tys.into_iter())
                        .map(|(r, ty)| VarInfo { ty: ty, ref_val: r })
                        .collect(),
                    context: m,
                })
                .collect(),
            sign.fallthrough,
        ))
    }

    pub(crate) fn exec(
        self: &Self,
        ext: &Extension,
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        match self.ext_reg.get(&ext.name) {
            None => Err(Error::UnsupportedLibCallName),
            Some(e) => e.exec(&ext.tmpl_args, &self.ty_reg, inputs),
        }
    }
}

// Helper to get the type infomation from a type and the registry.
fn get_info(reg: &TypeRegistry, ty: &Type) -> Result<TypeInfo, Error> {
    match reg.get(&ty.name) {
        None => Err(Error::UnsupportedTypeName),
        Some(e) => e.get_info(&ty.args, reg),
    }
}

// Class for the information on a variable.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct VarInfo {
    pub ty: Type,
    pub ref_val: RefValue,
}

// The partial state required to know the full state change an extension may do.
#[derive(Debug, PartialEq)]
pub(crate) struct PartialStateInfo {
    pub vars: Vec<VarInfo>,
    pub context: Context,
}

#[derive(Debug, PartialEq)]
struct ExtensionSignature {
    // The types of the extension inputs.
    pub args: Vec<Type>,
    // The types of the extension outputs per possible branch.
    pub results: Vec<Vec<Type>>,
    // The index of the fallthrough branch.
    pub fallthrough: Option<usize>,
}

// Trait for implementing an extension.
trait ExtensionImplementation {
    // Returns the signature given the extensions template arguments.
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error>;

    // Returns the changes in context, and reference values of all return values for all possible branches.
    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error>;

    // Returns the memory representation of the results, given the memory representations of the inputs.
    fn exec(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error>;
}

// Trait for implementing an extension.
trait NonBranchImplementation {
    // Returns the signature given the extensions template arguments.
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error>;

    // Returns the changes in context, and reference values of all return values for all possible branches.
    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error>;

    // Returns the memory representation of the results, given the memory representations of the inputs.
    fn exec(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error>;
}

type ExtensionBox = Box<dyn ExtensionImplementation + Sync + Send>;

type ExtensionRegistry = HashMap<String, ExtensionBox>;

// Creates the registry for the existing extensions.
fn get_ext_registry(prog: &Program) -> ExtensionRegistry {
    chain!(
        arithmetic::extensions().into_iter(),
        function_call::extensions(prog).into_iter(),
        gas_station::extensions().into_iter(),
        jump_nz::extensions().into_iter(),
        match_nullable::extensions().into_iter(),
        store::extensions().into_iter(),
        tuple_obj::extensions().into_iter(),
        unconditional_jump::extensions().into_iter()
    )
    .collect()
}

#[derive(Debug, PartialEq)]
pub(crate) struct TypeInfo {
    pub size: usize,
}

trait TypeInfoImplementation {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
    ) -> Result<TypeInfo, Error>;
}

type TypeInfoBox = Box<dyn TypeInfoImplementation + Sync + Send>;

type TypeRegistry = HashMap<String, TypeInfoBox>;

// Creates the registry for type information.
fn get_type_registry() -> TypeRegistry {
    chain!(
        arithmetic::types().into_iter(),
        gas_station::types().into_iter(),
        jump_nz::types().into_iter(),
        tuple_obj::types().into_iter(),
    )
    .collect()
}

// Utility functions for developing the extensions.

fn unwrap_type<'a>(tmpl_arg: &'a TemplateArg) -> Result<&'a Type, Error> {
    match tmpl_arg {
        TemplateArg::Type(t) => Ok(t),
        TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
    }
}

fn unwrap_value(tmpl_arg: &TemplateArg) -> Result<i64, Error> {
    match tmpl_arg {
        TemplateArg::Value(v) => Ok(*v),
        TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
    }
}

fn validate_size_eq(tmpl_args: &Vec<TemplateArg>, size: usize) -> Result<(), Error> {
    if tmpl_args.len() == size {
        Ok(())
    } else {
        Err(Error::WrongNumberOfTypeArgs)
    }
}

fn single_type_arg<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<&'a Type, Error> {
    validate_size_eq(tmpl_args, 1)?;
    unwrap_type(&tmpl_args[0])
}

fn single_value_arg<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<i64, Error> {
    validate_size_eq(tmpl_args, 1)?;
    unwrap_value(&tmpl_args[0])
}

fn type_value_args<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<(&'a Type, i64), Error> {
    validate_size_eq(tmpl_args, 2)?;
    Ok((unwrap_type(&tmpl_args[0])?, unwrap_value(&tmpl_args[1])?))
}

fn validate_mem_sizes<const N: usize>(
    inputs: &Vec<Vec<i64>>,
    expectation: [usize; N],
) -> Result<(), Error> {
    if inputs.iter().map(|input| input.len()).eq(expectation) {
        Ok(())
    } else {
        Err(Error::UnexpectedMemoryStructure)
    }
}

fn as_final(ref_val: &RefValue) -> Result<MemLocation, Error> {
    match ref_val {
        RefValue::Final(m) => Ok(*m),
        _ => Err(Error::IllegalArgsLocation),
    }
}

fn update_gas(mut ctxt: Context, change: i64) -> Context {
    *ctxt.resources.entry(Resource::Gas).or_insert(0) += change;
    ctxt
}

type NonBranchBox = Box<dyn NonBranchImplementation + Sync + Send>;

struct NonBranchExtension {
    inner: NonBranchBox,
}

impl ExtensionImplementation for NonBranchExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let (args, results) = self.inner.get_signature(tmpl_args)?;
        Ok(ExtensionSignature {
            args: args,
            results: vec![results],
            fallthrough: Some(0),
        })
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        Ok(vec![self
            .inner
            .mem_change(tmpl_args, registry, context, arg_refs)?])
    }

    fn exec(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        Ok((self.inner.exec(tmpl_args, registry, inputs)?, 0))
    }
}

fn wrap_non_branch(nbb: NonBranchBox) -> ExtensionBox {
    Box::new(NonBranchExtension { inner: nbb })
}
