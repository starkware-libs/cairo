use crate::{context::Context, graph::*, ref_value::*};
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
}

pub(crate) struct Registry {
    ext_reg: ExtensionRegistry,
    ty_reg: TypeRegistry,
}

impl Registry {
    pub(crate) fn new(prog: &Program) -> Registry {
        Registry {
            ext_reg: get_ext_registry(prog),
            ty_reg: get_type_registry(),
        }
    }

    pub(crate) fn get_type_info(self: &Self, ty: &Type) -> Result<TypeInfo, Error> {
        get_info(&self.ty_reg, ty)
    }

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
}

fn get_info(reg: &TypeRegistry, ty: &Type) -> Result<TypeInfo, Error> {
    match reg.get(&ty.name) {
        None => Err(Error::UnsupportedTypeName),
        Some(e) => e.get_info(&ty.args, reg),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct VarInfo {
    pub ty: Type,
    pub ref_val: RefValue,
}

#[derive(Debug, PartialEq)]
pub(crate) struct PartialStateInfo {
    pub vars: Vec<VarInfo>,
    pub context: Context,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ExtensionSignature {
    pub args: Vec<Type>,
    pub results: Vec<Vec<Type>>,
    pub fallthrough: Option<usize>,
}

fn single_type_arg<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<&'a Type, Error> {
    if tmpl_args.len() != 1 {
        Err(Error::WrongNumberOfTypeArgs)
    } else {
        match &tmpl_args[0] {
            TemplateArg::Type(t) => Ok(t),
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        }
    }
}

fn as_final(ref_val: &RefValue) -> Result<MemLocation, Error> {
    match ref_val {
        RefValue::Final(m) => Ok(*m),
        _ => Err(Error::IllegalArgsLocation),
    }
}

fn simple_invoke_ext_sign(args: Vec<Type>, results: Vec<Type>) -> ExtensionSignature {
    ExtensionSignature {
        args: args,
        results: vec![results],
        fallthrough: Some(0),
    }
}

trait ExtensionImplementation {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error>;

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error>;
}

type ExtensionBox = Box<dyn ExtensionImplementation + Sync + Send>;

type ExtensionRegistry = HashMap<String, ExtensionBox>;

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

fn get_type_registry() -> TypeRegistry {
    chain!(
        arithmetic::types().into_iter(),
        gas_station::types().into_iter(),
        jump_nz::types().into_iter(),
        tuple_obj::types().into_iter(),
    )
    .collect()
}
