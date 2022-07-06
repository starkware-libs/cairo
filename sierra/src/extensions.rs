use crate::error::Error;
use crate::graph::*;
use std::collections::HashMap;
use Result::*;

mod arithmetic;
mod gas_station;
mod jump_nz;
mod match_nullable;
mod unconditional_jump;

#[derive(Debug, PartialEq)]
pub(crate) struct ExtensionSignature {
    pub args: Vec<Type>,
    pub results: Vec<Vec<Type>>,
    pub fallthrough: Option<usize>,
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
}

type ExtensionRegistry = HashMap<String, Box<dyn ExtensionImplementation + Sync + Send>>;

lazy_static! {
    static ref REGISTRY: ExtensionRegistry = {
        let mut registry = ExtensionRegistry::new();
        arithmetic::register(&mut registry);
        unconditional_jump::register(&mut registry);
        jump_nz::register(&mut registry);
        match_nullable::register(&mut registry);
        gas_station::register(&mut registry);
        registry
    };
}

pub(crate) fn get_signature(e: &Extension) -> Result<ExtensionSignature, Error> {
    match REGISTRY.get(&e.name) {
        Some(ext) => ext.get_signature(&e.tmpl_args),
        _ => Err(Error::UnsupportedLibCallName),
    }
}
