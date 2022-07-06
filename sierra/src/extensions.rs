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

type ExtensionBox = Box<dyn ExtensionImplementation + Sync + Send>;

type ExtensionRegistry = HashMap<String, ExtensionBox>;

lazy_static! {
    static ref REGISTRY: ExtensionRegistry = {
        chain!(
            arithmetic::extensions().into_iter(),
            unconditional_jump::extensions().into_iter(),
            jump_nz::extensions().into_iter(),
            match_nullable::extensions().into_iter(),
            gas_station::extensions().into_iter()
        )
        .collect()
    };
}

pub(crate) fn get_signature(e: &Extension) -> Result<ExtensionSignature, Error> {
    match REGISTRY.get(&e.name) {
        Some(ext) => ext.get_signature(&e.tmpl_args),
        _ => Err(Error::UnsupportedLibCallName),
    }
}
