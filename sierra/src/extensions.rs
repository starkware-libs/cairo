use crate::error::Error;
use crate::graph::*;
use std::collections::HashMap;
use Result::*;

mod arithmetic;
mod gas_station;
mod jump_nz;
mod match_nullable;
mod unconditional_jump;

trait InvokeExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error>;
}

trait JumpExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Vec<Type>>), Error>;
}

pub(self) struct ExtensionRegistry {
    invoke_exts: HashMap<String, Box<dyn InvokeExtension + Sync + Send>>,
    jump_exts: HashMap<String, Box<dyn JumpExtension + Sync + Send>>,
}

lazy_static! {
    static ref REGISTRY: ExtensionRegistry = {
        let mut registry = ExtensionRegistry {
            invoke_exts: HashMap::<String, Box<dyn InvokeExtension + Sync + Send>>::new(),
            jump_exts: HashMap::<String, Box<dyn JumpExtension + Sync + Send>>::new(),
        };
        arithmetic::register(&mut registry);
        unconditional_jump::register(&mut registry);
        jump_nz::register(&mut registry);
        match_nullable::register(&mut registry);
        gas_station::register(&mut registry);
        registry
    };
}

pub fn get_invoke_signature(e: &Extension) -> Result<(Vec<Type>, Vec<Type>), Error> {
    match REGISTRY.invoke_exts.get(&e.name) {
        Some(ext) => ext.get_signature(&e.tmpl_args),
        _ => Err(Error::UnsupportedLibCallName),
    }
}

pub fn get_jump_signature(e: &Extension) -> Result<(Vec<Type>, Vec<Vec<Type>>), Error> {
    match REGISTRY.jump_exts.get(&e.name) {
        Some(ext) => ext.get_signature(&e.tmpl_args),
        _ => Err(Error::UnsupportedLibCallName),
    }
}
