use std::collections::HashMap;

use crate::program::{Extension, TemplateArg};

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

// Error option while using extensions.
#[derive(Debug, PartialEq)]
pub enum Error {
    UnsupportedLibCallName,
    WrongNumberOfTypeArgs,
    UnsupportedTypeArg,
    UnexpectedMemoryStructure,
}

// Handles extensions usages.
pub(crate) struct Extensions {
    ext_reg: HashMap<String, ExtensionBox>,
}

impl Extensions {
    pub(crate) fn new() -> Extensions {
        Extensions {
            ext_reg: chain!(
                gas::extensions().into_iter(),
                integer::extensions().into_iter(),
                mem::extensions().into_iter(),
                unconditional_jump::extensions().into_iter(),
            )
            .collect(),
        }
    }

    // Simulates the memory transformation of the extension from the set of inputs implementations
    // to the outputs.
    pub(crate) fn simulate(
        &self,
        ext: &Extension,
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        self.ext_reg
            .get(&ext.name)
            .ok_or(Error::UnsupportedLibCallName)?
            .simulate(&ext.tmpl_args, inputs)
    }
}

// Trait for implementing an extension.
trait ExtensionImplementation {
    // Simulates the memory transformation of the extension from the set of inputs implementations
    // to the outputs.
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error>;
}

type ExtensionBox = Box<dyn ExtensionImplementation + Sync + Send>;

// Utility functions for developing the extensions.

struct NonBranchExtension {
    inner: NonBranchExtensionBox,
}
type NonBranchExtensionBox = Box<dyn NonBranchExtensionImplementation + Sync + Send>;

impl ExtensionImplementation for NonBranchExtension {
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        Ok((self.inner.simulate(tmpl_args, inputs)?, 0))
    }
}

// Trait for implementing a non branch extension.
trait NonBranchExtensionImplementation {
    // Simulates the memory transformation of the extension from the set of inputs implementations
    // to the outputs.
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error>;
}

fn validate_mem_sizes<const N: usize>(
    inputs: &[Vec<i64>],
    expectation: [usize; N],
) -> Result<(), Error> {
    if inputs.iter().map(|input| input.len()).eq(expectation) {
        Ok(())
    } else {
        Err(Error::UnexpectedMemoryStructure)
    }
}

fn unwrap_value(tmpl_arg: &TemplateArg) -> Result<i64, Error> {
    match tmpl_arg {
        TemplateArg::Value(v) => Ok(*v),
        TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
    }
}

fn validate_size_eq(tmpl_args: &[TemplateArg], size: usize) -> Result<(), Error> {
    if tmpl_args.len() == size { Ok(()) } else { Err(Error::WrongNumberOfTypeArgs) }
}

fn single_value_arg(tmpl_args: &[TemplateArg]) -> Result<i64, Error> {
    validate_size_eq(tmpl_args, 1)?;
    unwrap_value(&tmpl_args[0])
}

fn wrap_non_branch(nbb: NonBranchExtensionBox) -> ExtensionBox {
    Box::new(NonBranchExtension { inner: nbb })
}
