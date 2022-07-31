use std::collections::HashMap;

use thiserror::Error;

use crate::program::{Extension, TemplateArg};

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

// Error option while using extensions.
#[derive(Error, Debug, PartialEq)]
pub enum ExtensionError {
    #[error("Count not find the requested exension")]
    UnsupportedLibCallName,
    #[error("Expected a different number of template arguments")]
    WrongNumberOfTemplateArgs,
    #[error("Provided template arg is unsupported")]
    UnsupportedTemplateArg,
    #[error("Unexpected memory structure")]
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
    ) -> Result<(Vec<Vec<i64>>, usize), ExtensionError> {
        self.ext_reg
            .get(&ext.name)
            .ok_or(ExtensionError::UnsupportedLibCallName)?
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
    ) -> Result<(Vec<Vec<i64>>, usize), ExtensionError>;
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
    ) -> Result<(Vec<Vec<i64>>, usize), ExtensionError> {
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
    ) -> Result<Vec<Vec<i64>>, ExtensionError>;
}

fn validate_mem_sizes<const N: usize>(
    inputs: &[Vec<i64>],
    expectation: [usize; N],
) -> Result<(), ExtensionError> {
    if inputs.iter().map(|input| input.len()).eq(expectation) {
        Ok(())
    } else {
        Err(ExtensionError::UnexpectedMemoryStructure)
    }
}

fn unwrap_value(tmpl_arg: &TemplateArg) -> Result<i64, ExtensionError> {
    match tmpl_arg {
        TemplateArg::Value(v) => Ok(*v),
        TemplateArg::Type(_) => Err(ExtensionError::UnsupportedTemplateArg),
    }
}

fn validate_size_eq(tmpl_args: &[TemplateArg], size: usize) -> Result<(), ExtensionError> {
    if tmpl_args.len() == size { Ok(()) } else { Err(ExtensionError::WrongNumberOfTemplateArgs) }
}

fn single_value_arg(tmpl_args: &[TemplateArg]) -> Result<i64, ExtensionError> {
    validate_size_eq(tmpl_args, 1)?;
    unwrap_value(&tmpl_args[0])
}

fn wrap_non_branch(nbb: NonBranchExtensionBox) -> ExtensionBox {
    Box::new(NonBranchExtension { inner: nbb })
}
