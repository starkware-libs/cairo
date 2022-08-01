use std::collections::HashMap;

use thiserror::Error;

use crate::program::{Extension, Identifier, TemplateArg};

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

// Error option while using extensions.
#[derive(Error, Debug, PartialEq)]
pub enum SpecializationError {
    #[error("Count not find the requested extension")]
    UnsupportedLibCallName,
    #[error("Expected a different number of template arguments")]
    WrongNumberOfTemplateArgs,
    #[error("Provided template arg is unsupported")]
    UnsupportedTemplateArg,
}

// Error option while using extensions.
#[derive(Error, Debug, PartialEq)]
pub enum ExtensionError {
    #[error("Count not specialize extension")]
    Specialization { extension: Extension, error: SpecializationError },
}

// Handles extensions usages.
pub struct Extensions {
    specializers: HashMap<Identifier, SpecializerBox>,
}
impl Extensions {
    pub fn new() -> Extensions {
        Extensions {
            specializers: chain!(
                gas::extensions().into_iter(),
                integer::extensions().into_iter(),
                mem::extensions().into_iter(),
                unconditional_jump::extensions().into_iter(),
            )
            .collect(),
        }
    }
    pub fn specialize(&self, extension: &Extension) -> Result<SpecializationBox, ExtensionError> {
        self.specializers
            .get(&extension.id)
            .ok_or_else(|| ExtensionError::Specialization {
                extension: extension.clone(),
                error: SpecializationError::UnsupportedLibCallName,
            })?
            .specialize(&extension.tmpl_args)
            .map_err(|error| ExtensionError::Specialization { extension: extension.clone(), error })
    }
}
impl Default for Extensions {
    fn default() -> Self {
        Self::new()
    }
}

// Trait for implementing an specialization generator.
trait Specializer {
    // Creates the specialization with the template arguments.
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError>;
}

pub trait Specialization {}

type SpecializerBox = Box<dyn Specializer + Sync + Send>;
type SpecializationBox = Box<dyn Specialization + Sync + Send>;

fn validate_no_args(tmpl_args: &[TemplateArg]) -> Result<(), SpecializationError> {
    if tmpl_args.is_empty() { Ok(()) } else { Err(SpecializationError::WrongNumberOfTemplateArgs) }
}

#[cfg(test)]
mod test;
