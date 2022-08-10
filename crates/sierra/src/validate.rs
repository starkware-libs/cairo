use thiserror::Error;

use crate::extensions::{ExtensionError, Extensions};
use crate::program::Program;

/// Errors encountered during validation.
#[derive(Error, Debug, PartialEq)]
pub enum ValidationError {
    #[error("error while using extensions")]
    Extension(ExtensionError),
}

/// Validates a Sierra program.
pub fn validate(program: &Program) -> Result<(), ValidationError> {
    let mut extensions = Extensions::default();
    for declaration in &program.type_declarations {
        extensions.specialize_type(declaration).map_err(ValidationError::Extension)?;
    }
    for declaration in &program.extension_declarations {
        extensions.specialize_extension(declaration).map_err(ValidationError::Extension)?;
    }
    Ok(())
}
