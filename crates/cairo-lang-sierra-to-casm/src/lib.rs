//! CASM backend. Compiles from Sierra down to CASM. See [cairo_lang_sierra] and [cairo_lang_casm]

pub mod annotations;
// TODO(ilya): Reduce the size of CompilationError.
#[allow(clippy::result_large_err)]
pub mod compiler;
pub mod environment;
pub mod invocations;
pub mod metadata;
pub mod references;
pub mod relocations;
#[cfg(any(feature = "testing", test))]
pub mod test_utils;
pub mod type_sizes;
