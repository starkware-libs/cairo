//! CASM backend. Compiles from Sierra down to CASM. See [cairo_lang_sierra] and [cairo_lang_casm]

use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra_type_size::TypeSizeMap;
use circuit::CircuitsInfo;
use metadata::Metadata;
use num_bigint::BigInt;

pub mod annotations;
pub mod circuit;
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

/// Information in the program level required for compiling an invocation.
pub struct ProgramInfo<'a> {
    pub metadata: &'a Metadata,
    pub type_sizes: &'a TypeSizeMap,
    /// Information about the circuits in the program.
    pub circuits_info: &'a CircuitsInfo,
    /// Returns the given a const type returns a vector of cells value representing it.
    pub const_data_values: &'a dyn Fn(&ConcreteTypeId) -> Vec<BigInt>,
}
