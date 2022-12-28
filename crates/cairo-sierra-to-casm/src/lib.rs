//! CASM backend. Compiles from Sierra down to CASM. See [sierra] and [casm]

pub mod annotations;
pub mod compiler;
pub mod environment;
pub mod invocations;
pub mod metadata;
pub mod references;
pub mod relocations;
#[cfg(any(feature = "testing", test))]
pub mod test_utils;
pub mod type_sizes;
