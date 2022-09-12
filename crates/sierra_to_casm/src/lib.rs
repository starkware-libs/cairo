//! CASM backend. Compiles from Sierra down to CASM. See [sierra] and [casm]

pub mod annotations;
pub mod compiler;
pub mod environment;
pub mod frame_state;
pub mod invocations;
pub mod references;
pub mod relocations;
pub mod type_sizes;
