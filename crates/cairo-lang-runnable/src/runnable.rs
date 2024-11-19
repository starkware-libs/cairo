use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_vm::types::builtin_name::BuiltinName;
use serde::{Deserialize, Serialize};

/// Structure to hold the runnable represenstation of a program.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Runnable {
    /// The bytecode of the program.
    pub program: AssembledCairoProgram,
    /// The available entrypoints for the program.
    pub entrypoints: Vec<RunnableEntryPoint>,
}

/// Information about a runnable entrypoint.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunnableEntryPoint {
    /// The used builtins of the function.
    pub builtins: Vec<BuiltinName>,
    /// The offset of the entrypoint in the bytecode.
    pub offset: usize,
    /// The kind of the entrypoint.
    pub kind: EntryPointKind,
}

/// The kind of an entrypoint.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EntryPointKind {
    /// The entrypoint is a function, ending with a `ret`, expecting the builtins as its parameters.
    Function,
    /// The entrypoint starts with `ap += <builtins.len()>` and expected the builtins to be injected
    /// there, and ends with an infinite loop.
    NonReturning,
}
