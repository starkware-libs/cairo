use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::casm;
use cairo_vm::types::builtin_name::BuiltinName;
use itertools::chain;
use serde::{Deserialize, Serialize};

use crate::compile::CompiledFunction;

/// Structure to hold the runnable represenstation of a program.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Runnable {
    /// The bytecode of the program.
    pub program: AssembledCairoProgram,
    /// The available entrypoints for the program.
    pub entrypoints: Vec<RunnableEntryPoint>,
}

impl Runnable {
    /// Create a new runnable program from a compiled function.
    pub fn new(compiled: CompiledFunction) -> Self {
        let non_returning_header = casm! {
            ap += (compiled.wrapper.builtins.len());
            call rel 4;
            jmp rel 0;
        };
        Self {
            program: compiled.program.assemble_ex(
                chain!(&non_returning_header.instructions, &compiled.wrapper.header),
                &compiled.wrapper.footer,
            ),
            entrypoints: vec![
                RunnableEntryPoint {
                    builtins: compiled.wrapper.builtins.clone(),
                    offset: 0,
                    kind: EntryPointKind::NonReturning,
                },
                RunnableEntryPoint {
                    builtins: compiled.wrapper.builtins,
                    offset: non_returning_header.current_code_offset,
                    kind: EntryPointKind::Function,
                },
            ],
        }
    }
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
