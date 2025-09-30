use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::casm;
use cairo_vm::types::builtin_name::BuiltinName;
use itertools::chain;
use serde::{Deserialize, Serialize};

use crate::compile::CompiledFunction;
use crate::debug_info::{Annotations, DebugInfo, ProgramInformation};

pub const NOT_RETURNING_HEADER_SIZE: usize = 6;

/// Structure to hold the executable representation of a program.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Executable {
    /// The bytecode of the program.
    pub program: AssembledCairoProgram,
    /// The available entrypoints for the program.
    pub entrypoints: Vec<ExecutableEntryPoint>,
    /// Debug information for the assembled program.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub debug_info: Option<DebugInfo>,
}

impl Executable {
    /// Create a new executable program from a compiled function.
    pub fn new(compiled: CompiledFunction) -> Self {
        let non_returning_header = casm! {
            ap += (compiled.wrapper.builtins.len());
            call rel 4;
            jmp rel 0;
        };
        assert_eq!(non_returning_header.current_code_offset, NOT_RETURNING_HEADER_SIZE);
        Self {
            program: compiled.program.assemble_ex(
                chain!(&non_returning_header.instructions, &compiled.wrapper.header),
                &compiled.wrapper.footer,
            ),
            entrypoints: vec![
                ExecutableEntryPoint {
                    builtins: compiled.wrapper.builtins.clone(),
                    offset: 0,
                    kind: EntryPointKind::Standalone,
                },
                ExecutableEntryPoint {
                    builtins: compiled.wrapper.builtins,
                    offset: NOT_RETURNING_HEADER_SIZE,
                    kind: EntryPointKind::Bootloader,
                },
            ],
            debug_info: Some(DebugInfo {
                annotations: Annotations::from(ProgramInformation {
                    program_offset: NOT_RETURNING_HEADER_SIZE
                        + compiled
                            .wrapper
                            .header
                            .iter()
                            .map(|inst| inst.body.op_size())
                            .sum::<usize>(),
                }),
            }),
        }
    }
}

/// Information about an executable entrypoint.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutableEntryPoint {
    /// The used builtins of the function.
    pub builtins: Vec<BuiltinName>,
    /// The offset of the entrypoint in the bytecode.
    pub offset: usize,
    /// The kind of the entrypoint.
    pub kind: EntryPointKind,
}

/// The kind of an entrypoint.
#[derive(Debug, Clone, Serialize, PartialEq, Deserialize)]
pub enum EntryPointKind {
    /// Entrypoint is for running it using a bootloader.
    ///
    /// The entrypoint is a function, ending with a `ret`, expecting the builtins as its parameters.
    Bootloader,
    /// Entrypoint is for running this executable as a standalone program.
    ///
    /// The entrypoint starts with `ap += <builtins.len()>` and expects the builtins to be injected
    /// there, and ends with an infinite loop.
    Standalone,
}
