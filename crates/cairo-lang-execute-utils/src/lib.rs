use std::collections::HashMap;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_casm::hints::Hint;
use cairo_lang_executable::executable::{EntryPointKind, Executable, ExecutableEntryPoint};
use cairo_lang_runner::{Arg, build_hints_dict};
use cairo_lang_utils::bigint::BigUintAsHex;
use cairo_vm::Felt252;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::MaybeRelocatable;
use num_bigint::BigInt;

// Returns a tuple containing:
/// 1. The Program instance - set according to the type of the required entrypoint.
/// 2. The hint by string mapping.
///
/// This is required for running an executable in the Cairo-VM.
pub fn program_and_hints_from_executable(
    executable: &Executable,
    entrypoint: &ExecutableEntryPoint,
) -> anyhow::Result<(Program, HashMap<String, Hint>)> {
    let data: Vec<MaybeRelocatable> =
        executable.program.bytecode.iter().map(Felt252::from).map(MaybeRelocatable::from).collect();
    let (hints, string_to_hint) = build_hints_dict(&executable.program.hints);
    let program = match entrypoint.kind {
        EntryPointKind::Standalone => Program::new_for_proof(
            entrypoint.builtins.clone(),
            data,
            entrypoint.offset,
            entrypoint.offset + 4,
            hints,
            Default::default(),
            Default::default(),
            vec![],
            None,
        ),
        EntryPointKind::Bootloader => Program::new(
            entrypoint.builtins.clone(),
            data,
            Some(entrypoint.offset),
            hints,
            Default::default(),
            Default::default(),
            vec![],
            None,
        ),
    }
    .with_context(|| "Failed setting up program.")?;

    Ok((program, string_to_hint))
}

/// Returns the user arguments for the program after parsing them from the provided file or list.
/// `as_file` - The content of a flag with an optional path to a json containing the args as hex
/// strings.
/// `as_list` - The content of a flag with the direct numeric value of the list.
/// Making sure not both are provided should be done by the cli.
///
/// This is required for running an executable in the Cairo-VM.
pub fn user_args_from_flags(
    as_file: Option<&PathBuf>,
    as_list: &[BigInt],
) -> anyhow::Result<Vec<Arg>> {
    let user_args = if let Some(path) = as_file {
        let as_vec: Vec<BigUintAsHex> = serde_json::from_reader(std::fs::File::open(path)?)
            .with_context(|| "Failed reading args file.")?;
        as_vec.into_iter().map(|v| Arg::Value(v.value.into())).collect()
    } else {
        as_list.iter().map(|v| Arg::Value(v.into())).collect()
    };

    Ok(user_args)
}
