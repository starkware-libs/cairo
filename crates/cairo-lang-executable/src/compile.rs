use std::path::Path;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::{ensure_diagnostics, get_sierra_program_for_functions};
use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_executable_plugin::{
    EXECUTABLE_PREFIX, EXECUTABLE_RAW_ATTR, executable_plugin_suite,
};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::{CrateId, CrateInput};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_runnable_utils::builder::{
    CasmProgramWrapperInfo, EntryCodeConfig, RunnableBuilder,
};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::find_executable_function_ids;
use cairo_lang_sierra_generator::program_generator::{
    SierraProgramDebugInfo, SierraProgramWithDebug,
};
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::write_comma_separated;
use itertools::Itertools;
use salsa::Database;

/// The CASM compilation result.
pub struct CompiledFunction {
    /// The compiled CASM program.
    pub program: CairoProgram,
    /// The wrapper information for the program.
    pub wrapper: CasmProgramWrapperInfo,
}
impl std::fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "// builtins:")?;
        if !self.wrapper.builtins.is_empty() {
            write!(f, " ")?;
            write_comma_separated(f, self.wrapper.builtins.iter().map(|b| b.to_str()))?;
        }
        writeln!(f)?;
        writeln!(f, "// header")?;
        for instruction in &self.wrapper.header {
            writeln!(f, "{instruction};")?;
        }
        writeln!(f, "// sierra based code")?;
        write!(f, "{}", self.program)?;
        writeln!(f, "// footer")?;
        for instruction in &self.wrapper.footer {
            writeln!(f, "{instruction};")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct ExecutableConfig {
    /// If true, will allow syscalls in the program.
    ///
    /// In general, syscalls are not allowed in executables, as they are currently not verified.
    pub allow_syscalls: bool,

    /// Replace the panic flow with an unprovable opcode, this reduces code size but might make it
    /// more difficult to debug.
    pub unsafe_panic: bool,
}

/// Represents the output of compiling an executable.
///
/// Includes the `CompiledFunction` along with supplementary objects useful for profiling.
pub struct CompileExecutableResult<'db> {
    /// The compiled function.
    pub compiled_function: CompiledFunction,
    /// A runnable builder with the program corresponding to the compiled function.
    pub builder: RunnableBuilder,
    /// The debug info for the Sierra program in the builder.
    pub debug_info: SierraProgramDebugInfo<'db>,
}

/// Compile the function given by path.
/// Errors if there is ambiguity.
pub fn prepare_db(config: &ExecutableConfig) -> Result<RootDatabase> {
    let mut builder = RootDatabase::builder();
    builder
        .skip_auto_withdraw_gas()
        .with_cfg(CfgSet::from_iter([Cfg::kv("gas", "disabled")]))
        .detect_corelib()
        .with_default_plugin_suite(executable_plugin_suite());
    if config.unsafe_panic {
        builder.with_unsafe_panic();
    }

    builder.build()
}

/// Compile the function given by path.
/// Errors if there is ambiguity.
pub fn compile_executable<'db>(
    db: &'db mut dyn Database,
    path: &Path,
    executable_path: Option<&str>,
    diagnostics_reporter: DiagnosticsReporter<'_>,
    config: ExecutableConfig,
) -> Result<CompileExecutableResult<'db>> {
    // let mut db = prepare_db(&config)?;

    let main_crate_inputs = setup_project(db, Path::new(&path))?;
    let diagnostics_reporter = diagnostics_reporter.with_crates(&main_crate_inputs);
    let main_crate_ids = CrateInput::into_crate_ids(db, main_crate_inputs);

    compile_executable_in_prepared_db(
        db,
        executable_path,
        main_crate_ids,
        diagnostics_reporter,
        config,
    )
}

/// Runs compiler on the specified executable function.
/// If no executable was specified, verify that there is only one.
/// Otherwise, return an error.
pub fn compile_executable_in_prepared_db<'db>(
    db: &'db dyn Database,
    executable_path: Option<&str>,
    main_crate_ids: Vec<CrateId<'db>>,
    mut diagnostics_reporter: DiagnosticsReporter<'_>,
    config: ExecutableConfig,
) -> Result<CompileExecutableResult<'db>> {
    ensure_diagnostics(db, &mut diagnostics_reporter)?;

    let executables = find_executable_functions(db, main_crate_ids, executable_path);

    let executable = match executables.len() {
        0 => {
            // Report diagnostics as they might reveal the reason why no executable was found.
            anyhow::bail!("Requested `#[executable]` not found.");
        }
        1 => executables[0],
        _ => {
            let executable_names = executables
                .iter()
                .map(|executable| originating_function_path(db, *executable))
                .join("\n  ");
            anyhow::bail!(
                "More than one executable found in the main crate: \n  {}\nUse --executable to \
                 specify which to compile.",
                executable_names
            );
        }
    };

    compile_executable_function_in_prepared_db(db, executable, config)
}

/// Search crates identified by `main_crate_ids` for executable functions.
/// If `executable_path` is provided, only functions with exactly the same path will be returned.
pub fn find_executable_functions<'db>(
    db: &'db dyn Database,
    main_crate_ids: Vec<CrateId<'db>>,
    executable_path: Option<&str>,
) -> Vec<ConcreteFunctionWithBodyId<'db>> {
    let mut executables: Vec<_> = find_executable_function_ids(db, main_crate_ids)
        .into_iter()
        .filter_map(|(id, labels)| {
            labels.into_iter().any(|ssid| ssid.long(db) == EXECUTABLE_RAW_ATTR).then_some(id)
        })
        .collect();

    if let Some(executable_path) = executable_path {
        executables
            .retain(|executable| originating_function_path(db, *executable) == executable_path);
    };
    executables
}

/// Returns the path to the function that the executable is wrapping.
///
/// If the executable is not wrapping a function, returns the full path of the executable.
pub fn originating_function_path<'db>(
    db: &'db dyn Database,
    wrapper: ConcreteFunctionWithBodyId<'db>,
) -> String {
    let semantic = wrapper.base_semantic_function(db);
    let wrapper_name = semantic.name(db).long(db).as_str();
    let wrapper_full_path = semantic.full_path(db);
    let Some(wrapped_name) = wrapper_name.strip_prefix(EXECUTABLE_PREFIX) else {
        return wrapper_full_path;
    };
    let Some(wrapper_path_to_module) = wrapper_full_path.strip_suffix(wrapper_name) else {
        return wrapper_full_path;
    };
    format!("{wrapper_path_to_module}{wrapped_name}")
}

/// Runs compiler for an executable function.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `executable` - [`ConcreteFunctionWithBodyId`]s to compile.
/// * `diagnostics_reporter` - The diagnostics reporter.
/// * `config` - If true, the compilation will not fail if the program is not sound.
/// # Returns
/// * `Ok(Vec<String>)` - The result artifact of the compilation.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_executable_function_in_prepared_db<'db>(
    db: &'db dyn Database,
    executable: ConcreteFunctionWithBodyId<'db>,
    config: ExecutableConfig,
) -> Result<CompileExecutableResult<'db>> {
    let SierraProgramWithDebug { program: sierra_program, debug_info } =
        get_sierra_program_for_functions(db, vec![executable])
            .with_context(|| "Compilation failed without any diagnostics.")?;
    if !config.allow_syscalls {
        // Finding if any syscall libfuncs are used in the program.
        // If any are found, the compilation will fail, as syscalls are not proved in executables.
        for libfunc in &sierra_program.libfunc_declarations {
            if libfunc.long_id.generic_id.0.ends_with("_syscall") {
                anyhow::bail!(
                    "The function is using libfunc `{}`. Syscalls are not supported in \
                     `#[executable]`.",
                    libfunc.long_id.generic_id
                );
            }
        }
    }

    // Since we build the entry point asking for a single function - we know it will be first, and
    // that it will be available.
    let executable_func = sierra_program.funcs[0].clone();
    assert_eq!(executable_func.id, db.intern_sierra_function(executable.function_id(db).unwrap()));
    let builder = RunnableBuilder::new(sierra_program.clone(), None).map_err(|err| {
        let mut locs = vec![];
        for stmt_idx in err.stmt_indices() {
            if let Some(loc) =
                debug_info.statements_locations.statement_diagnostic_location(db, stmt_idx)
            {
                locs.push(format!("#{stmt_idx} {:?}", loc.debug(db)))
            }
        }
        anyhow::anyhow!("Failed to create runnable builder: {}\n{}", err, locs.join("\n"))
    })?;

    // If syscalls are allowed it means we allow for unsound programs.
    let allow_unsound = config.allow_syscalls;
    let wrapper = builder
        .create_wrapper_info(&executable_func, EntryCodeConfig::executable(allow_unsound))?;
    let compiled_function = CompiledFunction { program: builder.casm_program().clone(), wrapper };
    Ok(CompileExecutableResult { compiled_function, builder, debug_info: debug_info.clone() })
}
