use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_runnable_utils::builder::{
    CasmProgramWrapperInfo, EntryCodeConfig, RunnableBuilder,
};
use cairo_lang_semantic::db::PluginSuiteInput;
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::find_executable_function_ids;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::{Upcast, write_comma_separated};
use itertools::Itertools;

use crate::plugin::{EXECUTABLE_PREFIX, EXECUTABLE_RAW_ATTR, executable_plugin_suite};

/// The CASM compilation result.
pub struct CompiledFunction {
    /// The compiled CASM program.
    pub program: CairoProgram,
    /// The wrapper information for the program.
    pub wrapper: CasmProgramWrapperInfo,
}
impl std::fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "# builtins:")?;
        if !self.wrapper.builtins.is_empty() {
            write!(f, " ")?;
            write_comma_separated(f, self.wrapper.builtins.iter().map(|b| b.to_str()))?;
        }
        writeln!(f)?;
        writeln!(f, "# header #")?;
        for instruction in &self.wrapper.header {
            writeln!(f, "{};", instruction)?;
        }
        writeln!(f, "# sierra based code #")?;
        write!(f, "{}", self.program)?;
        writeln!(f, "# footer #")?;
        for instruction in &self.wrapper.footer {
            writeln!(f, "{};", instruction)?;
        }
        Ok(())
    }
}

/// Compile the function given by path.
/// Errors if there is ambiguity.
pub fn compile_executable(
    path: &Path,
    executable_path: Option<&str>,
    diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<CompiledFunction> {
    let mut db = RootDatabase::builder().skip_auto_withdraw_gas().detect_corelib().build()?;

    let main_crate_ids = setup_project(&mut db, Path::new(&path))?;

    for crate_id in main_crate_ids.iter() {
        db.set_crate_plugins_from_suite(
            *crate_id,
            get_default_plugin_suite() + executable_plugin_suite(),
        );
    }

    compile_executable_in_prepared_db(&db, executable_path, main_crate_ids, diagnostics_reporter)
}

/// Runs compiler on the specified executable function.
/// If no executable was specified, verify that there is only one.
/// Otherwise, return an error.
pub fn compile_executable_in_prepared_db(
    db: &RootDatabase,
    executable_path: Option<&str>,
    main_crate_ids: Vec<CrateId>,
    mut diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<CompiledFunction> {
    let mut executables: Vec<_> = find_executable_function_ids(db, main_crate_ids)
        .into_iter()
        .filter_map(|(id, labels)| {
            labels.into_iter().any(|l| l == EXECUTABLE_RAW_ATTR).then_some(id)
        })
        .collect();

    if let Some(executable_path) = executable_path {
        executables
            .retain(|executable| originating_function_path(db, *executable) == executable_path);
    };
    let executable = match executables.len() {
        0 => {
            // Report diagnostics as they might reveal the reason why no executable was found.
            diagnostics_reporter.ensure(db)?;
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

    compile_executable_function_in_prepared_db(db, executable, diagnostics_reporter)
}

/// Returns the path to the function that the executable is wrapping.
///
/// If the executable is not wrapping a function, returns the full path of the executable.
fn originating_function_path(db: &RootDatabase, wrapper: ConcreteFunctionWithBodyId) -> String {
    let wrapper_name = wrapper.name(db);
    let wrapper_full_path = wrapper.base_semantic_function(db).full_path(db.upcast());
    let Some(wrapped_name) = wrapper_name.strip_suffix(EXECUTABLE_PREFIX) else {
        return wrapper_full_path;
    };
    let Some(wrapper_path_to_module) = wrapper_full_path.strip_suffix(wrapper_name.as_str()) else {
        return wrapper_full_path;
    };
    format!("{}{}", wrapper_path_to_module, wrapped_name)
}

/// Runs compiler for an executable function.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `executable` - [`ConcreteFunctionWithBodyId`]s to compile.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(Vec<String>)` - The result artifact of the compilation.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_executable_function_in_prepared_db(
    db: &RootDatabase,
    executable: ConcreteFunctionWithBodyId,
    mut diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<CompiledFunction> {
    diagnostics_reporter.ensure(db)?;
    let SierraProgramWithDebug { program: sierra_program, debug_info: _ } = Arc::unwrap_or_clone(
        db.get_sierra_program_for_functions(vec![executable])
            .ok()
            .with_context(|| "Compilation failed without any diagnostics.")?,
    );
    let executable_func = sierra_program.funcs[0].clone();
    let builder = RunnableBuilder::new(sierra_program, None)?;
    let wrapper = builder.create_wrapper_info(&executable_func, EntryCodeConfig::executable())?;
    Ok(CompiledFunction { program: builder.casm_program().clone(), wrapper })
}
