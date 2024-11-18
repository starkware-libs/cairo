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
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::find_executable_function_ids;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::{Upcast, write_comma_separated};
use itertools::Itertools;

use crate::plugin::{RUNNABLE_PREFIX, RUNNABLE_RAW_ATTR, runnable_plugin_suite};

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
pub fn compile_runnable(
    path: &Path,
    runnable_path: Option<&str>,
    diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<CompiledFunction> {
    let mut db = RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(runnable_plugin_suite())
        .build()?;

    let main_crate_ids = setup_project(&mut db, Path::new(&path))?;

    compile_runnable_in_prepared_db(&db, runnable_path, main_crate_ids, diagnostics_reporter)
}

/// Runs compiler on the specified runnable function.
/// If no runnable was specified, verify that there is only one.
/// Otherwise, return an error.
pub fn compile_runnable_in_prepared_db(
    db: &RootDatabase,
    runnable_path: Option<&str>,
    main_crate_ids: Vec<CrateId>,
    mut diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<CompiledFunction> {
    let mut runnables: Vec<_> = find_executable_function_ids(db, main_crate_ids)
        .into_iter()
        .filter_map(|(id, labels)| labels.into_iter().any(|l| l == RUNNABLE_RAW_ATTR).then_some(id))
        .collect();

    if let Some(runnable_path) = runnable_path {
        runnables.retain(|runnable| originating_function_path(db, *runnable) == runnable_path);
    };
    let runnable = match runnables.len() {
        0 => {
            // Report diagnostics as they might reveal the reason why no runnable was found.
            diagnostics_reporter.ensure(db)?;
            anyhow::bail!("Requested `#[runnable]` not found.");
        }
        1 => runnables[0],
        _ => {
            let runnable_names = runnables
                .iter()
                .map(|runnable| originating_function_path(db, *runnable))
                .join("\n  ");
            anyhow::bail!(
                "More than one runnable found in the main crate: \n  {}\nUse --runnable to \
                 specify which to compile.",
                runnable_names
            );
        }
    };

    compile_runnable_function_in_prepared_db(db, runnable, diagnostics_reporter)
}

/// Returns the path to the function that the runnable is wrapping.
///
/// If the runnable is not wrapping a function, returns the full path of the runnable.
fn originating_function_path(db: &RootDatabase, wrapper: ConcreteFunctionWithBodyId) -> String {
    let wrapper_name = wrapper.name(db);
    let wrapper_full_path = wrapper.base_semantic_function(db).full_path(db.upcast());
    let Some(wrapped_name) = wrapper_name.strip_suffix(RUNNABLE_PREFIX) else {
        return wrapper_full_path;
    };
    let Some(wrapper_path_to_module) = wrapper_full_path.strip_suffix(wrapper_name.as_str()) else {
        return wrapper_full_path;
    };
    format!("{}{}", wrapper_path_to_module, wrapped_name)
}

/// Runs compiler for a runnable function.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `runnable` - [`ConcreteFunctionWithBodyId`]s to compile.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(Vec<String>)` - The result artifact of the compilation.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_runnable_function_in_prepared_db(
    db: &RootDatabase,
    runnable: ConcreteFunctionWithBodyId,
    mut diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<CompiledFunction> {
    diagnostics_reporter.ensure(db)?;
    let SierraProgramWithDebug { program: sierra_program, debug_info: _ } = Arc::unwrap_or_clone(
        db.get_sierra_program_for_functions(vec![runnable])
            .ok()
            .with_context(|| "Compilation failed without any diagnostics.")?,
    );
    let runnable_func = sierra_program.funcs[0].clone();
    let builder = RunnableBuilder::new(sierra_program, None)?;
    let wrapper = builder.create_wrapper_info(&runnable_func, EntryCodeConfig::provable())?;
    Ok(CompiledFunction { program: builder.casm_program().clone(), wrapper })
}
