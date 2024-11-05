use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_runnable_utils::builder::RunnableBuilder;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::find_executable_function_ids;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_utils::Upcast;
use itertools::Itertools;

use crate::plugin::{RUNNABLE_ATTR, RunnablePlugin};

/// Compile the function given by path.
/// Errors if there is ambiguity.
pub fn compile_runnable(
    path: &Path,
    runnable_path: Option<&str>,
    diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<String> {
    let mut db = RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(std::mem::take(PluginSuite::default().add_plugin::<RunnablePlugin>()))
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
) -> Result<String> {
    let mut runnables: Vec<_> = find_executable_function_ids(db, main_crate_ids)
        .into_iter()
        .filter_map(|(id, labels)| {
            labels.into_iter().find(|l| l == RUNNABLE_ATTR).is_some().then_some(id)
        })
        .collect();

    // TODO(ilya): Add contract names.
    if let Some(runnable_path) = runnable_path {
        runnables.retain(|runnable| {
            runnable.base_semantic_function(db).full_path(db.upcast()) == runnable_path
        });
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
                .map(|runnable| runnable.base_semantic_function(db).full_path(db.upcast()))
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
) -> Result<String> {
    diagnostics_reporter.ensure(db)?;
    let SierraProgramWithDebug { program: sierra_program, debug_info: _ } = Arc::unwrap_or_clone(
        db.get_sierra_program_for_functions(vec![runnable])
            .ok()
            .with_context(|| "Compilation failed without any diagnostics.")?,
    );
    let runnable_func = sierra_program.funcs[0].clone();
    let builder = RunnableBuilder::new(sierra_program, None)?;
    Ok(builder.casm_function_program(&runnable_func)?)
}
