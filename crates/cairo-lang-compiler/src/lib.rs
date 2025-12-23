//! Cairo compiler.
//!
//! This crate is responsible for compiling a Cairo project into a Sierra program.
//! It is the main entry point for the compiler.
use std::path::Path;
use std::sync::Mutex;

use ::cairo_lang_diagnostics::ToOption;
use anyhow::{Context, Result};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::ids::{CrateId, CrateInput};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_lowering::optimizations::config::Optimizations;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_sierra::debug_info::{Annotations, DebugInfo};
use cairo_lang_sierra::program::{Program, ProgramArtifact};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::{collect_executables, find_executable_function_ids};
use cairo_lang_sierra_generator::program_generator::{
    SierraProgramWithDebug, find_all_free_function_ids, try_get_function_with_body_id,
};
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{CloneableDatabase, Intern};
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use salsa::Database;

use crate::db::RootDatabase;
use crate::diagnostics::{DiagnosticsError, DiagnosticsReporter};
use crate::project::{ProjectConfig, get_main_crate_ids_from_project, setup_project};

pub mod db;
pub mod diagnostics;
pub mod project;

#[cfg(test)]
mod test;

/// Configuration for the compiler.
#[derive(Default)]
pub struct CompilerConfig<'a> {
    pub diagnostics_reporter: DiagnosticsReporter<'a>,

    /// Replaces Sierra IDs with human-readable ones.
    pub replace_ids: bool,

    /// Adds a mapping used by [cairo-profiler](https://github.com/software-mansion/cairo-profiler)
    /// to [Annotations] in [DebugInfo].
    pub add_statements_functions: bool,

    /// Adds a mapping used by [cairo-coverage](https://github.com/software-mansion/cairo-coverage)
    /// to [Annotations] in [DebugInfo].
    pub add_statements_code_locations: bool,

    /// Adds a mapping used by [cairo-debugger](https://github.com/software-mansion-labs/cairo-debugger)
    /// to [Annotations] in [DebugInfo] in the compiled tests.
    pub add_functions_debug_info: bool,
}

/// Compiles a Cairo project at the given path.
/// The project must be a valid Cairo project:
/// Either a standalone `.cairo` file (a single crate), or a directory with a `cairo_project.toml`
/// file.
/// # Arguments
/// * `path` - The path to the project.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(Program)` - The compiled program.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_cairo_project_at_path(
    path: &Path,
    compiler_config: CompilerConfig<'_>,
    inlining_strategy: InliningStrategy,
) -> Result<Program> {
    let mut db = RootDatabase::builder()
        .with_optimizations(Optimizations::enabled_with_default_movable_functions(
            inlining_strategy,
        ))
        .detect_corelib()
        .build()?;
    let main_crate_ids = setup_project(&mut db, path)?;
    compile_prepared_db_program(
        &db,
        CrateInput::into_crate_ids(&db, main_crate_ids),
        compiler_config,
    )
}

/// Compiles a Cairo project.
/// The project must be a valid Cairo project.
/// This function is a wrapper over [`RootDatabase::builder()`] and [`compile_prepared_db_program`].
/// # Arguments
/// * `project_config` - The project configuration.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(Program)` - The compiled program.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile(
    project_config: ProjectConfig,
    compiler_config: CompilerConfig<'_>,
) -> Result<Program> {
    let db = RootDatabase::builder()
        .with_optimizations(Optimizations::enabled_with_default_movable_functions(
            InliningStrategy::Default,
        ))
        .with_project_config(project_config.clone())
        .build()?;
    let main_crate_ids = get_main_crate_ids_from_project(&db, &project_config);

    compile_prepared_db_program(&db, main_crate_ids, compiler_config)
}

/// Runs Cairo compiler.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `main_crate_ids` - [`CrateId`]s to compile. Do not include dependencies here, only pass
///   top-level crates in order to eliminate unused code. Use `CrateLongId::Real(name).intern(db)`
///   in order to obtain [`CrateId`] from its name.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(Program)` - The compiled program.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_prepared_db_program<'db>(
    db: &'db dyn Database,
    main_crate_ids: Vec<CrateId<'db>>,
    compiler_config: CompilerConfig<'_>,
) -> Result<Program> {
    Ok(compile_prepared_db(db, main_crate_ids, compiler_config)?.program)
}

/// Runs Cairo compiler.
///
/// Similar to `compile_prepared_db_program`, but this function returns all the raw debug
/// information.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `main_crate_ids` - [`CrateId`]s to compile. Do not include dependencies here, only pass
///   top-level crates in order to eliminate unused code. Use `CrateLongId::Real(name).intern(db)`
///   in order to obtain [`CrateId`] from its name.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(SierraProgramWithDebug)` - The compiled program with debug info.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_prepared_db<'db>(
    db: &'db dyn Database,
    main_crate_ids: Vec<CrateId<'db>>,
    mut compiler_config: CompilerConfig<'_>,
) -> Result<SierraProgramWithDebug<'db>> {
    compiler_config.diagnostics_reporter.ensure(db)?;

    let mut sierra_program_with_debug = db
        .get_sierra_program(main_crate_ids)
        .to_option()
        .context("Compilation failed without any diagnostics")?
        .clone();

    if compiler_config.replace_ids {
        sierra_program_with_debug.program =
            replace_sierra_ids_in_program(db, &sierra_program_with_debug.program);
    }

    Ok(sierra_program_with_debug)
}

/// Checks if parallelism is available for the warmup.
fn should_warmup() -> bool {
    rayon::current_num_threads() > 1
}

/// Checks if there are diagnostics and reports them to the provided callback as strings.
/// Returns `Err` if diagnostics were found.
///
/// Note: Usually diagnostics should be checked as early as possible to avoid running into
/// compilation errors that have not been reported to the user yet (which can result in compiler
/// panic). This requires us to split the diagnostics warmup and function compilation warmup into
/// two separate steps (note that we don't usually know the `ConcreteFunctionWithBodyId` yet when
/// calculating diagnostics).
///
/// Performs parallel database warmup (if possible) and calls `DiagnosticsReporter::ensure`.
pub fn ensure_diagnostics(
    db: &dyn CloneableDatabase,
    diagnostic_reporter: &mut DiagnosticsReporter<'_>,
) -> std::result::Result<(), DiagnosticsError> {
    if should_warmup() {
        let crates = diagnostic_reporter.crates_of_interest(db);
        let warmup_db = db.dyn_clone();
        let ensure_db = db.dyn_clone();
        rayon::join(
            move || warmup_diagnostics_blocking(warmup_db.as_ref(), crates),
            move || diagnostic_reporter.ensure(ensure_db.as_ref()),
        )
        .1
    } else {
        diagnostic_reporter.ensure(db)
    }
}

/// Spawns threads to compute the diagnostics queries, making sure later calls for these queries
/// would be faster as the queries were already computed.
fn warmup_diagnostics_blocking(db: &dyn CloneableDatabase, crates: Vec<CrateInput>) {
    crates.into_par_iter().for_each_with(db.dyn_clone(), |db, crate_input| {
        let db = db.as_ref();
        let crate_id = crate_input.into_crate_long_id(db).intern(db);
        db.crate_modules(crate_id).into_par_iter().for_each_with(
            db.dyn_clone(),
            |db, module_id| {
                for file_id in db.module_files(*module_id).unwrap_or_default().iter().copied() {
                    db.file_syntax_diagnostics(file_id);
                }
                let _ = db.module_semantic_diagnostics(*module_id);
                let _ = db.module_lowering_diagnostics(*module_id);
            },
        );
    });
}

/// Spawns threads to compute the `function_with_body_sierra` query and all dependent queries for
/// the requested functions and their dependencies.
///
/// Note that typically spawn_warmup_db should be used as this function is blocking.
fn warmup_functions_blocking<'db>(
    db: &dyn CloneableDatabase,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId<'db>>,
) {
    let processed_function_ids = &Mutex::new(UnorderedHashSet::<salsa::Id>::default());
    requested_function_ids.into_par_iter().for_each_with(db.dyn_clone(), move |db, func_id| {
        fn handle_func_inner<'db>(
            processed_function_ids: &Mutex<UnorderedHashSet<salsa::Id>>,
            db: &dyn CloneableDatabase,
            func_id: ConcreteFunctionWithBodyId<'db>,
        ) {
            if processed_function_ids.lock().unwrap().insert(func_id.as_intern_id()) {
                let Ok(function) = db.function_with_body_sierra(func_id) else {
                    return;
                };
                function.body.par_iter().for_each_with(db.dyn_clone(), move |db, statement| {
                    let related_function_id: ConcreteFunctionWithBodyId<'_> =
                        if let Some(r_id) = try_get_function_with_body_id(db.as_ref(), statement) {
                            r_id
                        } else {
                            return;
                        };

                    handle_func_inner(processed_function_ids, db.as_ref(), related_function_id);
                });
            }
        }
        handle_func_inner(processed_function_ids, db.as_ref(), func_id)
    });
}

/// Checks if there are diagnostics in the database and if there are none, returns
/// the [SierraProgramWithDebug] object of the requested functions.
pub fn get_sierra_program_for_functions<'db>(
    db: &'db dyn CloneableDatabase,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId<'db>>,
) -> Result<&'db SierraProgramWithDebug<'db>> {
    if should_warmup() {
        let requested_function_ids = requested_function_ids.clone();
        warmup_functions_blocking(db, requested_function_ids);
    }
    db.get_sierra_program_for_functions(requested_function_ids)
        .to_option()
        .context("Compilation failed without any diagnostics.")
}

/// Runs Cairo compiler for specified crates.
///
/// Wrapper over [`compile_prepared_db`], but this function returns [`ProgramArtifact`]
/// with requested debug info.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `main_crate_ids` - [`CrateId`]s to compile. Do not include dependencies here, only pass
///   top-level crates in order to eliminate unused code. Use `CrateLongId::Real(name).intern(db)`
///   in order to obtain [`CrateId`] from its name.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(ProgramArtifact)` - The compiled program artifact with requested debug info.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_prepared_db_program_artifact<'db>(
    db: &'db dyn CloneableDatabase,
    main_crate_ids: Vec<CrateId<'db>>,
    mut compiler_config: CompilerConfig<'_>,
) -> Result<ProgramArtifact> {
    ensure_diagnostics(db, &mut compiler_config.diagnostics_reporter)?;

    let executable_functions = find_executable_function_ids(db, main_crate_ids.clone());

    let function_ids = if executable_functions.is_empty() {
        // No executables found - compile for all main crates.
        // TODO(maciektr): Deprecate in future. This compilation is useless, without `replace_ids`.
        find_all_free_function_ids(db, main_crate_ids)
            .to_option()
            .context("Compilation failed without any diagnostics.")?
    } else {
        // Compile for executable functions only.
        executable_functions.keys().cloned().collect()
    };

    let mut program_artifact =
        compile_prepared_db_program_artifact_for_functions(db, function_ids, compiler_config)?;

    // Calculate executable function Sierra ids.
    let executables = collect_executables(db, executable_functions, &program_artifact.program);

    let debug_info = program_artifact.debug_info.take().unwrap_or_default();

    Ok(program_artifact.with_debug_info(DebugInfo { executables, ..debug_info }))
}

/// Runs Cairo compiler for specified functions.
///
/// Wrapper over [`compile_prepared_db`], but this function returns [`ProgramArtifact`]
/// with requested debug info.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `requested_function_ids` - [`ConcreteFunctionWithBodyId`]s to compile.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(ProgramArtifact)` - The compiled program artifact with requested debug info.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_prepared_db_program_artifact_for_functions<'db>(
    db: &'db dyn CloneableDatabase,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId<'db>>,
    compiler_config: CompilerConfig<'_>,
) -> Result<ProgramArtifact> {
    let mut sierra_program_with_debug =
        get_sierra_program_for_functions(db, requested_function_ids)?.clone();

    if compiler_config.replace_ids {
        sierra_program_with_debug.program =
            replace_sierra_ids_in_program(db, &sierra_program_with_debug.program);
    }

    let mut annotations = Annotations::default();

    if compiler_config.add_statements_functions {
        annotations.extend(Annotations::from(
            sierra_program_with_debug
                .debug_info
                .statements_locations
                .extract_statements_functions(db),
        ))
    };

    if compiler_config.add_statements_code_locations {
        annotations.extend(Annotations::from(
            sierra_program_with_debug
                .debug_info
                .statements_locations
                .extract_statements_source_code_locations(db),
        ))
    };

    if compiler_config.add_functions_debug_info {
        annotations.extend(Annotations::from(
            sierra_program_with_debug.debug_info.functions_info.extract_serializable_debug_info(db),
        ))
    }

    let debug_info = DebugInfo {
        type_names: Default::default(),
        libfunc_names: Default::default(),
        user_func_names: Default::default(),
        annotations,
        executables: Default::default(),
    };

    Ok(ProgramArtifact::stripped(sierra_program_with_debug.program).with_debug_info(debug_info))
}
