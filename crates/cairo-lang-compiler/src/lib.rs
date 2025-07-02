//! Cairo compiler.
//!
//! This crate is responsible for compiling a Cairo project into a Sierra program.
//! It is the main entry point for the compiler.
use std::path::Path;
use std::sync::{Arc, Mutex};

use ::cairo_lang_diagnostics::ToOption;
use anyhow::{Context, Result};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_sierra::debug_info::{Annotations, DebugInfo};
use cairo_lang_sierra::program::{Program, ProgramArtifact};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::{collect_executables, find_executable_function_ids};
use cairo_lang_sierra_generator::program_generator::{
    SierraProgramWithDebug, try_get_function_with_body_id,
};
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use rayon::{ThreadPool, ThreadPoolBuilder};

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
pub struct CompilerConfig<'c> {
    pub diagnostics_reporter: DiagnosticsReporter<'c>,

    /// Replaces sierra ids with human-readable ones.
    pub replace_ids: bool,

    /// Disables inlining functions.
    pub inlining_strategy: InliningStrategy,

    /// The name of the allowed libfuncs list to use in compilation.
    /// If None the default list of audited libfuncs will be used.
    pub allowed_libfuncs_list_name: Option<String>,

    /// Adds mapping used by [cairo-profiler](https://github.com/software-mansion/cairo-profiler) to
    /// [cairo_lang_sierra::debug_info::Annotations] in [cairo_lang_sierra::debug_info::DebugInfo].
    pub add_statements_functions: bool,

    /// Adds mapping used by [cairo-coverage](https://github.com/software-mansion/cairo-coverage) to
    /// [cairo_lang_sierra::debug_info::Annotations] in [cairo_lang_sierra::debug_info::DebugInfo].
    pub add_statements_code_locations: bool,
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
) -> Result<Program> {
    let mut db = RootDatabase::builder()
        .with_inlining_strategy(compiler_config.inlining_strategy)
        .detect_corelib()
        .build()?;
    let main_crate_ids = setup_project(&mut db, path)?;
    compile_prepared_db_program(&mut db, main_crate_ids, compiler_config)
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
    let mut db = RootDatabase::builder()
        .with_inlining_strategy(compiler_config.inlining_strategy)
        .with_project_config(project_config.clone())
        .build()?;
    let main_crate_ids = get_main_crate_ids_from_project(&mut db, &project_config);

    compile_prepared_db_program(&mut db, main_crate_ids, compiler_config)
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
pub fn compile_prepared_db_program(
    db: &mut RootDatabase,
    main_crate_ids: Vec<CrateId>,
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
pub fn compile_prepared_db(
    db: &RootDatabase,
    main_crate_ids: Vec<CrateId>,
    mut compiler_config: CompilerConfig<'_>,
) -> Result<SierraProgramWithDebug> {
    compiler_config.diagnostics_reporter.ensure(db)?;

    let mut sierra_program_with_debug = Arc::unwrap_or_clone(
        db.get_sierra_program(main_crate_ids)
            .to_option()
            .context("Compilation failed without any diagnostics")?,
    );

    if compiler_config.replace_ids {
        sierra_program_with_debug.program =
            replace_sierra_ids_in_program(db, &sierra_program_with_debug.program);
    }

    Ok(sierra_program_with_debug)
}

/// Context for database warmup.
///
/// This struct will spawn a thread pool that can be used for parallel database warmup.
/// This can be both diagnostics warmup and function compilation warmup.
/// We encapsulate the thread pool here so that we can reuse it easily for both.
/// Note: Usually diagnostics should be checked as early as possible to avoid running into
/// compilation errors that have not been reported to the user yet (which can result in compiler
/// panic). This requires us to split the diagnostics warmup and function compilation warmup into
/// two separate steps (note that we don't usually know the `ConcreteFunctionWithBodyId` yet when
/// calculating diagnostics).
pub enum DbWarmupContext {
    Warmup { pool: ThreadPool },
    NoWarmup,
}

impl DbWarmupContext {
    /// Creates a new thread pool.
    pub fn new() -> Self {
        if !Self::should_warmup() {
            return Self::NoWarmup;
        }
        const MAX_WARMUP_PARALLELISM: usize = 4;
        let pool = ThreadPoolBuilder::new()
            .num_threads(rayon::current_num_threads().min(MAX_WARMUP_PARALLELISM))
            .build()
            .expect("failed to build rayon thread pool");
        Self::Warmup { pool }
    }

    /// Checks if parallelism is available for warmup.
    fn should_warmup() -> bool {
        rayon::current_num_threads() > 1
    }

    /// Performs parallel database warmup (if possible)
    fn warmup_diagnostics(
        &self,
        db: &RootDatabase,
        diagnostic_reporter: &mut DiagnosticsReporter<'_>,
    ) {
        match self {
            Self::Warmup { pool } => diagnostic_reporter.warm_up_diagnostics(db, pool),
            Self::NoWarmup => {}
        }
    }

    /// Checks if there are diagnostics and reports them to the provided callback as strings.
    /// Returns `Err` if diagnostics were found.
    ///
    /// Performs parallel database warmup (if possible) and calls `DiagnosticsReporter::ensure`.
    pub fn ensure_diagnostics(
        &self,
        db: &RootDatabase,
        diagnostic_reporter: &mut DiagnosticsReporter<'_>,
    ) -> std::result::Result<(), DiagnosticsError> {
        self.warmup_diagnostics(db, diagnostic_reporter);
        diagnostic_reporter.ensure(db)?;
        Ok(())
    }

    /// Spawns a task to warm up the db for the requested functions (if possible).
    fn warmup_db(
        &self,
        db: &RootDatabase,
        requested_function_ids: Vec<ConcreteFunctionWithBodyId>,
    ) {
        match self {
            Self::Warmup { pool } => {
                let snapshot = salsa::ParallelDatabase::snapshot(db);
                pool.spawn(move || warmup_db_blocking(snapshot, requested_function_ids));
            }
            Self::NoWarmup => {}
        }
    }
}

impl Default for DbWarmupContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Spawns threads to compute the `function_with_body_sierra` query and all dependent queries for
/// the requested functions and their dependencies.
///
/// Note that typically spawn_warmup_db should be used as this function is blocking.
fn warmup_db_blocking(
    snapshot: salsa::Snapshot<RootDatabase>,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId>,
) {
    let processed_function_ids =
        &Mutex::new(UnorderedHashSet::<ConcreteFunctionWithBodyId>::default());
    rayon::scope(move |s| {
        for func_id in requested_function_ids {
            let snapshot = salsa::ParallelDatabase::snapshot(&*snapshot);

            s.spawn(move |_| {
                fn handle_func_inner(
                    processed_function_ids: &Mutex<UnorderedHashSet<ConcreteFunctionWithBodyId>>,
                    snapshot: salsa::Snapshot<RootDatabase>,
                    func_id: ConcreteFunctionWithBodyId,
                ) {
                    if processed_function_ids.lock().unwrap().insert(func_id) {
                        rayon::scope(move |s| {
                            let db = &*snapshot;
                            let Ok(function) = db.function_with_body_sierra(func_id) else {
                                return;
                            };
                            for statement in &function.body {
                                let Some(related_function_id) =
                                    try_get_function_with_body_id(db, statement)
                                else {
                                    continue;
                                };

                                let snapshot = salsa::ParallelDatabase::snapshot(&*snapshot);
                                s.spawn(move |_| {
                                    handle_func_inner(
                                        processed_function_ids,
                                        snapshot,
                                        related_function_id,
                                    )
                                })
                            }
                        });
                    }
                }

                handle_func_inner(processed_function_ids, snapshot, func_id)
            });
        }
    });
}

///  Checks if there are diagnostics in the database and if there are None, returns
///  the [SierraProgramWithDebug] object of the requested functions
pub fn get_sierra_program_for_functions(
    db: &RootDatabase,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId>,
    context: DbWarmupContext,
) -> Result<Arc<SierraProgramWithDebug>> {
    context.warmup_db(db, requested_function_ids.clone());
    db.get_sierra_program_for_functions(requested_function_ids)
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")
}

/// Runs Cairo compiler.
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
pub fn compile_prepared_db_program_artifact(
    db: &mut RootDatabase,
    main_crate_ids: Vec<CrateId>,
    mut compiler_config: CompilerConfig<'_>,
) -> Result<ProgramArtifact> {
    let add_statements_functions = compiler_config.add_statements_functions;
    let add_statements_code_locations = compiler_config.add_statements_code_locations;

    compiler_config.diagnostics_reporter.ensure(db)?;

    let executable_functions = find_executable_function_ids(db, main_crate_ids.clone());

    let mut sierra_program_with_debug = if executable_functions.is_empty() {
        // No executables found - compile for all main crates.
        // TODO(maciektr): Deprecate in future. This compilation is useless, without `replace_ids`.
        Arc::unwrap_or_clone(
            db.get_sierra_program(main_crate_ids)
                .to_option()
                .context("Compilation failed without any diagnostics")?,
        )
    } else {
        // Compile for executable functions only.
        Arc::unwrap_or_clone(
            db.get_sierra_program_for_functions(executable_functions.clone().into_keys().collect())
                .to_option()
                .context("Compilation failed without any diagnostics")?,
        )
    };

    if compiler_config.replace_ids {
        sierra_program_with_debug.program =
            replace_sierra_ids_in_program(db, &sierra_program_with_debug.program);
    }

    let mut annotations = Annotations::default();

    if add_statements_functions {
        annotations.extend(Annotations::from(
            sierra_program_with_debug
                .debug_info
                .statements_locations
                .extract_statements_functions(db),
        ))
    };

    if add_statements_code_locations {
        annotations.extend(Annotations::from(
            sierra_program_with_debug
                .debug_info
                .statements_locations
                .extract_statements_source_code_locations(db),
        ))
    };

    let debug_info = DebugInfo {
        type_names: Default::default(),
        libfunc_names: Default::default(),
        user_func_names: Default::default(),
        annotations,
        executables: Default::default(),
    };

    // Calculate executable function Sierra ids.
    let executables =
        collect_executables(db, executable_functions, &sierra_program_with_debug.program);

    Ok(ProgramArtifact::stripped(sierra_program_with_debug.program)
        .with_debug_info(DebugInfo { executables, ..debug_info }))
}
