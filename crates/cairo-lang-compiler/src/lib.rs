//! Cairo compiler.
//!
//! This crate is responsible for compiling a Cairo project into a Sierra program.
//! It is the main entry point for the compiler.
use std::path::Path;
use std::sync::Arc;

use ::cairo_lang_diagnostics::ToOption;
use anyhow::{bail, Context, Result};
use cairo_lang_filesystem::db::FilesGroupEx;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;

use crate::db::RootDatabase;
use crate::diagnostics::{check_diagnostics, eprint_diagnostic};
use crate::project::{get_main_crate_ids_from_project, setup_project, ProjectConfig};

pub mod db;
pub mod diagnostics;
pub mod project;

/// Configuration for the compiler.
pub struct CompilerConfig {
    pub on_diagnostic: Option<Box<dyn FnMut(String)>>,

    /// Replaces sierra ids with human-readable ones.
    pub replace_ids: bool,
}

/// The default compiler configuration.
impl Default for CompilerConfig {
    fn default() -> Self {
        CompilerConfig { on_diagnostic: Some(Box::new(eprint_diagnostic)), replace_ids: false }
    }
}

pub type SierraProgram = Arc<Program>;

/// Compiles a Cairo project at the given path.
/// The project must be a valid Cairo project:
/// Either a standalone `.cairo` file (a single crate), or a directory with a `cairo_project.toml`
/// file.
/// # Arguments
/// * `path` - The path to the project.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(SierraProgram)` - The compiled program.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_cairo_project_at_path(
    path: &Path,
    compiler_config: CompilerConfig,
) -> Result<SierraProgram> {
    let mut db = RootDatabase::default();
    let main_crate_ids = setup_project(&mut db, path)?;
    compile_prepared_db(db, main_crate_ids, compiler_config)
}

/// Compiles a Cairo project.
/// The project must be a valid Cairo project.
/// # Arguments
/// * `project_config` - The project configuration.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(SierraProgram)` - The compiled program.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile(
    project_config: ProjectConfig,
    compiler_config: CompilerConfig,
) -> Result<SierraProgram> {
    let mut db = RootDatabase::default();
    db.with_project_config(project_config.clone());
    let main_crate_ids = get_main_crate_ids_from_project(&mut db, &project_config);

    compile_prepared_db(db, main_crate_ids, compiler_config)
}

fn compile_prepared_db(
    mut db: RootDatabase,
    main_crate_ids: Vec<CrateId>,
    compiler_config: CompilerConfig,
) -> Result<SierraProgram> {
    if check_diagnostics(&mut db, compiler_config.on_diagnostic) {
        bail!("Compilation failed.");
    }

    let mut sierra_program = db
        .get_sierra_program(main_crate_ids)
        .to_option()
        .context("Compilation failed without any diagnostics")?;

    if compiler_config.replace_ids {
        sierra_program = Arc::new(replace_sierra_ids_in_program(&db, &sierra_program));
    }

    Ok(sierra_program)
}
