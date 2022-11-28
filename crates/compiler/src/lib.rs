use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{bail, Context, Result};
use filesystem::db::FilesGroupEx;
use sierra::program::Program;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;

use crate::db::RootDatabase;
use crate::diagnostics::check_diagnostics;
use crate::project::{get_main_crate_ids_from_project, setup_project, ProjectConfig};

pub mod db;
pub mod diagnostics;
pub mod project;

pub struct CompileArgs<'c> {
    pub compile: CompileProjectConfig,

    pub on_diagnostic: Option<&'c mut dyn FnMut(String)>,

    /// Replaces sierra ids with human-readable ones.
    pub replace_ids: bool,
}

pub enum CompileProjectConfig {
    LoadFromPath { path: PathBuf },
    Prepared { project_config: ProjectConfig },
}

pub type CompilerDatabase = RootDatabase;
pub type SierraProgram = Arc<Program>;

pub fn init_db() -> CompilerDatabase {
    RootDatabase::default()
}

pub fn compile(args: CompileArgs<'_>) -> Result<SierraProgram> {
    let mut db = RootDatabase::default();
    compile_with_db(&mut db, args)
}

pub fn compile_with_db(
    db: &mut CompilerDatabase,
    CompileArgs { compile, on_diagnostic, replace_ids }: CompileArgs<'_>,
) -> Result<SierraProgram> {
    let main_crate_ids = match compile {
        CompileProjectConfig::LoadFromPath { path } => setup_project(db, &path)?,
        CompileProjectConfig::Prepared { project_config } => {
            let main_crate_ids = get_main_crate_ids_from_project(db, &project_config);
            db.with_project_config(project_config);
            main_crate_ids
        }
    };

    if check_diagnostics(db, on_diagnostic) {
        bail!("Compilation failed.");
    }

    let mut sierra_program = db
        .get_sierra_program(main_crate_ids)
        .context("Compilation failed without any diagnostics")?;

    if replace_ids {
        sierra_program = Arc::new(replace_sierra_ids_in_program(db, &sierra_program));
    }

    Ok(sierra_program)
}
