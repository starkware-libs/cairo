use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{FilesGroup, FilesGroupEx};
use filesystem::ids::FileLongId;
use lowering::db::LoweringGroup;
use parser::db::ParserGroup;
use semantic::db::SemanticGroup;
use sierra::program::Program;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;

use crate::db::RootDatabase;
use crate::project::{get_main_crate_ids_from_project, ProjectConfig, setup_project};

pub mod db;
pub mod project;

pub struct CompilerConfig {
    pub on_diagnostic: Option<Box<dyn FnMut(String)>>,

    /// Replaces sierra ids with human-readable ones.
    pub replace_ids: bool,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        CompilerConfig {
            on_diagnostic: Some(Box::new(eprint_diagnostic)),
            replace_ids: false,
        }
    }
}

pub enum Setup {
    Path(PathBuf),
    ProjectConfig(ProjectConfig),
}

pub type SierraProgram = Arc<Program>;

pub struct CompilerDatabase {
    db: RootDatabase,
    config: CompilerConfig,
}

impl CompilerDatabase {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            db: RootDatabase::default(),
            config,
        }
    }

    pub fn with_default_config() -> Self {
        Self::new(CompilerConfig::default())
    }

    pub fn upcast(&self) -> &RootDatabase { &self.db }
    pub fn upcast_mut(&mut self) -> &mut RootDatabase { &mut self.db }

    pub fn compile(&mut self, setup: Setup) -> Result<SierraProgram> {
        let main_crate_ids = match setup {
            Setup::Path(path) => setup_project(&mut self.db, &path)?,
            Setup::ProjectConfig(project_config) => {
                let main_crate_ids = get_main_crate_ids_from_project(&mut self.db, &project_config);
                self.db.with_project_config(project_config);
                main_crate_ids
            }
        };

        if self.check_diagnostics() {
            bail!("Compilation failed.");
        }

        let mut sierra_program = self.db
            .get_sierra_program(main_crate_ids)
            .context("Compilation failed without any diagnostics")?;

        if self.config.replace_ids {
            sierra_program = Arc::new(replace_sierra_ids_in_program(&self.db, &sierra_program));
        }

        Ok(sierra_program)
    }

    /// Checks if there are diagnostics and reports them to `CompilerConfig::on_diagnostic`
    /// as strings.
    ///
    /// # Returns
    ///
    /// Returns `true` if diagnostics were found.
    pub fn check_diagnostics(&mut self) -> bool {
        let mut ignore_diagnostic = |_| ();
        let on_diagnostic = self.config.on_diagnostic.as_deref_mut().unwrap_or(&mut ignore_diagnostic);

        let mut found_diagnostics = false;
        for crate_id in self.db.crates() {
            for module_id in &*self.db.crate_modules(crate_id) {
                for file_id in self.db.module_files(*module_id).unwrap_or_default() {
                    if self.db.file_content(file_id).is_none() {
                        if let ModuleId::CrateRoot(_) = *module_id {
                            match self.db.lookup_intern_file(file_id) {
                                FileLongId::OnDisk(path) => {
                                    on_diagnostic(format!("{} not found", path.display()))
                                }
                                FileLongId::Virtual(_) => panic!("Missing virtual file."),
                            }
                            found_diagnostics = true;
                        }
                    } else {
                        let diag = self.db.file_syntax_diagnostics(file_id);
                        if !diag.get_all().is_empty() {
                            found_diagnostics = true;
                            on_diagnostic(diag.format(&self.db));
                        }
                    }
                }

                if let Some(diag) = self.db.module_semantic_diagnostics(*module_id) {
                    if !diag.get_all().is_empty() {
                        found_diagnostics = true;
                        on_diagnostic(diag.format(&self.db));
                    }
                }

                if let Some(diag) = self.db.module_lowering_diagnostics(*module_id) {
                    if !diag.get_all().is_empty() {
                        found_diagnostics = true;
                        on_diagnostic(diag.format(&self.db));
                    }
                }

                let diag = self.db.module_sierra_diagnostics(*module_id);
                if !diag.get_all().is_empty() {
                    found_diagnostics = true;
                    on_diagnostic(diag.format(&self.db));
                }
            }
        }
        found_diagnostics
    }
}

fn eprint_diagnostic(diag: String) {
    eprint!("{}", diag);
}
