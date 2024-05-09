use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::update_crate_roots_from_project_config;
use cairo_lang_project::{ProjectConfig, PROJECT_FILE_NAME};
use tracing::error;

use super::Project;

/// A [`Project`] manually specified using a `cairo_project.toml` file.
pub struct CairoProject {
    /// Path to the `cairo_project.toml` file.p
    project_path: PathBuf,

    /// Last known revision of `cairo_project.toml` contents, parsed.
    project_config: Option<ProjectConfig>,
}

impl CairoProject {
    /// Constructs a new [`CairoProject`] and loads it.
    pub fn initialize(project_path: PathBuf) -> Self {
        assert_eq!(project_path.file_name().and_then(|s| s.to_str()), Some(PROJECT_FILE_NAME));
        let mut this = Self { project_path, project_config: None };
        this.reload();
        this
    }
}

impl Project for CairoProject {
    fn manifest_files(&self) -> Vec<&Path> {
        vec![&self.project_path]
    }

    fn main_manifest_file(&self) -> &Path {
        &self.project_path
    }

    fn reload(&mut self) {
        let project_config = ProjectConfig::from_file(&self.project_path)
            .with_context(|| {
                format!("failed to reload cairo project: {}", self.project_path.display())
            })
            .inspect_err(|e| {
                // TODO(mkaput): Send a notification to the language client about the error.
                error!("{e:?}");
            })
            .ok();

        if project_config.is_some() {
            self.project_config = project_config;
        }
    }

    fn requires_unmanaged_core(&self) -> bool {
        true
    }

    fn apply_db_changes(&self, db: &mut RootDatabase) {
        if let Some(project_config) = self.project_config.as_ref() {
            update_crate_roots_from_project_config(db, project_config);
        }
    }
}
