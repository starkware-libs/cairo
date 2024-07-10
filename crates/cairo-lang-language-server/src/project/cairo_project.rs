//! Query implementations for the `cairo_project.toml`-based projects.

use std::sync::Arc;

use anyhow::Context;
use cairo_lang_project::ProjectConfig;
use cairo_lang_utils::LookupIntern;
use tracing::error;

use crate::project::main::{LsProjectsGroup, ProjectId};
use crate::project::project_manifest_path::ProjectManifestPath;
use crate::project::Crate;

/// Gets the list of crates from a `cairo_project.toml`-based project.
///
/// The `cairo_project.toml` file is straightforward and self-descriptive enough to not be needed to
/// be cached in the database, hence it is read here directly and processed immediately.
pub fn project_crates(db: &dyn LsProjectsGroup, project: ProjectId) -> Arc<[Arc<Crate>]> {
    let ProjectManifestPath::CairoProject(manifest_path) = project.lookup_intern(db) else {
        unreachable!()
    };

    let Ok(project_config) = ProjectConfig::from_file(&manifest_path)
        .with_context(|| format!("failed to read cairo project: {}", manifest_path.display()))
        .inspect_err(|e| {
            // TODO(mkaput): Send a notification to the language client about the error.
            error!("{e:?}");
        })
    else {
        return [].into();
    };

    project_config
        .content
        .crate_roots
        .iter()
        .map(|(name, root)| {
            let name = name.clone();
            let root = project_config.absolute_crate_root(root);
            let settings = project_config.content.crates_config.get(&name).clone();
            Crate { name, root, custom_main_file_stem: None, settings }.into()
        })
        .collect::<Vec<_>>()
        .into()
}
