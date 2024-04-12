use std::path::PathBuf;

use anyhow::{Context, Result};
use cairo_lang_filesystem::db::CrateSettings;
use cairo_lang_filesystem::ids::CrateLongId;
use tower_lsp::lsp_types::Url;

use crate::toolchain::scarb::ScarbToolchain;

const SCARB_PROJECT_FILE_NAME: &str = "Scarb.toml";

#[derive(Clone)]
pub struct ScarbService {
    scarb: ScarbToolchain,
}

impl ScarbService {
    pub fn new(scarb: &ScarbToolchain) -> Self {
        ScarbService { scarb: scarb.clone() }
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub fn crate_source_paths(
        &self,
        root_path: PathBuf,
    ) -> Result<Vec<(CrateLongId, PathBuf, CrateSettings)>> {
        let metadata =
            self.scarb.metadata(&root_path).context("Obtaining Scarb metadata for crate roots.")?;
        let crate_roots = metadata
            .compilation_units
            .into_iter()
            .flat_map(|unit| unit.components)
            .filter_map(|component| {
                let source_path: PathBuf = component.source_path.into();
                if source_path.exists() {
                    let crate_id = CrateLongId::Real(component.name.as_str().into());
                    let edition = metadata
                        .packages
                        .iter()
                        .find(|package| package.id == component.package)
                        .and_then(|package| {
                            package
                                .edition
                                .clone()
                                .map(|edition| serde_json::from_value(edition.into()).unwrap())
                        })
                        .unwrap_or_default();
                    Some((
                        crate_id,
                        source_path,
                        // TODO(ilya): Get experimental features from Scarb.
                        CrateSettings {
                            edition,
                            cfg_set: Default::default(),
                            experimental_features: Default::default(),
                        },
                    ))
                } else {
                    None
                }
            })
            .collect();
        Ok(crate_roots)
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub fn corelib_path(&self, root_path: PathBuf) -> Result<Option<PathBuf>> {
        let metadata = self
            .scarb
            .metadata(&root_path)
            .context("Obtaining Scarb metadata for corelib path.")?;
        let corelib_package = metadata
            .compilation_units
            .into_iter()
            .flat_map(|unit| unit.components)
            .filter(|component| component.source_root().exists())
            .find(|component| component.name == "core")
            .map(|component| component.source_root().into());
        Ok(corelib_package)
    }
}

pub fn is_scarb_manifest_path(file_path: &Url) -> bool {
    file_path.path().ends_with(SCARB_PROJECT_FILE_NAME)
}
