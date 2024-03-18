use std::env;
use std::path::PathBuf;

use anyhow::{Context, Result};
use cairo_lang_filesystem::db::CrateSettings;
use cairo_lang_filesystem::ids::CrateLongId;
use scarb_metadata::Metadata;
use tower_lsp::lsp_types::Url;
use tower_lsp::Client;

use crate::{ScarbResolvingFinish, ScarbResolvingStart};

const MAX_CRATE_DETECTION_DEPTH: usize = 20;
const SCARB_PROJECT_FILE_NAME: &str = "Scarb.toml";

pub struct ScarbService {
    scarb_path: Option<PathBuf>,
    client: Client,
}

impl ScarbService {
    pub fn new(client: &Client) -> Self {
        let scarb_path = env::var_os("SCARB").map(PathBuf::from);
        ScarbService { scarb_path, client: client.clone() }
    }

    fn scarb_path(&self) -> Option<PathBuf> {
        self.scarb_path.clone()
    }

    pub fn is_scarb_found(&self) -> bool {
        self.scarb_path.is_some()
    }

    #[tracing::instrument(level = "trace", skip_all)]
    pub fn is_scarb_project(&self, root_path: PathBuf) -> bool {
        self.scarb_manifest_path(root_path).is_some()
    }

    #[tracing::instrument(level = "trace", skip_all)]
    fn scarb_manifest_path(&self, root_path: PathBuf) -> Option<PathBuf> {
        let mut path = root_path;
        for _ in 0..MAX_CRATE_DETECTION_DEPTH {
            path.pop();
            let manifest_path = path.join(SCARB_PROJECT_FILE_NAME);
            if manifest_path.exists() {
                return Some(manifest_path);
            };
        }
        None
    }

    #[tracing::instrument(level = "trace", skip_all)]
    fn get_scarb_metadata(&self, root_path: PathBuf) -> Result<Metadata> {
        let manifest_path = self
            .scarb_manifest_path(root_path)
            .expect("Scarb metadata called outside of a Scarb project.");
        let scarb_path =
            self.scarb_path().expect("Scarb metadata called outside of a Scarb project.");
        scarb_metadata::MetadataCommand::new()
            .scarb_path(scarb_path)
            .manifest_path(manifest_path)
            .inherit_stderr()
            .exec()
            .context("Failed to obtain Scarb metadata.")
            .map_err(Into::into)
    }

    /// Reads Scarb project metadata from manifest file.
    #[tracing::instrument(level = "debug", skip(self))]
    pub async fn scarb_metadata(&self, root_path: PathBuf) -> Result<Metadata> {
        self.client.send_notification::<ScarbResolvingStart>(()).await;
        let result = self.get_scarb_metadata(root_path);
        self.client.send_notification::<ScarbResolvingFinish>(()).await;
        result
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub async fn crate_source_paths(
        &self,
        root_path: PathBuf,
    ) -> Result<Vec<(CrateLongId, PathBuf, CrateSettings)>> {
        let metadata = self
            .scarb_metadata(root_path)
            .await
            .context("Obtaining Scarb metadata for crate roots.")?;
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
    pub async fn corelib_path(&self, root_path: PathBuf) -> Result<Option<PathBuf>> {
        let metadata = self
            .scarb_metadata(root_path)
            .await
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
