use std::env;
use std::path::PathBuf;

use anyhow::Result;
use cairo_lang_filesystem::ids::{CrateLongId, Directory};
use scarb_metadata::Metadata;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;
const SCARB_PROJECT_FILE_NAME: &str = "Scarb.toml";

pub struct ScarbService {
    scarb_path: Option<PathBuf>,
}

impl ScarbService {
    fn scarb_path(&self) -> Option<PathBuf> {
        self.scarb_path.clone()
    }

    pub fn is_scarb_found(&self) -> bool {
        self.scarb_path.is_some()
    }

    pub fn is_scarb_project(&self, root_path: PathBuf) -> bool {
        self.scarb_manifest_path(root_path).is_some()
    }

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

    /// Reads Scarb project metadata from manifest file.
    pub fn scarb_metadata(&self, root_path: PathBuf) -> Result<Metadata> {
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
            .map_err(Into::into)
    }

    pub fn crate_roots(&self, root_path: PathBuf) -> Result<Vec<(CrateLongId, Directory)>> {
        let metadata = self.scarb_metadata(root_path)?;
        let crate_roots = metadata
            .compilation_units
            .into_iter()
            .flat_map(|unit| unit.components)
            .filter(|component| component.source_root().exists())
            .map(|component| {
                let crate_id = CrateLongId(component.name.as_str().into());
                let directory = Directory(component.source_root().into());
                (crate_id, directory)
            })
            .collect();
        Ok(crate_roots)
    }

    pub fn corelib_path(&self, root_path: PathBuf) -> Result<Option<PathBuf>> {
        let metadata = self.scarb_metadata(root_path)?;
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

impl Default for ScarbService {
    fn default() -> Self {
        let scarb_path = env::var_os("SCARB").map(PathBuf::from);
        ScarbService { scarb_path }
    }
}
