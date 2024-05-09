use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use scarb_metadata::Metadata;
use tracing::{error, trace, warn};

use super::Project;
use crate::toolchain::scarb::{ScarbToolchain, SCARB_LOCK, SCARB_TOML};

mod db;

/// A [`Project`] backed by a Scarb workspace (`Scarb.toml` file).
pub struct ScarbWorkspace {
    /// Path to the top-most Scarb manifest file (i.e., the **workspace** one).
    manifest_path: PathBuf,

    /// Scarb toolchain to use in this workspace.
    scarb_toolchain: ScarbToolchain,

    /// Path to the lock file of this workspace.
    lock_path: PathBuf,

    /// Last known revision of `scarb metadata` command output.
    metadata: Option<Metadata>,
}

impl ScarbWorkspace {
    /// Constructs a new [`ScarbWorkspace`] and triggers `scarb metadata` loading.
    ///
    /// The `manifest_path` argument can point to either a package `Scarb.toml` or a workspace one.
    /// If input package happens to be part of a broader workspace, this object will adapt to it.
    pub fn initialize(manifest_path: PathBuf, scarb_toolchain: &ScarbToolchain) -> Self {
        assert_eq!(manifest_path.file_name().and_then(|s| s.to_str()), Some(SCARB_TOML));

        let lock_path = manifest_path.with_file_name(SCARB_LOCK);
        let mut this = Self {
            manifest_path,
            scarb_toolchain: scarb_toolchain.clone(),
            lock_path,
            metadata: None,
        };

        this.do_reload();

        this
    }

    /// Calls `scarb metadata`, parses its result and stores it in the `metadata` field.
    // FIXME(mkaput): This should actually invoke `scarb metadata` in a background task and not
    //  block the main thread.
    #[tracing::instrument(level = "debug", skip_all, fields(manifest = ?self.manifest_path))]
    fn do_reload(&mut self) {
        let metadata = self
            .scarb_toolchain
            .metadata(&self.manifest_path)
            .with_context(|| {
                format!("failed to reload scarb workspace: {}", self.manifest_path.display())
            })
            .inspect_err(|e| {
                // TODO(mkaput): Send a notification to the language client about the error.
                error!("{e:?}");
            })
            .ok();

        if let Some(metadata) = metadata {
            // It is possible that this workspace was initialized with a path to workspace
            // member (we cannot be sure until we execute `scarb metadata`).
            // If this is true, change `manifest_path` to the real workspace manifest path.
            self.manifest_path = metadata.workspace.manifest_path.clone().into();

            self.lock_path = metadata.workspace.manifest_path.with_file_name(SCARB_LOCK).into();

            self.metadata = Some(metadata);
        }
    }
}

impl Project for ScarbWorkspace {
    fn manifest_files(&self) -> Vec<&Path> {
        let Some(metadata) = &self.metadata else {
            return vec![&self.manifest_path];
        };

        [metadata.workspace.manifest_path.as_std_path(), self.lock_path.as_path()]
            .into_iter()
            .chain(
                metadata
                    .workspace
                    .members
                    .iter()
                    .flat_map(|pkg_id| metadata.get_package(pkg_id))
                    .map(|pkg| pkg.manifest_path.as_std_path()),
            )
            .collect()
    }

    fn main_manifest_file(&self) -> &Path {
        match &self.metadata {
            Some(metadata) => metadata.workspace.manifest_path.as_std_path(),
            None => &self.manifest_path,
        }
    }

    fn reload(&mut self) {
        self.do_reload();
    }

    fn requires_unmanaged_core(&self) -> bool {
        self.metadata.is_none()
    }

    fn apply_db_changes(&self, db: &mut RootDatabase) {
        if let Some(metadata) = &self.metadata {
            db::update_crate_roots(metadata, db);
        } else {
            trace!("metadata has not been yet loaded, skipping");
        }
    }
}
