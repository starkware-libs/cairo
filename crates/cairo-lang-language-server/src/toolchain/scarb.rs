use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use anyhow::{bail, Context, Result};
use scarb_metadata::{Metadata, MetadataCommand};
use tower_lsp::lsp_types::notification::Notification;
use tracing::warn;

use crate::env_config;
use crate::server::notifier::Notifier;

pub const SCARB_TOML: &str = "Scarb.toml";
pub const SCARB_LOCK: &str = "Scarb.lock";

// TODO(mkaput): Collect `scarb --version` and display it in the editor UI.
/// The ultimate object for invoking Scarb.
///
/// This object tries to maintain good UX when doing any Scarb operations, for example, by sending
/// progress notifications to the language client.
///
/// This object is small and cheap to clone, so it can be passed around freely.
#[derive(Clone)]
pub struct ScarbToolchain {
    /// Cached path to the `scarb` executable.
    scarb_path_cell: Arc<OnceLock<Option<PathBuf>>>,
    /// The notifier object used to send notifications to the language client.
    notifier: Notifier,
}

impl ScarbToolchain {
    /// Constructs a new [`ScarbToolchain`].
    pub fn new(notifier: &Notifier) -> Self {
        ScarbToolchain { scarb_path_cell: Default::default(), notifier: notifier.clone() }
    }

    /// Finds the path to the `scarb` executable to use.
    ///
    /// This method may send notifications to the language client if there are any actionable issues
    /// with the found `scarb` installation, or if it could not be found.
    fn discover(&self) -> Option<&Path> {
        self.scarb_path_cell
            .get_or_init(|| {
                let path = env_config::scarb_path();
                // TODO(mkaput): Perhaps we should display this notification again after reloading?
                if path.is_none() {
                    warn!("attempt to use scarb without SCARB env being set");
                    self.notifier.send_notification::<ScarbPathMissing>(());
                }
                path
            })
            .as_ref()
            .map(PathBuf::as_path)
    }

    /// Calls `scarb metadata` for the given `Scarb.toml` and parse its output.
    ///
    /// This is a blocking operation that may be long-running. It should only be called from within
    /// a background task. The `scarb metadata` command performs workspace resolution, which does a
    /// lot of IO, including network requests (for fetching registry index and downloading
    /// packages).
    ///
    /// This method may send notifications to the language client, informing the user about
    /// the progress of the operation or any actionable issues.
    #[tracing::instrument(level = "debug", skip(self))]
    pub fn metadata(&self, manifest: &Path) -> Result<Metadata> {
        let Some(scarb_path) = self.discover() else {
            bail!("could not find scarb executable");
        };

        self.notifier.send_notification::<ScarbResolvingStart>(());

        let result = MetadataCommand::new()
            .scarb_path(scarb_path)
            .manifest_path(manifest)
            .inherit_stderr()
            .exec()
            .context("failed to execute: scarb metadata");

        self.notifier.send_notification::<ScarbResolvingFinish>(());

        result
    }
}

#[derive(Debug)]
struct ScarbPathMissing {}

impl Notification for ScarbPathMissing {
    type Params = ();
    const METHOD: &'static str = "scarb/could-not-find-scarb-executable";
}

#[derive(Debug)]
struct ScarbResolvingStart {}

impl Notification for ScarbResolvingStart {
    type Params = ();
    const METHOD: &'static str = "scarb/resolving-start";
}

#[derive(Debug)]
struct ScarbResolvingFinish {}

impl Notification for ScarbResolvingFinish {
    type Params = ();
    const METHOD: &'static str = "scarb/resolving-finish";
}
