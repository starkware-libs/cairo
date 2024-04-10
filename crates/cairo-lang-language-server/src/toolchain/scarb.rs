use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use anyhow::{bail, Context, Result};
use scarb_metadata::{Metadata, MetadataCommand};
use tower_lsp::lsp_types::notification::Notification;
use tracing::warn;

use crate::env_config;
use crate::server::notification_sender::Notifier;

/// The ultimate object for invoking Scarb.
///
/// This object tries to maintain good UX when doing any Scarb operations, for example, by sending
/// progress notifications to the language client.
///
/// This object is small and cheap to clone, so it can be passed around freely.
#[derive(Clone)]
pub struct Scarb {
    scarb_path_cell: Arc<OnceLock<Option<PathBuf>>>,
    notifier: Notifier,
}

impl Scarb {
    pub fn new(notifier: &Notifier) -> Self {
        Scarb { scarb_path_cell: Default::default(), notifier: notifier.clone() }
    }

    fn scarb_path(&self) -> Option<&Path> {
        self.scarb_path_cell
            .get_or_init(|| {
                let path = env_config::scarb_path();
                if path.is_none() {
                    warn!("attempt to use scarb without SCARB env being set");
                    self.notifier.send_notification::<ScarbPathMissing>(());
                }
                path
            })
            .as_ref()
            .map(PathBuf::as_path)
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub fn metadata(&self, manifest: &Path) -> Result<Metadata> {
        let Some(scarb_path) = self.scarb_path() else {
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
