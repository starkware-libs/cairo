use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, OnceLock};

use anyhow::{Context, Result, bail};
use lsp_types::notification::Notification;
use scarb_metadata::{Metadata, MetadataCommand};
use tracing::{error, warn};

use crate::env_config;
use crate::server::client::Notifier;

pub const SCARB_TOML: &str = "Scarb.toml";

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

    /// States whether this instance is in _silent mode_.
    ///
    /// See [`ScarbToolchain::silent`] for more info.
    is_silent: bool,
}

impl ScarbToolchain {
    /// Constructs a new [`ScarbToolchain`].
    pub fn new(notifier: Notifier) -> Self {
        ScarbToolchain { scarb_path_cell: Default::default(), notifier, is_silent: false }
    }

    /// Finds the path to the `scarb` executable to use.
    ///
    /// This method may send notifications to the language client if there are any actionable issues
    /// with the found `scarb` installation or if it could not be found.
    fn discover(&self) -> Option<&Path> {
        self.scarb_path_cell
            .get_or_init(|| {
                let path = env_config::scarb_path();
                // TODO(mkaput): Perhaps we should display this notification again after reloading?
                if path.is_none() {
                    if self.is_silent {
                        // If we are in silent mode, then missing Scarb is probably dealt with
                        // at the caller site.
                        warn!("attempt to use scarb without SCARB env being set");
                    } else {
                        error!("attempt to use scarb without SCARB env being set");
                        self.notifier.notify::<ScarbPathMissing>(());
                    }
                }
                path
            })
            .as_ref()
            .map(PathBuf::as_path)
    }

    /// Creates a clone instance of this object that will be in _silent mode_.
    ///
    /// Silent mode means that any operations invoked through this instance should avoid performing
    /// any user-visible actions.
    pub fn silent(&self) -> Self {
        if self.is_silent {
            // Going silent from silent is noop, so skip any shenanigans we do here.
            self.clone()
        } else {
            Self {
                // Disassociate this instance from the shared path cell if it has not been
                // initialized yet.
                //
                // This maintains a good UX for the following scenario (timeline):
                // 1. CairoLS is started without a path to Scarb provided.
                // 2. Some internal operation is silently attempting to query Scarb, which will
                //    initialize the cell but only log a warning.
                // 3. User-invoked operation makes an attempt to query Scarb.
                //
                // At this point we want to show missing Scarb notification,
                // but without this trick we would never do
                // as the path cell would be already initialized.
                scarb_path_cell: match self.scarb_path_cell.get() {
                    Some(_) => self.scarb_path_cell.clone(),
                    None => Default::default(),
                },

                notifier: self.notifier.clone(),

                is_silent: true,
            }
        }
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
    #[tracing::instrument(skip(self))]
    pub fn metadata(&self, manifest: &Path) -> Result<Metadata> {
        let Some(scarb_path) = self.discover() else {
            bail!("could not find scarb executable");
        };

        if !self.is_silent {
            self.notifier.notify::<ScarbResolvingStart>(());
        }

        let result = MetadataCommand::new()
            .scarb_path(scarb_path)
            .manifest_path(manifest)
            .inherit_stderr()
            .exec()
            .context("failed to execute: scarb metadata");

        if !self.is_silent {
            self.notifier.notify::<ScarbResolvingFinish>(());
        }

        result
    }

    pub fn proc_macro_server(&self) -> Result<Child> {
        let Some(scarb_path) = self.discover() else { bail!("failed to get scarb path") };

        let proc_macro_server = Command::new(scarb_path)
            .arg("--quiet") // If not set scarb will print all "Compiling ..." messages we don't need (and these can crash input parsing).
            .arg("proc-macro-server")
            .envs(std::env::var("RUST_BACKTRACE").map(|value| ("RUST_BACKTRACE", value)))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            // We use this channel for debugging.
            .stderr(Stdio::inherit())
            .spawn()?;

        Ok(proc_macro_server)
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
