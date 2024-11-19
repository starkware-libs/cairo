use std::num::NonZeroU32;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result, anyhow};
use cairo_lang_semantic::plugin::PluginSuite;
use crossbeam::channel::{Receiver, Sender};
use governor::clock::QuantaClock;
use governor::state::{InMemoryState, NotKeyed};
use governor::{Quota, RateLimiter};
use scarb_proc_macro_server_types::jsonrpc::RpcResponse;
use scarb_proc_macro_server_types::methods::ProcMacroResult;
use tracing::error;

use super::client::connection::ProcMacroServerConnection;
use super::client::status::ClientStatus;
use super::client::{ProcMacroClient, RequestParams};
use crate::lang::db::AnalysisDatabase;
use crate::lang::proc_macros::db::ProcMacroGroup;
use crate::lang::proc_macros::plugins::proc_macro_plugin_suite;
use crate::lsp::ext::{
    ProcMacroServerInitializationFailed, ProcMacroServerInitializationFailedParams,
};
use crate::server::client::Notifier;
use crate::toolchain::scarb::ScarbToolchain;

/// Manages lifecycle of proc-macro-server client.
pub struct ProcMacroClientController {
    notifier: Notifier,
    scarb: ScarbToolchain,
    plugin_suite: Option<PluginSuite>,
    initialization_retries: RateLimiter<NotKeyed, InMemoryState, QuantaClock>,
    channels: Option<ProcMacroChannelsSenders>,
}

impl ProcMacroClientController {
    pub fn init_channels(&mut self) -> (Receiver<()>, Receiver<()>) {
        let (response_sender, response_receiver) = crossbeam::channel::bounded(1);
        let (error_sender, error_receiver) = crossbeam::channel::bounded(1);

        self.channels =
            Some(ProcMacroChannelsSenders { error: error_sender, response: response_sender });

        (error_receiver, response_receiver)
    }

    pub fn new(notifier: Notifier, scarb: ScarbToolchain) -> Self {
        Self {
            notifier,
            scarb,
            plugin_suite: Default::default(),
            initialization_retries: RateLimiter::direct(
                Quota::with_period(
                    Duration::from_secs(180 / 5), // Across 3 minutes (180 seconds) / 5 retries.
                )
                .unwrap()
                .allow_burst(
                    NonZeroU32::new(5).unwrap(), // All 5 retries can be used as fast as possible.
                ),
            ),
            channels: Default::default(),
        }
    }

    /// Start proc-macro-server.
    /// Note that this will only try to go from `ClientStatus::Uninitialized` to
    /// `ClientStatus::Initializing`.
    pub fn initialize_once(&mut self, db: &mut AnalysisDatabase) {
        if db.proc_macro_client_status().is_uninitialized() {
            self.try_initialize(db);
        }
    }

    /// Tries starting proc-macro-server initialization process.
    ///
    /// Returns value indicating if initialization was attempted.
    pub fn try_initialize(&mut self, db: &mut AnalysisDatabase) -> bool {
        let initialize = self.initialization_retries.check().is_ok();

        if initialize {
            self.spawn_server(db);
        }

        initialize
    }

    /// Spawns proc-macro-server.
    fn spawn_server(&mut self, db: &mut AnalysisDatabase) {
        match self.scarb.proc_macro_server() {
            Ok(proc_macro_server) => {
                let channels = self.channels.clone().unwrap();

                let client = ProcMacroClient::new(
                    ProcMacroServerConnection::new(proc_macro_server, channels.response),
                    channels.error,
                );

                client.start_initialize();

                db.set_proc_macro_client_status(ClientStatus::Initializing(Arc::new(client)));
            }
            Err(err) => {
                error!("spawning proc-macro-server failed: {err:?}");

                self.fatal_failed(db, ProcMacroServerInitializationFailedParams::SpawnFail)
            }
        }
    }

    fn fatal_failed(
        &self,
        db: &mut AnalysisDatabase,
        params: ProcMacroServerInitializationFailedParams,
    ) {
        db.set_proc_macro_client_status(ClientStatus::FailedToInitialize);

        self.notifier.notify::<ProcMacroServerInitializationFailed>(params);
    }

    pub fn handle_error(&mut self, db: &mut AnalysisDatabase) {
        if !self.try_initialize(db) {
            self.fatal_failed(db, ProcMacroServerInitializationFailedParams::NoMoreRetries);
        }
    }

    /// Check if there was error reported, if so try to restart.
    /// If client is ready applies all available responses.
    pub fn on_response(&mut self, db: &mut AnalysisDatabase) {
        match db.proc_macro_client_status() {
            ClientStatus::Initializing(client) => {
                let Ok(defined_macros) = client.finish_initialize() else {
                    self.handle_error(db);

                    return;
                };

                let new_plugin_suite = proc_macro_plugin_suite(defined_macros);

                // Store current plugins for identity comparison, so we can remove them if we
                // restart proc-macro-server.
                let previous_plugin_suite = self.plugin_suite.replace(new_plugin_suite.clone());

                db.replace_plugin_suite(previous_plugin_suite, new_plugin_suite);

                db.set_proc_macro_client_status(ClientStatus::Ready(client));
            }
            ClientStatus::Ready(client) => {
                // TODO we should check here if there are live snapshots, but no idea if it is
                // possible with salsa public api. if there are snapshots running we
                // can skip this job, this will lead to more updates at once later
                // and less cancellation.
                self.apply_responses(db, &client);
            }
            _ => {}
        }
    }

    /// Process proc-macro-server responses by updating resolutions.
    pub fn apply_responses(&mut self, db: &mut AnalysisDatabase, client: &ProcMacroClient) {
        let mut attribute_resolutions = db.attribute_macro_resolution();
        let mut attribute_resolutions_changed = false;

        let mut derive_resolutions = db.derive_macro_resolution();
        let mut derive_resolutions_changed = false;

        let mut inline_macro_resolutions = db.inline_macro_resolution();
        let mut inline_macro_resolutions_changed = false;

        client.for_available_responses(|response, requests| {
            let params = requests.remove(&response.id).unwrap();

            match parse_proc_macro_response(response) {
                Ok(result) => {
                    match params {
                        RequestParams::Attribute(params) => {
                            attribute_resolutions.insert(params, result);
                            attribute_resolutions_changed = true;
                        }
                        RequestParams::Derive(params) => {
                            derive_resolutions.insert(params, result);
                            derive_resolutions_changed = true;
                        }
                        RequestParams::Inline(params) => {
                            inline_macro_resolutions.insert(params, result);
                            inline_macro_resolutions_changed = true;
                        }
                    };
                }
                Err(error) => {
                    error!("{error:#?}");

                    self.handle_error(db)
                }
            }
        });

        // Set input only if resolution changed, this way we don't recompute queries if there were
        // no updates.
        if attribute_resolutions_changed {
            db.set_attribute_macro_resolution(attribute_resolutions);
        }
        if derive_resolutions_changed {
            db.set_derive_macro_resolution(derive_resolutions);
        }
        if inline_macro_resolutions_changed {
            db.set_inline_macro_resolution(inline_macro_resolutions);
        }
    }
}

fn parse_proc_macro_response(response: RpcResponse) -> Result<ProcMacroResult> {
    let success = response
        .into_result()
        .map_err(|error| anyhow!("proc-macro-server responded with error: {error:?}"))?;

    serde_json::from_value(success).context("failed to deserialize response into `ProcMacroResult`")
}

#[derive(Debug, Clone)]
struct ProcMacroChannelsSenders {
    // Single element queue used for notifying when responses queue is pushed.
    response: Sender<()>,

    // Single element queue used for notifying when client occurred an error.
    error: Sender<()>,
}
