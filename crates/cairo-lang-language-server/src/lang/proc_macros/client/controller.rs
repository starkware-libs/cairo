use std::num::NonZeroU32;
use std::time::Duration;

use anyhow::{Context, Result, anyhow};
use cairo_lang_semantic::plugin::PluginSuite;
use governor::clock::QuantaClock;
use governor::state::{InMemoryState, NotKeyed};
use governor::{Quota, RateLimiter};
use scarb_proc_macro_server_types::jsonrpc::RpcResponse;
use scarb_proc_macro_server_types::methods::ProcMacroResult;
use tracing::error;

use super::connection::ProcMacroServerConnection;
use super::status::{ClientStatus, ProcMacroClientStatusChange};
use super::{ProcMacroClient, RequestParams};
use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::lang::proc_macros::client::status::ClientStatusChange;
use crate::lang::proc_macros::db::ProcMacroCacheGroup;
use crate::lang::proc_macros::plugins::proc_macro_plugin_suite;
use crate::lsp::ext::ProcMacroServerInitializationFailed;
use crate::server::client::Notifier;
use crate::toolchain::scarb::ScarbToolchain;

/// Manages lifecycle of proc-macro-server client.
pub struct ProcMacroClientController {
    status_change: ProcMacroClientStatusChange,
    notifier: Notifier,
    scarb: ScarbToolchain,
    plugin_suite: Option<PluginSuite>,
    initialization_retries: RateLimiter<NotKeyed, InMemoryState, QuantaClock>,
}

impl ProcMacroClientController {
    pub fn new(notifier: Notifier, scarb: ScarbToolchain) -> Self {
        Self {
            status_change: Default::default(),
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
        }
    }

    /// Start proc-macro-server after config reload.
    /// Note that this will only try to go from `ClientStatus::Disabled` to
    /// `ClientStatus::Initializing` if config allows this.
    pub fn try_initialize_if_disabled(&mut self, db: &mut AnalysisDatabase, config: &Config) {
        if db.proc_macro_client_status().disabled() {
            self.try_initialize(db, config);
        }
    }

    /// Tries starting proc-macro-server initialization process, if allowed by config.
    ///
    /// Returns value indicating if initialization was attempted.
    pub fn try_initialize(&mut self, db: &mut AnalysisDatabase, config: &Config) -> bool {
        // Do not initialize if not yet received client config (None) or received `true`.
        // Keep the rate limiter check as second condition if when config doesn't allow it to make
        // sure it is not impacted.
        let initialize = config.disable_proc_macros == Some(false)
            && self.initialization_retries.check().is_ok();

        if initialize {
            self.spawn_server(db);
        }

        initialize
    }

    /// Spawns proc-macro-server.
    fn spawn_server(&mut self, db: &mut AnalysisDatabase) {
        match self.scarb.proc_macro_server() {
            Ok(proc_macro_server) => {
                db.set_proc_macro_client_status(ClientStatus::Initializing);

                let client = ProcMacroClient::new(
                    ProcMacroServerConnection::new(proc_macro_server),
                    self.status_change.clone(),
                );

                // `initialize` is blocking.
                std::thread::spawn(move || client.initialize());
            }
            Err(err) => {
                error!("spawning proc-macro-server failed: {err:?}");

                self.status_change.update(ClientStatusChange::FatalFailed);
            }
        }
    }

    /// Check if there was status change reported and applies it.
    /// If client is ready applies all available responses.
    pub fn maybe_update_and_apply_responses(&mut self, db: &mut AnalysisDatabase, config: &Config) {
        if let Some(change) = self.status_change.changed() {
            self.update_status(db, config, change);
        };

        // TODO we should check here if there are live snapshots, but no idea if it is possible with
        // salsa public api. if there are snapshots running we can skip this job, this will lead to
        // more updates at once later and less cancellation.
        if let Some(client) = db.proc_macro_client_status().ready() {
            self.apply_responses(db, client);
        };
    }

    /// Process status change update.
    fn update_status(
        &mut self,
        db: &mut AnalysisDatabase,
        config: &Config,
        status: ClientStatusChange,
    ) {
        match status {
            ClientStatusChange::Failed if self.try_initialize(db, config) => {}
            ClientStatusChange::Failed | ClientStatusChange::FatalFailed => {
                db.set_proc_macro_client_status(ClientStatus::FailedToInitialize);

                self.notifier.notify::<ProcMacroServerInitializationFailed>(());
            }
            ClientStatusChange::Ready(defined_macros, client) => {
                let new_plugin_suite = proc_macro_plugin_suite(defined_macros);

                // Store current plugins for identity comparison, so we can remove them if we
                // restart proc-macro-server.
                let previous_plugin_suite = self.plugin_suite.replace(new_plugin_suite.clone());

                db.replace_plugin_suite(previous_plugin_suite, new_plugin_suite);

                db.set_proc_macro_client_status(ClientStatus::Ready(client));
            }
        }
    }

    /// Process proc-macro-server responses by updating resolutions.
    pub fn apply_responses(&self, db: &mut AnalysisDatabase, client: &ProcMacroClient) {
        let mut attribute_resolutions = db.attribute_macro_resolution();
        let mut attribute_resolutions_changed = false;

        let mut derive_resolutions = db.derive_macro_resolution();
        let mut derive_resolutions_changed = false;

        let mut inline_macro_resolutions = db.inline_macro_resolution();
        let mut inline_macro_resolutions_changed = false;

        let mut requests = client.requests_params.lock().unwrap();

        for response in client.available_responses() {
            // TODO investigate this as it somehow panicks if there is \0 problem.
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

                    self.status_change.update(ClientStatusChange::Failed);
                }
            }
        }

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
