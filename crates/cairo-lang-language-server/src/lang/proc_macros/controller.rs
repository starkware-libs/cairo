use std::num::NonZeroU32;
use std::sync::Arc;
use std::time::Duration;

use crossbeam::channel::{Receiver, Sender};
use governor::clock::QuantaClock;
use governor::state::{InMemoryState, NotKeyed};
use governor::{Quota, RateLimiter};
use tracing::error;

use super::client::ProcMacroClient;
use super::client::connection::ProcMacroServerConnection;
use super::client::status::ClientStatus;
use crate::lang::db::AnalysisDatabase;
use crate::lang::proc_macros::db::ProcMacroGroup;
use crate::toolchain::scarb::ScarbToolchain;

/// Manages lifecycle of proc-macro-server client.
//                 +---------------------------+
//                 | ProcMacroClientController |
//                 +---------------------------+
//                               |
//                        +------+-----+
//                        | initialize |
//                        +------------+
//                               |
//                       +-------v-------+
//            +----------| LS main Loop  |----------------+
//            |          +---------------+                |
//            |                                           |
//  +---------v----------+                      +---------v-------+
//  | Check for response |                      | Check for error |
//  +--------------------+                      +-----------------+
//            |                                           |
//            | true                                      | true
//            |                                           |
//  +---------v-----------------------+         +---------v------+
//  | Process all available responses |         | restart server |
//  +---------------------------------+         +----------------+
pub struct ProcMacroClientController {
    scarb: ScarbToolchain,
    initialization_retries: RateLimiter<NotKeyed, InMemoryState, QuantaClock>,
    channels: Option<ProcMacroChannelsSenders>,
}

impl ProcMacroClientController {
    pub fn init_channels(&mut self) -> ProcMacroChannelsReceivers {
        let (response_sender, response_receiver) = crossbeam::channel::bounded(1);
        let (error_sender, error_receiver) = crossbeam::channel::bounded(1);

        self.channels =
            Some(ProcMacroChannelsSenders { error: error_sender, response: response_sender });

        ProcMacroChannelsReceivers { error: error_receiver, response: response_receiver }
    }

    pub fn new(scarb: ScarbToolchain) -> Self {
        Self {
            scarb,
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
    /// Note that this will only try to go from `ClientStatus::Pending` to
    /// `ClientStatus::Starting`.
    pub fn initialize_once(&mut self, db: &mut AnalysisDatabase) {
        if db.proc_macro_client_status().is_uninitialized() {
            self.try_initialize(db);
        }
    }

    pub fn handle_error(&mut self, db: &mut AnalysisDatabase) {
        if !self.try_initialize(db) {
            self.fatal_failed(db);
        }
    }

    /// Check if there was error reported, if so try to restart.
    /// If client is ready applies all available responses.
    pub fn on_response(&mut self, db: &mut AnalysisDatabase) {
        match db.proc_macro_client_status() {
            ClientStatus::Starting(client) => {
                let Ok(_defined_macros) = client.finish_initialize() else {
                    self.handle_error(db);

                    return;
                };

                // TODO setup db plugins.

                db.set_proc_macro_client_status(ClientStatus::Ready(client));
            }
            ClientStatus::Ready(_client) => {
                // TODO handle responses
            }
            _ => {}
        }
    }

    /// Tries starting proc-macro-server initialization process.
    ///
    /// Returns value indicating if initialization was attempted.
    fn try_initialize(&mut self, db: &mut AnalysisDatabase) -> bool {
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

                db.set_proc_macro_client_status(ClientStatus::Starting(Arc::new(client)));
            }
            Err(err) => {
                error!("spawning proc-macro-server failed: {err:?}");

                self.fatal_failed(db)
            }
        }
    }

    fn fatal_failed(&self, db: &mut AnalysisDatabase) {
        db.set_proc_macro_client_status(ClientStatus::Crashed);

        // TODO Send notification.
    }
}

#[derive(Clone)]
pub struct ProcMacroChannelsReceivers {
    // Single element queue used for notifying when responses queue is pushed.
    pub response: Receiver<()>,

    // Single element queue used for notifying when client occurred an error.
    pub error: Receiver<()>,
}

#[derive(Clone)]
struct ProcMacroChannelsSenders {
    // Single element queue used for notifying when responses queue is pushed.
    response: Sender<()>,

    // Single element queue used for notifying when client occurred an error.
    error: Sender<()>,
}
