use anyhow::{Context, Result, anyhow, ensure};
use connection::ProcMacroServerConnection;
use crossbeam::channel::Sender;
use scarb_proc_macro_server_types::jsonrpc::{RequestId, RpcRequest};
use scarb_proc_macro_server_types::methods::Method;
use scarb_proc_macro_server_types::methods::defined_macros::{
    DefinedMacros, DefinedMacrosParams, DefinedMacrosResponse,
};
pub use status::ClientStatus;
use tracing::error;

pub mod connection;
mod id_generator;
pub mod status;

#[derive(Debug)]
pub struct ProcMacroClient {
    connection: ProcMacroServerConnection,
    id_generator: id_generator::IdGenerator,
    error_channel: Sender<()>,
}

impl ProcMacroClient {
    pub fn new(connection: ProcMacroServerConnection, error_channel: Sender<()>) -> Self {
        Self { connection, id_generator: Default::default(), error_channel }
    }

    pub fn start_initialize(&self) {
        if let Err(err) = self.request_defined_macros() {
            error!("failed to request defined macros: {err:?}");

            self.failed();
        }
    }

    pub fn finish_initialize(&self) -> Result<DefinedMacrosResponse> {
        self.handle_defined_macros()
            .inspect_err(|err| error!("failed to fetch defined macros: {err:?}"))
    }

    fn request_defined_macros(&self) -> Result<()> {
        let id = self.id_generator.unique_id();

        self.send_request_untracked::<DefinedMacros>(id, &DefinedMacrosParams {})?;

        ensure!(
            id == 0,
            "fetching defined macros should be the first sent request, expected id=0 instead it \
             is: {id}"
        );

        Ok(())
    }

    fn handle_defined_macros(&self) -> Result<DefinedMacrosResponse> {
        let response = self
            .connection
            .responses
            .lock()
            .expect("responses lock should not be poisoned")
            .pop_front()
            .expect("responses should not be empty after receiving response");

        ensure!(
            response.id == 0,
            "fetching defined macros should be done before any other request is sent, received \
             response id: {}, expected 0",
            response.id
        );

        let success = response
            .into_result()
            .map_err(|error| anyhow!("proc-macro-server responded with error: {error:?}"))?;

        serde_json::from_value(success)
            .context("failed to deserialize response for defined macros request")
    }

    fn send_request_untracked<M: Method>(&self, id: RequestId, params: &M::Params) -> Result<()> {
        self.connection
            .requester
            .send(RpcRequest {
                id,
                method: M::METHOD.to_string(),
                value: serde_json::to_value(params).unwrap(),
            })
            .with_context(|| anyhow!("sending request {id} failed"))
    }

    fn failed(&self) {
        let _ = self.error_channel.try_send(());
    }
}
