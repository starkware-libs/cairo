use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, anyhow, ensure};
use connection::ProcMacroServerConnection;
use rustc_hash::FxHashMap;
use scarb_proc_macro_server_types::jsonrpc::{RequestId, RpcRequest, RpcResponse};
use scarb_proc_macro_server_types::methods::Method;
use scarb_proc_macro_server_types::methods::defined_macros::{
    DefinedMacros, DefinedMacrosParams, DefinedMacrosResponse,
};
use scarb_proc_macro_server_types::methods::expand::{
    ExpandAttribute, ExpandAttributeParams, ExpandDerive, ExpandDeriveParams, ExpandInline,
    ExpandInlineMacroParams,
};
pub use status::ClientStatus;
use status::{ClientStatusChange, ProcMacroClientStatusChange};
use tracing::error;

mod connection;
pub mod controller;
mod id_generator;
mod status;

#[derive(Debug)]
pub enum RequestParams {
    Attribute(ExpandAttributeParams),
    Derive(ExpandDeriveParams),
    Inline(ExpandInlineMacroParams),
}

#[derive(Debug)]
pub struct ProcMacroClient {
    connection: ProcMacroServerConnection,
    status_change: ProcMacroClientStatusChange,
    id_generator: id_generator::IdGenerator,
    pub(super) requests_params: Mutex<FxHashMap<RequestId, RequestParams>>,
}

impl ProcMacroClient {
    fn new(
        connection: ProcMacroServerConnection,
        status_change: ProcMacroClientStatusChange,
    ) -> Self {
        Self {
            connection,
            status_change,
            id_generator: Default::default(),
            requests_params: Default::default(),
        }
    }

    pub fn request_attribute(&self, params: ExpandAttributeParams) {
        self.send_request_tracked::<ExpandAttribute>(params, RequestParams::Attribute)
    }

    pub fn request_derives(&self, params: ExpandDeriveParams) {
        self.send_request_tracked::<ExpandDerive>(params, RequestParams::Derive)
    }

    pub fn request_inline_macros(&self, params: ExpandInlineMacroParams) {
        self.send_request_tracked::<ExpandInline>(params, RequestParams::Inline)
    }

    /// Initlializes client by fetching defined macros.
    /// Note: This is blocking!
    fn initialize(self) {
        match self.fetch_defined_macros() {
            Ok(defined_macros) => {
                self.status_change
                    .clone()
                    .update(ClientStatusChange::Ready(defined_macros, Arc::new(self)));
            }
            Err(err) => {
                error!("failed to fetch defined macros: {err:?}");

                self.status_change.update(ClientStatusChange::RecoverablyFailed);
            }
        }
    }

    /// Reads all available responses without waiting for new ones.
    fn available_responses(&self) -> impl Iterator<Item = RpcResponse> + '_ {
        self.connection.responder.try_iter()
    }

    fn fetch_defined_macros(&self) -> Result<DefinedMacrosResponse> {
        let id = self.send_request_untracked::<DefinedMacros>(&DefinedMacrosParams {})?;

        ensure!(
            id == 0,
            "fetching defined macros should have been the first sent request, it was {id}. (zero \
             counting)"
        );

        // This works because this it is first request we sends and we wait for response before
        // sending any more requests.
        let response = self
            .connection
            .responder
            .recv()
            .context("failed to read response for defined macros request")?;

        ensure!(
            response.id == id,
            "fetching defined macros should have been the first sent request, received response \
             for id: {} <- should be {id}",
            response.id
        );

        let success = response
            .into_result()
            .map_err(|error| anyhow!("proc-macro-server responded with error: {error:?}"))?;

        serde_json::from_value(success)
            .context("failed to deserialize response for defined macros request")
    }

    fn send_request_untracked<M: Method>(&self, params: &M::Params) -> Result<RequestId> {
        let id = self.id_generator.unique_id();

        self.connection
            .requester
            .send(RpcRequest {
                id,
                method: M::METHOD.to_string(),
                value: serde_json::to_value(params).unwrap(),
            })
            .with_context(|| anyhow!("sending request {id} failed"))
            .map(|_| id)
    }

    fn send_request_tracked<M: Method>(
        &self,
        params: M::Params,
        map: impl FnOnce(M::Params) -> RequestParams,
    ) {
        match self.send_request_untracked::<M>(&params) {
            Ok(id) => {
                self.requests_params.lock().unwrap().insert(id, map(params));
            }
            Err(err) => {
                error!("Sending request to proc-macro-server failed: {err:?}");

                self.status_change.update(ClientStatusChange::RecoverablyFailed);
            }
        }
    }
}
