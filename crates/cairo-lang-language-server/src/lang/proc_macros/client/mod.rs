use std::sync::Mutex;

use anyhow::{Context, Result, anyhow, ensure};
use connection::ProcMacroServerConnection;
use crossbeam::channel::Sender;
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
use tracing::error;

pub mod connection;
mod id_generator;
pub mod status;

#[derive(Debug)]
pub enum RequestParams {
    Attribute(ExpandAttributeParams),
    Derive(ExpandDeriveParams),
    Inline(ExpandInlineMacroParams),
}

#[derive(Debug)]
pub struct ProcMacroClient {
    connection: ProcMacroServerConnection,
    id_generator: id_generator::IdGenerator,
    requests_params: Mutex<FxHashMap<RequestId, RequestParams>>,
    error_channel: Sender<()>,
}

impl ProcMacroClient {
    pub fn new(connection: ProcMacroServerConnection, error_channel: Sender<()>) -> Self {
        Self {
            connection,
            id_generator: Default::default(),
            requests_params: Default::default(),
            error_channel,
        }
    }

    pub fn request_attribute(&self, params: ExpandAttributeParams) {
        self.send_request::<ExpandAttribute>(params, RequestParams::Attribute)
    }

    pub fn request_derives(&self, params: ExpandDeriveParams) {
        self.send_request::<ExpandDerive>(params, RequestParams::Derive)
    }

    pub fn request_inline_macros(&self, params: ExpandInlineMacroParams) {
        self.send_request::<ExpandInline>(params, RequestParams::Inline)
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

    /// Calls `job` for all available responses without waiting for new ones.
    pub fn for_available_responses(
        &self,
        mut job: impl FnMut(RpcResponse, &mut FxHashMap<RequestId, RequestParams>),
    ) {
        let mut responses = self.connection.responses.lock().unwrap();
        let mut requests = self.requests_params.lock().unwrap();

        while let Some(response) = responses.pop_front() {
            job(response, &mut requests);
        }
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
        let response = self.connection.responses.lock().unwrap().pop_front().unwrap();

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

    fn send_request<M: Method>(
        &self,
        params: M::Params,
        map: impl FnOnce(M::Params) -> RequestParams,
    ) {
        let id = self.id_generator.unique_id();
        // This must be locked before sending request so sending request and tracking is atomic
        // operation.
        let mut requests_params = self.requests_params.lock().unwrap();

        match self.send_request_untracked::<M>(id, &params) {
            Ok(()) => {
                requests_params.insert(id, map(params));
            }
            Err(err) => {
                error!("Sending request to proc-macro-server failed: {err:?}");

                self.failed();
            }
        }
    }

    fn failed(&self) {
        let _ = self.error_channel.try_send(());
    }
}