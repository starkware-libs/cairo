use std::sync::Mutex;

use proc_macro_server_api::methods::defined_macros::{
    DefinedMacros, DefinedMacrosParams, DefinedMacrosResponse,
};
use proc_macro_server_api::methods::expand::{
    ExpandAttribute, ExpandAttributeParams, ExpandDerive, ExpandDeriveParams, ExpandInline,
    ExpandInlineMacroParams,
};
use proc_macro_server_api::{Method, RequestId, RpcRequest, RpcResponse};
use rustc_hash::FxHashMap;

use super::connection::ProcMacroServerConnection;
use super::id_generator::IdGenerator;

#[derive(Debug)]
pub enum RequestParams {
    Attribute(ExpandAttributeParams),
    Derive(ExpandDeriveParams),
    Inline(ExpandInlineMacroParams),
}

#[derive(Debug)]
pub struct ProcMacroClient {
    id_generator: IdGenerator,
    connection: ProcMacroServerConnection,
    pub(super) requests_params: Mutex<FxHashMap<RequestId, RequestParams>>,
}

impl ProcMacroClient {
    pub fn new(process: std::process::Child) -> Self {
        Self {
            id_generator: Default::default(),
            connection: ProcMacroServerConnection::new(process),
            requests_params: Default::default(),
        }
    }

    /// Reads all available responses without waiting for new ones.
    pub fn available_responses(&self) -> impl Iterator<Item = RpcResponse> + '_ {
        self.connection.responder.try_iter()
    }
}

/// Used by [`ProcMacroCacheGroup`]
impl ProcMacroClient {
    pub fn request_attribute(&self, params: ExpandAttributeParams) {
        self.send_request_tracked::<ExpandAttribute>(params, RequestParams::Attribute);
    }

    pub fn request_derives(&self, params: ExpandDeriveParams) {
        self.send_request_tracked::<ExpandDerive>(params, RequestParams::Derive);
    }

    pub fn request_inline_macros(&self, params: ExpandInlineMacroParams) {
        self.send_request_tracked::<ExpandInline>(params, RequestParams::Inline);
    }

    pub fn defined_macros(&self) -> Option<DefinedMacrosResponse> {
        let id = self.send_request::<DefinedMacros>(&DefinedMacrosParams {});

        // This should be first sended request.
        assert_eq!(id, 0);

        // This works because this it is first request we sends and we wait for response before
        // sending any more requests.
        let response = self.connection.responder.recv().ok()?;

        if response.id != id {
            // Something went wrong.
            return None;
        }

        serde_json::from_value(response.value).ok()
    }

    fn send_request<M: Method>(&self, params: &M::Params) -> RequestId {
        let id = self.id_generator.unique_id();

        let send_failed = self
            .connection
            .requester
            .send(RpcRequest {
                id,
                method: M::METHOD.to_string(),
                value: serde_json::to_value(params).unwrap(),
            })
            .is_err();

        // proc macros server process stopped accepting requests -> exited
        if send_failed {
            // TODO
            // request proc-macro-server restart
        }

        id
    }

    fn send_request_tracked<M: Method>(
        &self,
        params: M::Params,
        map: impl FnOnce(M::Params) -> RequestParams,
    ) {
        let id = self.send_request::<M>(&params);

        self.requests_params.lock().unwrap().insert(id, map(params));
    }
}
