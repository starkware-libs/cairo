use super::cache_group::ProcMacroCacheGroup;
use crate::{lang::db::AnalysisDatabase, toolchain::scarb::ScarbToolchain};
use anyhow::Result;
use cairo_lang_macro::TokenStream;
use crossbeam::channel::{Receiver, Sender};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Write};

pub type Id = u64;

#[derive(Debug, Deserialize)]
pub struct RpcResponse {
    pub id: Id,
    pub value: serde_json::Value,
}

#[derive(Debug, Serialize)]
pub struct RpcRequest {
    pub id: Id,
    pub method: String,
    pub value: serde_json::Value,
}

#[derive(Debug, Clone)]
pub struct ProcMacroClient {
    responder: Receiver<RpcResponse>,
    requester: Sender<RpcRequest>,

    attribute_responses: Vec<Response<(String, TokenStream, TokenStream)>>,
    derive_responses: Vec<Response<(Vec<String>, TokenStream)>>,
    inline_macros_responses: Vec<Response<TokenStream>>,

    attribute_requests: FxHashMap<Id, (String, TokenStream, TokenStream)>,
    derive_requests: FxHashMap<Id, (Vec<String>, TokenStream)>,
    inline_macros_requests: FxHashMap<Id, TokenStream>,
}

pub fn spawn_proc_macro_server(scarb: &ScarbToolchain) -> Result<ProcMacroClient> {
    //spawns process and comunicates with it over stdio

    let process = std::process::Command::new("xd").spawn().unwrap();

    let input = process.stdout.unwrap();
    let mut output = process.stdin.unwrap();

    let (sender, responder) = crossbeam::channel::unbounded();
    let (requester, receiver) = crossbeam::channel::bounded(0);

    std::thread::spawn(move || {
        let mut line = String::new();
        let mut buffer = BufReader::new(input);

        loop {
            buffer.read_line(&mut line).unwrap();

            let response: RpcResponse = serde_json::from_str(&line).unwrap();

            sender.send(response).unwrap();
        }
    });

    std::thread::spawn(move || {
        for request in receiver {
            let request = serde_json::to_vec(&request).unwrap();

            output.write_all(&request).unwrap();
        }
    });

    Ok(ProcMacroClient {
        requester,
        responder,
        attribute_responses: Default::default(),
        derive_responses: Default::default(),
        inline_macros_responses: Default::default(),
        attribute_requests: Default::default(),
        derive_requests: Default::default(),
        inline_macros_requests: Default::default(),
    })
}

impl ProcMacroClient {
    fn fetch(&mut self) {
        // Read all available messages without waiting.
        while let Ok(response) = self.responder.try_recv() {
            let maybe_response =
                Self::try_move(response, &mut self.derive_requests, &mut self.derive_responses)
                    .and_then(|response| {
                        Self::try_move(
                            response,
                            &mut self.attribute_requests,
                            &mut self.attribute_responses,
                        )
                    })
                    .and_then(|response| {
                        Self::try_move(
                            response,
                            &mut self.inline_macros_requests,
                            &mut self.inline_macros_responses,
                        )
                    });

            if let Some(response) = maybe_response {
                // TODO received response for requests that has not been sended
            }
        }
    }

    pub fn apply_diff(&mut self, db: &mut AnalysisDatabase) {
        self.fetch();

        let responses = std::mem::take(&mut self.attribute_responses);
        let mut resolutions = db.attribute_macro_resolution();

        for response in responses {
            resolutions.insert(response.value, response.proc_macro_result);
        }

        db.set_attribute_macro_resolution(resolutions);
        //
        let responses = std::mem::take(&mut self.derive_responses);
        let mut resolutions = db.derive_macro_resolution();

        for response in responses {
            resolutions.insert(response.value, response.proc_macro_result);
        }

        db.set_derive_macro_resolution(resolutions);
        //
        let responses = std::mem::take(&mut self.inline_macros_responses);
        let mut resolutions = db.inline_macro_resolution();

        for response in responses {
            resolutions.insert(response.value, response.proc_macro_result);
        }

        db.set_inline_macro_resolution(resolutions);
    }

    fn try_move<T>(
        response: RpcResponse,
        requests: &mut FxHashMap<Id, T>,
        responses: &mut Vec<Response<T>>,
    ) -> Option<RpcResponse> {
        match requests.remove(&response.id) {
            Some(request) => {
                responses.push(Response {
                    proc_macro_result: serde_json::from_value(response.value).unwrap(),
                    value: request,
                });

                None
            }
            None => Some(response),
        }
    }
}

/// Used by [ProcMacroCacheGroup]
impl ProcMacroClient {
    pub fn request_attribute(&self, name: String, args: TokenStream, token_stream: TokenStream) {
        unimplemented!()
    }
    pub fn request_derives(&self, names: Vec<String>, token_stream: TokenStream) {
        unimplemented!()
    }
    pub fn request_inline_macros(&self, token_stream: TokenStream) {
        unimplemented!()
    }

    pub fn defined_attributes(&self) -> Vec<String> {
        unimplemented!()
    }
    pub fn defined_other_attributes(&self) -> Vec<String> {
        unimplemented!()
    }
    pub fn defined_derives(&self) -> Vec<String> {
        unimplemented!()
    }
    pub fn defined_inline_macros(&self) -> Vec<String> {
        unimplemented!()
    }
}

/// Used by main loop to update db
impl ProcMacroClient {
    pub fn attribute_responses(&self) -> Vec<Response<(String, TokenStream, TokenStream)>> {
        unimplemented!()
    }
    pub fn derive_responses(&self) -> Vec<Response<(Vec<String>, TokenStream)>> {
        unimplemented!()
    }
    pub fn inline_macros_responses(&self) -> Vec<Response<(String, TokenStream)>> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Default)]
pub struct Response<T> {
    proc_macro_result: ProcMacroResult,
    value: T,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Deserialize)]
pub struct ProcMacroResult {
    pub token_stream: TokenStream,
    pub diagnostics: Vec<cairo_lang_macro::Diagnostic>,
}
