use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::{Id, Request, Response, Result};

/// An incoming or outgoing JSON-RPC message.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Message {
    /// A response message.
    Response(Response),
    /// A request or notification message.
    Request(Request),
}

impl Message {
    /// Creates a JSON-RPC request message from untyped parts.
    pub fn request(method: &'static str, id: Id, params: Value) -> Message {
        let mut b = Request::build(method).id(id);
        if !params.is_null() {
            b = b.params(params);
        }
        Message::Request(b.finish())
    }

    /// Creates a JSON-RPC notification message from untyped parts.
    pub fn notification(method: &'static str, params: Value) -> Message {
        Message::Request(Request::build(method).params(params).finish())
    }

    /// Creates a JSON-RPC response message from untyped parts.
    pub fn response(id: Id, result: Result<Value>) -> Message {
        Message::Response(Response::from_parts(id, result))
    }
}

/// A utility object for generating unique IDs for JSON-RPC requests.
#[derive(Default)]
pub struct RequestIdGenerator {
    next_id: i64,
}

impl RequestIdGenerator {
    /// Generates a new unique request ID.
    pub fn next(&mut self) -> Id {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        Id::Number(id)
    }
}
