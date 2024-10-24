use lsp_server::RequestId;

/// A utility object for generating unique IDs for JSON-RPC requests.
#[derive(Default)]
pub struct RequestIdGenerator {
    next_id: i32,
}

impl RequestIdGenerator {
    /// Generates a new unique request ID.
    pub fn next(&mut self) -> RequestId {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);

        id.into()
    }
}
