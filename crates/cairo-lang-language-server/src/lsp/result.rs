use std::fmt;

use lsp_server::ErrorCode;

pub struct LSPError {
    pub code: ErrorCode,
    pub error: anyhow::Error,
}

pub type LSPResult<T> = Result<T, LSPError>;

/// A trait to convert result types into the lsp_server result type, [`LSPResult`].
pub trait LSPResultEx<T> {
    fn with_failure_code(self, code: ErrorCode) -> Result<T, LSPError>;
}

impl<T, E: Into<anyhow::Error>> LSPResultEx<T> for Result<T, E> {
    fn with_failure_code(self, code: ErrorCode) -> Result<T, LSPError> {
        self.map_err(|error| LSPError::new(error.into(), code))
    }
}

impl LSPError {
    pub fn new(error: anyhow::Error, code: ErrorCode) -> Self {
        Self { code, error }
    }
}

// Right now, we treat the error code as invisible data that won't
// be printed.
impl fmt::Debug for LSPError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.fmt(f)
    }
}

impl fmt::Display for LSPError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.fmt(f)
    }
}
