use std::fmt;

#[derive(Debug)]
pub enum SignatureError {
    FailedRetrievingSemanticData(String),
    FailedWritingSignature(String),
    FailedWritingSignatureFormatter(String),
}

impl fmt::Display for SignatureError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SignatureError::FailedRetrievingSemanticData(full_path) => {
                write!(f, "Failed retrieving semantic data for {full_path:?}.")
            }
            SignatureError::FailedWritingSignature(full_path) => {
                write!(f, "Failed writing signature for {full_path:?}.")
            }
            SignatureError::FailedWritingSignatureFormatter(e) => {
                write!(f, "Failed writing signature formatter. {e:?}")
            }
        }
    }
}

impl From<fmt::Error> for SignatureError {
    fn from(e: fmt::Error) -> Self {
        SignatureError::FailedWritingSignatureFormatter(e.to_string())
    }
}

impl From<SignatureError> for fmt::Error {
    fn from(_: SignatureError) -> Self {
        fmt::Error
    }
}
