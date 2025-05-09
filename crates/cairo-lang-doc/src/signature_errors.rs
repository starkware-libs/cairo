use std::fmt;

#[derive(Debug)]
pub enum SignatureError {
    FailedRetrievingSemanticData(String),
    FailedWritingSignature(String),
    FailedWritingType(String),
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
            SignatureError::FailedWritingType(full_path) => {
                write!(f, "Failed writing a type for {full_path:?}.")
            }
        }
    }
}

impl std::error::Error for SignatureError {}
