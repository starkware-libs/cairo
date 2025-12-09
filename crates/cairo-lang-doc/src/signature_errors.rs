use std::fmt;

use cairo_lang_diagnostics::DiagnosticAdded;

#[derive(Debug)]
pub enum SignatureError {
    FailedRetrievingSemanticData(String),
    FailedWritingSignature(String),
    FailedWritingSignatureDiagnostic(DiagnosticAdded),
    FailedWritingSignatureFormatter(fmt::Error),
}

impl fmt::Display for SignatureError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SignatureError::FailedRetrievingSemanticData(full_path) => {
                write!(f, "{full_path:?} Failed retrieving semantic data.")
            }
            SignatureError::FailedWritingSignature(full_path) => {
                write!(f, "{full_path:?} Failed writing signature.")
            }
            SignatureError::FailedWritingSignatureDiagnostic(diagnostics) => {
                write!(f, "Failed writing signature. {diagnostics:?}")
            }
            SignatureError::FailedWritingSignatureFormatter(e) => {
                write!(f, "Failed writing signature formatter. {e:?}")
            }
        }
    }
}

impl From<fmt::Error> for SignatureError {
    fn from(e: fmt::Error) -> Self {
        SignatureError::FailedWritingSignatureFormatter(e)
    }
}

impl From<SignatureError> for fmt::Error {
    fn from(_: SignatureError) -> Self {
        fmt::Error
    }
}

impl From<DiagnosticAdded> for SignatureError {
    fn from(e: DiagnosticAdded) -> Self {
        SignatureError::FailedWritingSignatureDiagnostic(e)
    }
}
