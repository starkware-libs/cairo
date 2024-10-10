use lsp_types::notification::PublishDiagnostics;
use lsp_types::{PublishDiagnosticsParams, Url};

use crate::server::client::Notifier;

pub trait NotifierExt {
    fn clear_diagnostics(&self, file: Url);
    fn publish_diagnostics(&self, file: Url, lsp_diagnostics: Vec<lsp_types::Diagnostic>);
}

impl NotifierExt for Notifier {
    fn clear_diagnostics(&self, file: Url) {
        self.publish_diagnostics(file, vec![])
    }

    #[tracing::instrument(skip_all)]
    fn publish_diagnostics(&self, file: Url, lsp_diagnostics: Vec<lsp_types::Diagnostic>) {
        self.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
            uri: file,
            diagnostics: lsp_diagnostics,
            version: None,
        })
    }
}
