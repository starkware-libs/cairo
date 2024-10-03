use std::collections::VecDeque;
use std::path::PathBuf;

use anyhow::Context;
use lsp_server::ErrorCode;
use lsp_types::request::WorkspaceConfiguration;
use lsp_types::{ClientCapabilities, ConfigurationItem, ConfigurationParams};
use serde_json::Value;
use tracing::{debug, error, warn};

use crate::lsp::capabilities::client::ClientCapabilitiesExt;
use crate::lsp::result::{LSPResult, LSPResultEx};
use crate::server::client::Requester;
use crate::server::schedule::Task;

// TODO(mkaput): Write a macro that will auto-generate this struct and the `reload` logic.
// TODO(mkaput): Write a test that checks that fields in this struct are sorted alphabetically.
// TODO(mkaput): Write a tool that syncs `configuration` in VSCode extension's `package.json`.
/// Runtime configuration for the language server.
///
/// The properties stored in this struct **may** change during LS lifetime (through the
/// [`Self::reload`] method).
/// Therefore, holding any references or copies of this struct or its values for
/// longer periods of time should be avoided, unless the copy will be reactively updated on
/// `workspace/didChangeConfiguration` requests.
#[derive(Debug, Default, Clone)]
pub struct Config {
    /// A user-provided path to the `core` crate source code for use in projects where `core` is
    /// unmanaged by the toolchain.
    ///
    /// The path may omit the `corelib/src` or `src` suffix.
    ///
    /// The property is set by the user under the `cairo1.corelibPath` key in client configuration.
    pub unmanaged_core_path: Option<PathBuf>,
    /// Whether to include the trace of the generation location of diagnostic location mapped by
    /// macros.
    ///
    /// The property is set by the user under the `cairo1.traceMacroDiagnostics` key in client
    /// configuration.
    pub trace_macro_diagnostics: bool,
}

impl Config {
    /// Reloads the configuration from the language client.
    #[tracing::instrument(name = "reload_config", level = "trace", skip_all)]
    pub fn reload(
        &mut self,
        requester: &mut Requester<'_>,
        client_capabilities: &ClientCapabilities,
    ) -> LSPResult<()> {
        if !client_capabilities.workspace_configuration_support() {
            warn!(
                "client does not support `workspace/configuration` requests, config will not be \
                 reloaded"
            );
            return Ok(());
        }

        let items = vec![
            ConfigurationItem { scope_uri: None, section: Some("cairo1.corelibPath".to_owned()) },
            ConfigurationItem {
                scope_uri: None,
                section: Some("cairo1.traceMacroDiagnostics".to_owned()),
            },
        ];
        let expected_len = items.len();

        let handler = move |response: Vec<Value>| {
            let response_len = response.len();
            if response_len != expected_len {
                error!(
                    "server returned unexpected number of configuration items, expected: \
                     {expected_len}, got: {response_len}"
                );
                return Task::nothing();
            }

            // This conversion is O(1), and makes popping from front also O(1).
            let mut response = VecDeque::from(response);

            Task::local(move |state, _, _, _| {
                state.config.unmanaged_core_path = response
                    .pop_front()
                    .as_ref()
                    .and_then(Value::as_str)
                    .filter(|s| !s.is_empty())
                    .map(Into::into);
                state.config.trace_macro_diagnostics =
                    response.pop_front().as_ref().and_then(Value::as_bool).unwrap_or_default();

                debug!("reloaded configuration: {:#?}", state.config);
            })
        };

        requester
            .request::<WorkspaceConfiguration>(ConfigurationParams { items }, handler)
            .context("failed to query language client for configuration items")
            .with_failure_code(ErrorCode::RequestFailed)
            .inspect_err(|e| warn!("{e:?}"))
    }
}
