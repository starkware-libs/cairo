use std::collections::VecDeque;
use std::path::PathBuf;

use anyhow::Context;
use tower_lsp::lsp_types::{ClientCapabilities, ConfigurationItem};
use tower_lsp::Client;
use tracing::{debug, error, warn};

use crate::lsp::client_capabilities::ClientCapabilitiesExt;

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
#[derive(Debug, Default)]
pub struct Config {
    /// A user-provided path to the `core` crate source code for use in projects where `core` is
    /// unmanaged by the toolchain.
    ///
    /// The path may omit the `corelib/src` or `src` suffix.
    ///
    /// The property is set by the user under the `cairo1.corelibPath` key in client configuration.
    pub unmanaged_core_path: Option<PathBuf>,
}

impl Config {
    /// Reloads the configuration from the language client.
    #[tracing::instrument(name = "reload_config", level = "trace", skip_all)]
    pub async fn reload(&mut self, client: &Client, client_capabilities: &ClientCapabilities) {
        if !client_capabilities.workspace_configuration_support() {
            warn!(
                "client does not support `workspace/configuration` requests, config will not be \
                 reloaded"
            );
            return;
        }

        let items = vec![ConfigurationItem {
            scope_uri: None,
            section: Some("cairo1.corelibPath".to_owned()),
        }];
        let expected_len = items.len();
        if let Ok(response) = client
            .configuration(items)
            .await
            .context("failed to query language client for configuration items")
            .inspect_err(|e| warn!("{e:?}"))
        {
            let response_len = response.len();
            if response_len != expected_len {
                error!(
                    "server returned unexpected number of configuration items, expected: \
                     {expected_len}, got: {response_len}"
                );
                return;
            }

            // This conversion is O(1), and makes popping from front also O(1).
            let mut response = VecDeque::from(response);

            self.unmanaged_core_path =
                response.pop_front().as_ref().and_then(|v| v.as_str()).map(Into::into);

            debug!("reloaded configuration: {self:#?}");
        }
    }
}
