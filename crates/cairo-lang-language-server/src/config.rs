use std::collections::VecDeque;
use std::path::PathBuf;

use anyhow::Context;
use tower_lsp::lsp_types::ConfigurationItem;
use tower_lsp::Client;
use tracing::{debug, error, warn};

// TODO(mkaput): Write a macro that will auto-generate this struct and the `reload` logic.
// TODO(mkaput): Write a test that checks that fields in this struct are sorted alphabetically.
// TODO(mkaput): Write a tool that syncs `configuration` in VSCode extension's `package.json`.
// TODO(mkaput): Consider moving `SCARB` env var here. Env var must override client config,
//  because env will be set in `scarb cairo-language-server` context.
/// Runtime configuration for the language server.
///
/// The properties stored in this struct **may** change during LS lifetime (through the
/// [`Self::reload`] method).
/// Therefore, it is **forbidden** to hold any references or copies of this struct or its values for
/// longer periods of time.
#[derive(Debug, Default)]
pub struct Config {
    /// A user-provided fallback path to the `core` crate source code for use in projects where
    /// `core` is unmanaged by the toolchain.
    ///
    /// This property is only used when the LS is unable to find unmanaged `core` automatically.
    /// The path may omit the `corelib/src` or `src` suffix.
    ///
    /// The property is set by the user under the `cairo1.corelibPath` key in client configuration.
    pub unmanaged_core_fallback_path: Option<PathBuf>,
}

impl Config {
    /// Reloads the configuration from the language client.
    #[tracing::instrument(name = "reload_config", level = "trace", skip_all)]
    pub async fn reload(&mut self, client: &Client) {
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

            self.unmanaged_core_fallback_path =
                response.pop_front().as_ref().and_then(|v| v.as_str()).map(Into::into);

            debug!("reloaded configuration: {self:#?}");
        }
    }
}
