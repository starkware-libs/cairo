pub mod client_capabilities;
pub mod cursor;
pub mod fixture;
pub mod jsonrpc;
mod mock_client;
pub mod normalize;
mod runtime;

pub use self::cursor::cursors;
pub use self::mock_client::MockClient;

/// Create a sandboxed environment for testing language server features.
///
/// This macro creates a [`fixture::Fixture`] first and sets it up according to the provided
/// properties, and then runs a [`MockClient`] on it.
///
/// See actual tests for usage examples.
macro_rules! sandbox {
    (
        $(files { $($file:expr => $content:expr),* $(,)? })?
        $(client_capabilities = $client_capabilities:expr;)?
        $(workspace_configuration = $workspace_configuration:expr;)?
    ) => {{
        use $crate::support::{
            client_capabilities,
            fixture::Fixture,
            MockClient
        };

        let mut fixture = Fixture::new();

        $($(fixture.add_file($file, $content);)*)?

        #[allow(unused_mut)]
        let mut client_capabilities = client_capabilities::base();

        #[allow(unused_assignments, unused_mut)]
        let mut workspace_configuration = serde_json::Value::default();
        $(
            workspace_configuration = $workspace_configuration;
            client_capabilities =
                client_capabilities::with_workspace_configuration(client_capabilities, true);
        )?

        $(
            client_capabilities = $client_capabilities(client_capabilities);
        )?

        MockClient::start(fixture, client_capabilities, workspace_configuration)
    }};
}

pub(crate) use sandbox;
