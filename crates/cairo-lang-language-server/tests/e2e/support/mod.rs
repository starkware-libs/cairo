pub mod client_capabilities;
pub mod fixture;
mod jsonrpc;
mod mock_client;
mod runtime;

pub use self::mock_client::MockClient;

/// Create a sandboxed environment for testing language server features.
///
/// This macro creates a [`fixture::Fixture`] first and sets it up according to the provided
/// properties, and then runs a [`mock_client::MockClient`] on it.
///
/// See actual tests for usage examples.
macro_rules! sandbox {
    (
        $(files { $($file:expr => $content:expr),* $(,)? })?
        $(client_capabilities = $client_capabilities:expr;)?
    ) => {{
        use $crate::support::{
            client_capabilities,
            fixture::Fixture,
            MockClient
        };

        let mut fixture = Fixture::new();

        $($(fixture.add_file($file, $content);)*)?

        let client_capabilities = client_capabilities::base();
        $(
            let client_capabilities = $client_capabilities(client_capabilities);
        )?

        MockClient::start(fixture, client_capabilities)
    }};
}

pub(crate) use sandbox;
