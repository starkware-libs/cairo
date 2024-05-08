pub mod client_capabilities;
pub mod fixture;
mod jsonrpc;
pub mod mock_client;
mod runtime;

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
            init_tracing,
            fixture::Fixture,
            mock_client::MockClient
        };

        init_tracing();
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

pub fn init_tracing() {
    use std::io;
    use std::io::IsTerminal;

    use tracing_subscriber::filter::EnvFilter;
    use tracing_subscriber::fmt::format::FmtSpan;
    use tracing_subscriber::fmt::time::Uptime;

    let _ = tracing_subscriber::fmt()
        .with_writer(io::stderr)
        .with_timer(Uptime::default())
        .with_ansi(io::stderr().is_terminal())
        .with_span_events(FmtSpan::CLOSE)
        .with_env_filter(
            EnvFilter::from_default_env()
                .add_directive("cairo_lang_language_server=trace".parse().unwrap()),
        )
        .try_init();
}
