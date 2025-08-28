use std::sync::Once;

use tracing_subscriber::EnvFilter;
use tracing_subscriber::filter::dynamic_filter_fn;
use tracing_subscriber::prelude::*;

static INIT: Once = Once::new();

#[allow(dead_code)]
pub mod level {
    pub const TRACE: tracing::Level = tracing::Level::TRACE;
    pub const DEBUG: tracing::Level = tracing::Level::DEBUG;
    pub const INFO: tracing::Level = tracing::Level::INFO;
    pub const WARN: tracing::Level = tracing::Level::WARN;
    pub const ERROR: tracing::Level = tracing::Level::ERROR;
}

/// Initializes logging (tracing library).
/// The format is:
/// `<level>  /path/to/file:<line_number>  <time>  <log_message>`
pub fn init_logging(level: tracing::Level) {
    INIT.call_once(|| {
        // Bridge log records to tracing so existing log macros still work via tracing-log
        tracing_log::LogTracer::init().ok();

        let env_filter = EnvFilter::try_from_default_env()
            .or_else(|_| EnvFilter::try_new(level.to_string()))
            .unwrap();

        // Custom filter: filter out all events from the "salsa" crate unless CAIRO_UNMUTE_SALSA is
        // set.
        let salsa_filter = exclude_salsa();

        let filter = env_filter.and_then(salsa_filter);

        let timer = tracing_subscriber::fmt::time::SystemTime;
        let fmt_layer = tracing_subscriber::fmt::layer()
            .with_timer(timer)
            .with_ansi(false)
            .with_level(true)
            .with_target(false)
            .with_file(true)
            .with_line_number(true);

        // Avoid panicking if a global subscriber is already set (e.g., tests or another init).
        let registry = tracing_subscriber::registry().with(filter);
        if cfg!(test) {
            let _ = registry.with(fmt_layer.with_test_writer()).try_init();
        } else {
            let _ = registry.with(fmt_layer).try_init();
        }
    });
}

/// Returns a filter that mutes all tracing events from the "salsa" crate unless the
/// `CAIRO_UNMUTE_SALSA` environment variable is set.
/// This is useful to reduce log noise from salsa internals during normal operation.
pub fn exclude_salsa() -> tracing_subscriber::filter::DynFilterFn<
    tracing_subscriber::Registry,
    impl Fn(
        &tracing::Metadata<'_>,
        &tracing_subscriber::layer::Context<'_, tracing_subscriber::Registry>,
    ) -> bool,
> {
    let salsa_unmuted = std::env::var("CAIRO_UNMUTE_SALSA").is_ok();
    dynamic_filter_fn(move |meta, _ctx| {
        if salsa_unmuted {
            // If the env var is set, do not filter out salsa events.
            true
        } else {
            // Filter out events whose target starts with "salsa"
            !meta.target().starts_with("salsa")
        }
    })
}
