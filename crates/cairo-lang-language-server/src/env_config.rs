//! Handling of LS configuration via environment variables.
//!
//! The [`report_to_logs`] function logs each variable value as a debug message.
//!
//! **Note**: Each variable should be described by a name constant, accessor function and entry
//! in the [`report_to_logs`] function.

use std::env;
use std::ffi::OsString;
use std::path::PathBuf;
use std::time::Duration;

use tracing::debug;

pub const CAIRO_LS_DB_REPLACE_INTERVAL: &'_ str = "CAIRO_LS_DB_REPLACE_INTERVAL";
pub const CAIRO_LS_LOG: &'_ str = "CAIRO_LS_LOG";
pub const CAIRO_LS_PROFILE: &'_ str = "CAIRO_LS_PROFILE";
pub const SCARB: &'_ str = "SCARB";

/// Interval between compiler database regenerations (to free unused memory).
pub fn db_replace_interval() -> Duration {
    const DEFAULT: u64 = 300;

    env::var(CAIRO_LS_DB_REPLACE_INTERVAL)
        .ok()
        .and_then(|v| v.parse().ok())
        .map(Duration::from_secs)
        .unwrap_or_else(|| Duration::from_secs(DEFAULT))
}

/// LS tracing filter, see [`tracing_subscriber::EnvFilter`] for more.
pub fn log_env_filter() -> String {
    env::var(CAIRO_LS_LOG).unwrap_or_default()
}

/// Whether to generate LS tracing profile in the opened project directory.
pub fn tracing_profile() -> bool {
    env::var_os(CAIRO_LS_PROFILE).map(env_to_bool).unwrap_or_default()
}

/// Path to the Scarb binary to call during analysis.
pub fn scarb_path() -> Option<PathBuf> {
    env::var_os(SCARB).map(PathBuf::from)
}

/// Print all environment variables values (or defaults) as debug messages in logs.
pub fn report_to_logs() {
    debug!("{CAIRO_LS_DB_REPLACE_INTERVAL}={:?}", db_replace_interval());
    debug!("{CAIRO_LS_LOG}={}", log_env_filter());
    debug!("{CAIRO_LS_PROFILE}={}", tracing_profile());
    debug!("{SCARB}={}", scarb_path().map(|p| p.display().to_string()).unwrap_or_default());
}

fn env_to_bool(os: OsString) -> bool {
    matches!(os.to_str(), Some("1") | Some("true"))
}
