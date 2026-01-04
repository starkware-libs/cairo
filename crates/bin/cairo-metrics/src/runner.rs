use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use std::{env, fs};

use anyhow::{Context, Result};

use crate::config::CommandSpec;

/// Runs a command and captures stderr, exit code, and wall time.
/// Shows command output only at DEBUG level or below.
pub fn run_command(spec: &CommandSpec) -> Result<RunOutput> {
    let show_output = tracing::enabled!(tracing::Level::DEBUG);

    let start = Instant::now();
    let output = Command::new(&spec.binary)
        .args(&spec.args)
        .stdout(if show_output { Stdio::inherit() } else { Stdio::null() })
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("failed to run: {}", spec.display()))?;

    Ok(RunOutput {
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        exit_code: output.status.code().unwrap_or(-1),
        wall_time: start.elapsed(),
    })
}

/// Output of a single command run.
#[derive(Debug, Clone)]
pub struct RunOutput {
    pub stderr: String,
    pub exit_code: i32,
    pub wall_time: Duration,
}

/// Temporary directory with RAII cleanup.
pub struct TempDir(PathBuf);

impl TempDir {
    /// Create a new temp directory for the given benchmark.
    pub fn new(name: &str) -> Result<Self> {
        let path = env::temp_dir().join(format!("cairo-metrics-{name}"));
        if path.exists() {
            fs::remove_dir_all(&path).with_context(|| {
                format!("failed to clean existing temp dir: {}", path.display())
            })?;
        }
        fs::create_dir_all(&path)
            .with_context(|| format!("failed to create temp dir: {}", path.display()))?;
        Ok(Self(path))
    }

    /// Returns the path to the temp directory.
    pub fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

/// Deep copy a directory.
pub fn copy_dir(src: &Path, dst: &Path) -> Result<()> {
    fs::create_dir_all(dst).with_context(|| format!("failed to create dir: {}", dst.display()))?;

    for entry in
        fs::read_dir(src).with_context(|| format!("failed to read dir: {}", src.display()))?
    {
        let entry = entry?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());

        if src_path.is_dir() {
            // Optimization: Skip target directory.
            if entry.file_name() == "target" {
                continue;
            }
            copy_dir(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path)
                .with_context(|| format!("failed to copy: {}", src_path.display()))?;
        }
    }
    Ok(())
}
