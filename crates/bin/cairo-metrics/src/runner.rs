use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::{env, fs};

use anyhow::{Context, Result};

static CAIRO_COMPILE_PATH: OnceLock<PathBuf> = OnceLock::new();

/// Returns the path to cairo-compile binary.
/// First checks for sibling binary (same dir as cairo-metrics), then falls back to PATH.
pub fn cairo_compile_path() -> &'static Path {
    CAIRO_COMPILE_PATH.get_or_init(|| {
        // Try sibling binary first (for local development).
        if let Ok(exe) = env::current_exe()
            && let Some(dir) = exe.parent()
        {
            let sibling = dir.join("cairo-compile");
            if sibling.exists() {
                tracing::debug!("Using sibling cairo-compile: {}", sibling.display());
                return sibling;
            }
        }
        // Fall back to PATH lookup.
        tracing::debug!("Using cairo-compile from PATH");
        PathBuf::from("cairo-compile")
    })
}

/// Temporary directory with automatic cleanup.
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
