use std::path::{Path, PathBuf};
use std::{env, fs};

use anyhow::{Context, Result};

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
