use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{cmp, fs};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

#[cfg(test)]
#[path = "config_test.rs"]
mod tests;

/// Top-level configuration.
#[derive(Debug, Clone)]
pub struct MetricsConfig {
    pub defaults: Defaults,
    pub benchmarks: Vec<Benchmark>,
}

impl MetricsConfig {
    /// Discover benchmarks from a flat directory structure.
    /// Each immediate subdirectory with a benchmark.toml is a benchmark.
    pub fn discover(benchmarks_dir: &Path) -> Result<Self> {
        let mut benchmarks = BTreeSet::new();
        for entry in fs::read_dir(benchmarks_dir)? {
            let dir = entry?.path();
            let config_path = dir.join("benchmark.toml");

            let content = fs::read_to_string(&config_path)?;
            let config: BenchmarkConfig = toml::from_str(&content)?;
            benchmarks.insert(config.into_benchmark(&dir)?);
        }

        Ok(MetricsConfig {
            defaults: Defaults::default(),
            benchmarks: benchmarks.into_iter().collect(),
        })
    }
}

/// A benchmark with its name and configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Benchmark {
    pub name: String,
    pub config: BenchmarkConfig,
    pub patches: Vec<Patch>,
}

impl Benchmark {
    /// Get the path to the Scarb project source directory.
    pub fn path(&self) -> &Path {
        &self.config.path
    }

    /// Check if this benchmark supports incremental compilation testing.
    /// Library projects are single-package and don't benefit from incremental caching.
    pub fn supports_incremental(&self) -> bool {
        !self.config.library
    }
}

impl PartialOrd for Benchmark {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Benchmark {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

/// Configuration loaded from benchmark.toml.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct BenchmarkConfig {
    /// Path to the Scarb project to benchmark (relative to repo root).
    pub path: PathBuf,
    /// Whether this benchmark is a library project (like corelib).
    /// Library projects: skip incremental benchmarks (single-package, no caching benefit)
    /// and skip casm phase (no .contract_class.json produced).
    #[serde(default)]
    pub library: bool,
}

impl BenchmarkConfig {
    /// Consume config and create a Benchmark with name derived from directory.
    pub fn into_benchmark(self, dir: &Path) -> Result<Benchmark> {
        let name = dir.file_name().unwrap().to_string_lossy().into_owned();
        let patches = discover_patches(dir)?;
        Ok(Benchmark { name, config: self, patches })
    }
}

/// Default values for benchmarks.
#[derive(Debug, Clone)]
pub struct Defaults {
    pub runs: usize,
    pub warmup: usize,
}

impl Default for Defaults {
    fn default() -> Self {
        Self { runs: 5, warmup: 1 }
    }
}

/// A single command to execute (no shell interpretation).
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CommandSpec {
    pub binary: PathBuf,
    #[serde(default)]
    pub args: Vec<String>,
}

impl CommandSpec {
    /// Format as a display string for logging/reporting.
    pub fn display(&self) -> String {
        if self.args.is_empty() {
            self.binary.display().to_string()
        } else {
            format!("{} {}", self.binary.display(), self.args.join(" "))
        }
    }
}

/// A git patch file for incremental benchmarks, for deterministic incremental benchmarks.
/// This is applied on top of a fresh copy of the example project, after a full build.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Patch {
    /// Index for ordering (parsed from filename like "0-name.patch").
    pub index: u32,
    /// Human-readable name (parsed from filename).
    pub name: String,
    /// Absolute path to the .patch file.
    pub path: PathBuf,
}

impl Patch {
    /// Parse a patch from a filename like "0-println.patch".
    pub fn from_path(path: PathBuf) -> Option<Self> {
        let filename = path.file_stem()?.to_str()?;
        let (index_str, name) = filename.split_once('-')?;
        let index = index_str.parse().ok()?;
        Some(Self { index, name: name.to_string(), path })
    }

    /// Apply this patch to a benchmark project sourcecode using `patch -p1`.
    pub fn apply(&self, dir: &Path) -> Result<()> {
        let status = Command::new("patch")
            .arg("-p1")
            .arg("-i")
            .arg(&self.path)
            .current_dir(dir)
            .status()
            .with_context(|| format!("failed to run patch: {}", self.path.display()))?;

        anyhow::ensure!(status.success(), "patch failed: {}", self.path.display());
        Ok(())
    }
}

impl PartialOrd for Patch {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Patch {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

fn discover_patches(dir: &Path) -> Result<Vec<Patch>> {
    let abs_dir = &dir
        .canonicalize()
        .with_context(|| format!("failed to canonicalize benchmark dir: {}", dir.display()))?;
    let mut patches = BTreeSet::new();
    for entry in fs::read_dir(abs_dir)? {
        let path = entry?.path();
        if path.extension() == Some(OsStr::new("patch")) {
            let patch = Patch::from_path(path.clone()).with_context(|| {
                format!("invalid patch filename '{}': expected N-name.patch", path.display())
            })?;
            patches.insert(patch);
        }
    }
    Ok(patches.into_iter().collect())
}
