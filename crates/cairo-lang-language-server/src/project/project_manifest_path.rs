use std::path::{Path, PathBuf};
use std::{fmt, fs};

use cairo_lang_project::PROJECT_FILE_NAME;

use crate::toolchain::scarb::SCARB_TOML;

#[cfg(test)]
#[path = "project_manifest_path_test.rs"]
mod project_manifest_path_test;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;

/// An absolute path to a manifest file of a single Cairo project.
#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum ProjectManifestPath {
    /// `cairo_project.toml` file.
    CairoProject(PathBuf),

    /// `Scarb.toml` file.
    ///
    /// This could either be a single package or a workspace manifest.
    Scarb(PathBuf),
}

impl ProjectManifestPath {
    /// Looks for a project manifest that **can** include source files at the given path.
    ///
    /// Returns `None` if the file at `path` is detached from any Cairo project.
    ///
    /// ## Precedence
    ///
    /// The following files are searched for in order:
    /// 1. `cairo_project.toml`
    /// 2. `Scarb.toml`
    ///
    /// This precedence rule also applies to manifest files themselves.
    /// If there are all `cairo_project.toml`, `Scarb.toml` and `Scarb.lock`
    /// files in the same directory, the `cairo_project.toml` file will be chosen for each.
    pub fn discover(path: &Path) -> Option<ProjectManifestPath> {
        return find_in_parent_dirs(path.to_path_buf(), PROJECT_FILE_NAME)
            .map(ProjectManifestPath::CairoProject)
            .or_else(|| {
                find_in_parent_dirs(path.to_path_buf(), SCARB_TOML).map(ProjectManifestPath::Scarb)
            });

        fn find_in_parent_dirs(mut path: PathBuf, target_file_name: &str) -> Option<PathBuf> {
            for _ in 0..MAX_CRATE_DETECTION_DEPTH {
                if !path.pop() {
                    return None;
                }

                let manifest_path = path.join(target_file_name);
                // Check if the file exists and we can actually access it.
                if fs::metadata(&manifest_path).is_ok() {
                    return Some(manifest_path);
                };
            }
            None
        }
    }

    /// Gets the underlying [`Path`] of the manifest file.
    pub fn as_path(&self) -> &Path {
        match self {
            ProjectManifestPath::CairoProject(path) | ProjectManifestPath::Scarb(path) => path,
        }
    }
}

impl fmt::Display for ProjectManifestPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProjectManifestPath::CairoProject(path) | ProjectManifestPath::Scarb(path) => {
                fmt::Display::fmt(&path.display(), f)
            }
        }
    }
}
