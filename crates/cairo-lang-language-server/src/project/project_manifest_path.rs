use std::path::{Path, PathBuf};
use std::{fmt, fs};

use cairo_lang_project::PROJECT_FILE_NAME;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;
const SCARB_TOML: &str = "Scarb.toml";

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
    /// Look for a project manifest that **can** include source files at the given path.
    ///
    /// Returns `None` if the file at `path` is detached from any Cairo project.
    ///
    /// ## Precedence
    ///
    /// The following files are searched for in order:
    /// 1. `cairo_project.toml`
    /// 2. `Scarb.toml`
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

#[cfg(test)]
mod tests {
    use assert_fs::prelude::*;
    use assert_fs::TempDir;

    use super::*;

    #[test]
    fn discover_cairo_project_toml() {
        let t = TempDir::new().unwrap();

        let manifest = t.child("cairo_project.toml");
        manifest.touch().unwrap();

        let source_path = t.child("src/lib.cairo");
        source_path.touch().unwrap();

        let actual = ProjectManifestPath::discover(source_path.path());
        assert_eq!(actual, Some(ProjectManifestPath::CairoProject(manifest.to_path_buf())));
    }

    #[test]
    fn discover_scarb_toml() {
        let t = TempDir::new().unwrap();

        let manifest = t.child("Scarb.toml");
        manifest.touch().unwrap();

        let source_path = t.child("src/lib.cairo");
        source_path.touch().unwrap();

        let actual = ProjectManifestPath::discover(source_path.path());
        assert_eq!(actual, Some(ProjectManifestPath::Scarb(manifest.to_path_buf())));
    }

    #[test]
    fn discover_no_manifest() {
        let t = TempDir::new().unwrap();

        let source_path = t.child("src/lib.cairo");
        source_path.touch().unwrap();

        let actual = ProjectManifestPath::discover(source_path.path());
        assert_eq!(actual, None);
    }

    #[test]
    fn discover_precedence() {
        let t = TempDir::new().unwrap();

        let scarb_manifest = t.child("Scarb.toml");
        scarb_manifest.touch().unwrap();

        let cairo_manifest = t.child("cairo_project.toml");
        cairo_manifest.touch().unwrap();

        let source_path = t.child("src/lib.cairo");
        source_path.touch().unwrap();

        let actual = ProjectManifestPath::discover(source_path.path());
        assert_eq!(actual, Some(ProjectManifestPath::CairoProject(cairo_manifest.to_path_buf())));
    }
}
