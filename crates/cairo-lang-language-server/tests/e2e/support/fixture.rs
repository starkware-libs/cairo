use std::fs;
use std::path::{Path, PathBuf};

use assert_fs::prelude::*;
use assert_fs::TempDir;
use lsp_types::Uri;

/// A temporary directory that is a context for testing the language server.
pub struct Fixture {
    t: TempDir,
    files: Vec<PathBuf>,
}

impl Fixture {
    /// Creates a new [`Fixture`] with an empty temporary directory.
    pub fn new() -> Self {
        let t = TempDir::new().unwrap();
        Self { t, files: Vec::new() }
    }
}

/// Builder methods.
impl Fixture {
    /// Creates a new file in the fixture with provided contents.
    pub fn add_file(&mut self, path: impl AsRef<Path>, contents: impl AsRef<str>) {
        self.files.push(path.as_ref().to_owned());
        self.t.child(path).write_str(contents.as_ref().trim()).unwrap();
    }
}

/// Introspection methods.
impl Fixture {
    pub fn root_path(&self) -> &Path {
        self.t.path()
    }

    pub fn root_uri(&self) -> Uri {
        self.t.path().to_str().unwrap().parse().unwrap()
    }

    pub fn file_absolute_path(&self, path: impl AsRef<Path>) -> PathBuf {
        self.t.child(path).path().to_owned()
    }

    pub fn file_uri(&self, path: impl AsRef<Path>) -> Uri {
        format!("file://{}", self.file_absolute_path(path).to_str().unwrap()).parse().unwrap()
    }

    pub fn read_file(&self, path: impl AsRef<Path>) -> String {
        fs::read_to_string(self.file_absolute_path(path)).unwrap()
    }
}
