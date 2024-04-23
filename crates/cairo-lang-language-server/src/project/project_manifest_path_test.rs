use std::path::{Path, PathBuf};

use assert_fs::prelude::*;
use assert_fs::TempDir;
use cairo_lang_project::PROJECT_FILE_NAME;

use super::ProjectManifestPath;
use crate::toolchain::scarb::{SCARB_LOCK, SCARB_TOML};

fn check_some(
    source: impl AsRef<Path>,
    manifest: impl AsRef<Path>,
    constructor: impl FnOnce(PathBuf) -> ProjectManifestPath,
) {
    let source = source.as_ref();
    let manifest = manifest.as_ref().to_path_buf();
    assert_eq!(ProjectManifestPath::discover(source), Some(constructor(manifest)));
}

fn check_none(source: impl AsRef<Path>) {
    let source = source.as_ref();
    assert_eq!(ProjectManifestPath::discover(source), None);
}

#[test]
fn discover_cairo_project_toml() {
    let t = TempDir::new().unwrap();

    let manifest = t.child(PROJECT_FILE_NAME);
    manifest.touch().unwrap();

    let source_path = t.child("src/lib.cairo");
    source_path.touch().unwrap();

    let sibling = t.child("foo.cairo");
    sibling.touch().unwrap();

    check_some(&source_path, &manifest, ProjectManifestPath::CairoProject);
    check_some(&manifest, &manifest, ProjectManifestPath::CairoProject);
    check_some(&sibling, &manifest, ProjectManifestPath::CairoProject);
}

#[test]
fn discover_scarb_toml() {
    let t = TempDir::new().unwrap();

    let manifest = t.child(SCARB_TOML);
    manifest.touch().unwrap();

    let source_path = t.child("src/lib.cairo");
    source_path.touch().unwrap();

    let sibling = t.child("foo.cairo");
    sibling.touch().unwrap();

    check_some(&source_path, &manifest, ProjectManifestPath::Scarb);
    check_some(&manifest, &manifest, ProjectManifestPath::Scarb);
    check_some(&sibling, &manifest, ProjectManifestPath::Scarb);
}

#[test]
fn discover_no_manifest() {
    let t = TempDir::new().unwrap();

    let source_path = t.child("src/lib.cairo");
    source_path.touch().unwrap();

    check_none(&source_path);
}

#[test]
fn discover_precedence() {
    let t = TempDir::new().unwrap();

    let scarb_manifest = t.child(SCARB_TOML);
    scarb_manifest.touch().unwrap();

    let scarb_lock = t.child(SCARB_LOCK);
    scarb_lock.touch().unwrap();

    let cairo_manifest = t.child(PROJECT_FILE_NAME);
    cairo_manifest.touch().unwrap();

    let source_path = t.child("src/lib.cairo");
    source_path.touch().unwrap();

    let sibling = t.child("foo.cairo");
    sibling.touch().unwrap();

    check_some(&source_path, &cairo_manifest, ProjectManifestPath::CairoProject);
    check_some(&sibling, &cairo_manifest, ProjectManifestPath::CairoProject);
    check_some(&cairo_manifest, &cairo_manifest, ProjectManifestPath::CairoProject);
    check_some(&scarb_manifest, &cairo_manifest, ProjectManifestPath::CairoProject);
    check_some(&scarb_lock, &cairo_manifest, ProjectManifestPath::CairoProject);
}
