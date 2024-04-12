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
