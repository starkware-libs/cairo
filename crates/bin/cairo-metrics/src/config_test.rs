use super::*;

#[test]
fn test_parse_minimal_benchmark() {
    let toml = r#"
path = "corelib/"
"#;
    let benchmark: BenchmarkConfig = toml::from_str(toml).unwrap();
    assert_eq!(benchmark.path, PathBuf::from("corelib/"));
}

#[test]
fn test_command_spec_display() {
    let spec = CommandSpec { binary: PathBuf::from("scarb"), args: vec!["build".into()] };
    assert_eq!(spec.display(), "scarb build");
}

#[test]
fn test_command_spec_display_no_args() {
    let spec = CommandSpec { binary: PathBuf::from("true"), args: vec![] };
    assert_eq!(spec.display(), "true");
}

#[test]
fn test_defaults() {
    let defaults = Defaults::default();
    assert_eq!(defaults.runs, 5);
    assert_eq!(defaults.warmup, 1);
}

#[test]
fn test_patch_discovery() {
    let temp_dir = std::env::temp_dir().join("cairo-metrics-test-patches");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    // Create some patch files.
    std::fs::write(temp_dir.join("0-first.patch"), "patch content").unwrap();
    std::fs::write(temp_dir.join("1-second.patch"), "patch content").unwrap();
    std::fs::write(temp_dir.join("not-a-patch.txt"), "ignore me").unwrap();

    let config = BenchmarkConfig { path: PathBuf::from("corelib"), library: false };
    let benchmark = config.into_benchmark(&temp_dir).unwrap();

    assert_eq!(benchmark.patches.len(), 2);
    assert_eq!(benchmark.patches[0].index, 0);
    assert_eq!(benchmark.patches[0].name, "first");
    assert_eq!(benchmark.patches[1].index, 1);
    assert_eq!(benchmark.patches[1].name, "second");

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn test_patch_from_path() {
    let path = PathBuf::from("/tmp/0-example.patch");
    let patch = Patch::from_path(path.clone()).unwrap();
    assert_eq!(patch.index, 0);
    assert_eq!(patch.name, "example");
    assert_eq!(patch.path, path);
}

#[test]
fn test_patch_from_path_invalid() {
    assert!(Patch::from_path(PathBuf::from("/tmp/invalid.patch")).is_none());
    assert!(Patch::from_path(PathBuf::from("/tmp/notapatch.txt")).is_none());
}

#[test]
fn test_supports_incremental_false() {
    let temp_dir = std::env::temp_dir().join("cairo-metrics-test-no-incr");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let config = BenchmarkConfig { path: PathBuf::from("corelib"), library: true };
    let benchmark = config.into_benchmark(&temp_dir).unwrap();

    assert!(!benchmark.supports_incremental());

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn test_supports_incremental_true() {
    let temp_dir = std::env::temp_dir().join("cairo-metrics-test-incr");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let config = BenchmarkConfig { path: PathBuf::from("test"), library: false };
    let benchmark = config.into_benchmark(&temp_dir).unwrap();

    assert!(benchmark.supports_incremental());

    let _ = std::fs::remove_dir_all(&temp_dir);
}
