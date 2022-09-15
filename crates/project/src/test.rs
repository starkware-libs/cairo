use indoc::indoc;

use crate::ProjectConfigInner;

#[test]
fn test_serde() {
    let config =
        ProjectConfigInner { crate_roots: [("crate".into(), "dir".into())].into_iter().collect() };
    let serialized = toml::to_string(&config).unwrap();
    assert_eq!(
        serialized,
        indoc! { r#"
            [crate_roots]
            crate = "dir"
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}
