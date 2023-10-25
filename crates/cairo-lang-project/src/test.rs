use indoc::indoc;

use crate::{AllCratesConfig, ProjectConfigContent, SingleCrateConfig};

#[test]
fn test_serde() {
    let config = ProjectConfigContent {
        crate_roots: [
            ("crate1".into(), "dir1".into()),
            ("crate2".into(), "dir2".into()),
            ("crate3".into(), "dir3".into()),
        ]
        .into_iter()
        .collect(),
        crate_config: AllCratesConfig {
            global: SingleCrateConfig { compatibility: Default::default() },
            crate_override: [
                ("crate1".into(), SingleCrateConfig { compatibility: Default::default() }),
                ("crate3".into(), SingleCrateConfig { compatibility: Default::default() }),
            ]
            .into_iter()
            .collect(),
        },
    };
    let serialized = toml::to_string(&config).unwrap();
    assert_eq!(
        serialized,
        indoc! { r#"
            [crate_roots]
            crate1 = "dir1"
            crate2 = "dir2"
            crate3 = "dir3"
            
            [crate_config.global]
            compatibility = "0"

            [crate_config.crate_override.crate1]
            compatibility = "0"

            [crate_config.crate_override.crate3]
            compatibility = "0"
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}
