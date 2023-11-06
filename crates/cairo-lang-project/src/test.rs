use cairo_lang_filesystem::db::Edition;
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
        crates_config: AllCratesConfig {
            global: SingleCrateConfig { edition: Default::default() },
            override_map: [
                ("crate1".into(), SingleCrateConfig { edition: Edition::V2023_10 }),
                ("crate3".into(), SingleCrateConfig { edition: Default::default() }),
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
            
            [config.global]
            edition = "2023_01"

            [config.override.crate1]
            edition = "2023_10"

            [config.override.crate3]
            edition = "2023_01"
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}
