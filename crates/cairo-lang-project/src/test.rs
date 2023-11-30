use cairo_lang_filesystem::db::Edition;
use indoc::indoc;
use pretty_assertions::assert_eq;

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
            global: SingleCrateConfig { edition: Default::default(), experimental_features: None },
            override_map: [
                (
                    "crate1".into(),
                    SingleCrateConfig { edition: Edition::V2023_10, experimental_features: None },
                ),
                (
                    "crate3".into(),
                    SingleCrateConfig {
                        edition: Default::default(),
                        experimental_features: Some(crate::ExperementalFeaturesConfig {}),
                    },
                ),
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

            [config.override.crate3.experimental_features]
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}
