use cairo_lang_filesystem::db::{CrateSettings, Edition, ExperementalFeaturesConfig};
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::{AllCratesConfig, ProjectConfigContent};

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
            global: CrateSettings {
                edition: Default::default(),
                experimental_features: ExperementalFeaturesConfig::default(),
            },
            override_map: [
                (
                    "crate1".into(),
                    CrateSettings {
                        edition: Edition::V2023_10,
                        experimental_features: ExperementalFeaturesConfig::default(),
                    },
                ),
                (
                    "crate3".into(),
                    CrateSettings {
                        edition: Default::default(),
                        experimental_features: ExperementalFeaturesConfig { negative_impls: true },
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

            [config.global.experimental_features]
            negative_impls = false

            [config.override.crate1]
            edition = "2023_10"

            [config.override.crate1.experimental_features]
            negative_impls = false

            [config.override.crate3]
            edition = "2023_01"

            [config.override.crate3.experimental_features]
            negative_impls = true
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}
