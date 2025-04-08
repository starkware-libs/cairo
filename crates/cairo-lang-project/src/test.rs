use cairo_lang_filesystem::db::{CrateSettings, Edition, ExperimentalFeaturesConfig};
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
                name: None,
                edition: Default::default(),
                version: Default::default(),
                dependencies: Default::default(),
                experimental_features: ExperimentalFeaturesConfig::default(),
                cfg_set: Default::default(),
            },
            override_map: [
                (
                    "crate1".into(),
                    CrateSettings {
                        name: None,
                        edition: Edition::V2023_10,
                        version: Default::default(),
                        dependencies: Default::default(),
                        experimental_features: ExperimentalFeaturesConfig::default(),
                        cfg_set: Default::default(),
                    },
                ),
                (
                    "crate3".into(),
                    CrateSettings {
                        name: None,
                        edition: Default::default(),
                        version: Default::default(),
                        dependencies: Default::default(),
                        experimental_features: ExperimentalFeaturesConfig {
                            negative_impls: true,
                            associated_item_constraints: false,
                            coupons: false,
                        },
                        cfg_set: Default::default(),
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

            [config.global.dependencies]

            [config.global.experimental_features]
            negative_impls = false
            associated_item_constraints = false
            coupons = false

            [config.override.crate1]
            edition = "2023_10"

            [config.override.crate1.dependencies]

            [config.override.crate1.experimental_features]
            negative_impls = false
            associated_item_constraints = false
            coupons = false

            [config.override.crate3]
            edition = "2023_01"

            [config.override.crate3.dependencies]

            [config.override.crate3.experimental_features]
            negative_impls = true
            associated_item_constraints = false
            coupons = false
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}

#[test]
fn test_serde_defaults() {
    let config_str = indoc! { r#"
        [crate_roots]

        [config.global]
        edition = "2023_01"

        [config.global.experimental_features]
        negative_impls = false
        associated_item_constraints = false
    "# };
    let result = indoc! { r#"
        [crate_roots]

        [config.global]
        edition = "2023_01"

        [config.global.dependencies]

        [config.global.experimental_features]
        negative_impls = false
        associated_item_constraints = false
        coupons = false

        [config.override]
    "# };

    let config: ProjectConfigContent = toml::from_str(config_str).unwrap();
    assert_eq!(result, toml::to_string(&config).unwrap());
}
