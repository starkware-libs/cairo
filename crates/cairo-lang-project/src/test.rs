use indoc::indoc;

use crate::{CrateConfigContent, CratesConfigContent, ProjectConfigContent};

#[test]
fn test_serde() {
    let config = ProjectConfigContent {
        crate_roots: [("crate_name".into(), "dir".into())].into_iter().collect(),
        crate_config: CratesConfigContent {
            global: CrateConfigContent { compatibility: Default::default() },
            crate_override: [(
                "crate_name".into(),
                CrateConfigContent { compatibility: Default::default() },
            )]
            .into_iter()
            .collect(),
        },
    };
    let serialized = toml::to_string(&config).unwrap();
    assert_eq!(
        serialized,
        indoc! { r#"
            [crate_roots]
            crate_name = "dir"
            
            [crate_config.global]
            compatibility = "V0"

            [crate_config.crate_override.crate_name]
            compatibility = "V0"
        "# }
    );
    assert_eq!(config, toml::from_str(&serialized).unwrap());
}
