use cairo_lang_language_server::lsp;
use indoc::indoc;
use tower_lsp::lsp_types::{lsp_request, ApplyWorkspaceEditResponse, ExecuteCommandParams};

use crate::support::normalize::normalize;
use crate::support::sandbox;

#[test]
fn cairo_projects() {
    let mut ls = sandbox! {
        files {
            "project1/cairo_project.toml" => indoc! {r#"
                [crate_roots]
                project1 = "src"
            "#},
            "project1/src/lib.cairo" => "fn main() {}",

            "project2/cairo_project.toml" => indoc! {r#"
                [crate_roots]
                project2 = "src"
            "#},
            "project2/src/lib.cairo" => "fn main() {}",

            "project2/subproject/cairo_project.toml" => indoc! {r#"
                [crate_roots]
                subproject = "src"
            "#},
            "project2/subproject/src/lib.cairo" => "fn main() {}"
        }
    };

    ls.open_and_wait_for_diagnostics("project1/src/lib.cairo");
    ls.open_and_wait_for_diagnostics("project2/src/lib.cairo");
    ls.open_and_wait_for_diagnostics("project2/subproject/src/lib.cairo");

    let output = ls.send_request::<lsp::ext::ViewAnalyzedCrates>(());

    assert_eq!(
        normalize(&ls, output),
        indoc! {r#"
            # Analyzed Crates

            - `core`: `[CAIRO_SOURCE]/corelib/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2024_07,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: true,
                        coupons: true,
                    },
                }
                ```
            - `project1`: `[ROOT]/project1/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2023_01,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: false,
                        coupons: false,
                    },
                }
                ```
            - `project2`: `[ROOT]/project2/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2023_01,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: false,
                        coupons: false,
                    },
                }
                ```
            - `subproject`: `[ROOT]/project2/subproject/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2023_01,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: false,
                        coupons: false,
                    },
                }
                ```
        "#}
    );
}

#[test]
fn reload() {
    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => indoc! {r#"
                [crate_roots]
                hello = "src"
            "#},
            "src/lib.cairo" => "fn main() {}",
        }
    };

    ls.open_and_wait_for_diagnostics("src/lib.cairo");

    let expected = ls.send_request::<lsp::ext::ViewAnalyzedCrates>(());

    ls.expect_request::<lsp_request!("workspace/applyEdit")>(|_| ApplyWorkspaceEditResponse {
        applied: true,
        failure_reason: None,
        failed_change: None,
    });
    ls.send_request::<lsp_request!("workspace/executeCommand")>(ExecuteCommandParams {
        command: "cairo.reload".into(),
        ..Default::default()
    });

    let actual = ls.send_request::<lsp::ext::ViewAnalyzedCrates>(());

    assert_eq!(expected, actual);
}

#[test]
fn edit_cairo_project() {
    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => indoc! {r#"
                [crate_roots]
                hello = "src"
            "#},
            "src/lib.cairo" => "fn main() {}",
        }
    };

    ls.open_and_wait_for_diagnostics("src/lib.cairo");

    let output = ls.send_request::<lsp::ext::ViewAnalyzedCrates>(());
    assert_eq!(
        normalize(&ls, output),
        indoc! {r#"
            # Analyzed Crates

            - `core`: `[CAIRO_SOURCE]/corelib/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2024_07,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: true,
                        coupons: true,
                    },
                }
                ```
            - `hello`: `[ROOT]/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2023_01,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: false,
                        coupons: false,
                    },
                }
                ```
        "#}
    );

    ls.overwrite(
        "cairo_project.toml",
        indoc! {r#"
            [crate_roots]
            hello = "src"

            [config.global]
            edition = "2024_07"
        "#},
    );

    let output = ls.send_request::<lsp::ext::ViewAnalyzedCrates>(());
    assert_eq!(
        normalize(&ls, output),
        indoc! {r#"
            # Analyzed Crates

            - `core`: `[CAIRO_SOURCE]/corelib/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2024_07,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: true,
                        coupons: true,
                    },
                }
                ```
            - `hello`: `[ROOT]/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2024_07,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: false,
                        coupons: false,
                    },
                }
                ```
        "#}
    );

    ls.overwrite(
        "cairo_project.toml",
        indoc! {r#"
            [crate_roots]
            world = "src"

            [config.global]
            edition = "2024_07"
        "#},
    );

    let output = ls.send_request::<lsp::ext::ViewAnalyzedCrates>(());
    assert_eq!(
        normalize(&ls, output),
        indoc! {r#"
            # Analyzed Crates

            - `core`: `[CAIRO_SOURCE]/corelib/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2024_07,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: true,
                        coupons: true,
                    },
                }
                ```
            - `world`: `[ROOT]/src/lib.cairo`
                ```rust
                CrateSettings {
                    edition: V2024_07,
                    cfg_set: None,
                    experimental_features: ExperimentalFeaturesConfig {
                        negative_impls: false,
                        coupons: false,
                    },
                }
                ```
        "#}
    );
}
