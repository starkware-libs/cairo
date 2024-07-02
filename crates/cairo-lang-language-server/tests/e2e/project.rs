use cairo_lang_language_server::lsp;
use indoc::indoc;

use crate::support::normalize::assert_normalized_eq;
use crate::support::sandbox;

#[test]
fn cairo_projects() {
    let mut ls = sandbox! {
        files {
            "project1/cairo_project.toml" => indoc! {r#"
                [crate_roots]
                project1 = "src"
                
                [config.global]
                edition = "2023_11"
            "#},
            "project1/src/lib.cairo" => r#"fn main() {}"#,

            "project2/cairo_project.toml" => indoc! {r#"
                [crate_roots]
                project2 = "src"
                
                [config.global]
                edition = "2023_11"
            "#},
            "project2/src/lib.cairo" => r#"fn main() {}"#,

            "project2/subproject/cairo_project.toml" => indoc! {r#"
                [crate_roots]
                subproject = "src"
                
                [config.global]
                edition = "2023_11"
            "#},
            "project2/subproject/src/lib.cairo" => r#"fn main() {}"#,
        }
    };

    ls.open_and_wait_for_diagnostics("project1/src/lib.cairo");
    ls.open_and_wait_for_diagnostics("project2/src/lib.cairo");
    ls.open_and_wait_for_diagnostics("project2/subproject/src/lib.cairo");

    let projects = ls.send_request::<lsp::methods::test::DebugProjects>(());
    assert_normalized_eq!(
        &ls,
        projects,
        indoc! {r#"[
            CairoProject {
                project_path: "[ROOT]/project2/subproject/cairo_project.toml",
            },
            CairoProject {
                project_path: "[ROOT]/project2/cairo_project.toml",
            },
            CairoProject {
                project_path: "[ROOT]/project1/cairo_project.toml",
            },
        ]"#}
    );
}
