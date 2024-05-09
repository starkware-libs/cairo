use cairo_lang_language_server::lsp;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::support::normalize::normalize;
use crate::support::sandbox;

cairo_lang_test_utils::test_file_test!(
    hover,
    "tests/test_data/project",
    {
        cairo_projects: "cairo_projects.txt",
    },
    test_project_model
);

fn test_project_model(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let dyn_files = inputs.iter().flat_map(|(p, c)| Some((p.strip_prefix("file: ")?, c)));

    let mut ls = sandbox! {
        dyn_files(dyn_files)
    };

    for path_to_open in inputs["open files in order"].lines() {
        ls.open_and_wait_for_diagnostics(path_to_open);
    }

    let projects = ls.send_request::<lsp::methods::test::DebugProjects>(());
    let projects = normalize(&ls, projects);

    TestRunnerResult::success(OrderedHashMap::from([(
        "expected project model".to_owned(),
        projects,
    )]))
}
