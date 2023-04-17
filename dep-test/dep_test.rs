use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Lines, Read};
use std::path::PathBuf;
use std::{fs, io};

use toml::Value;

/// Tests that all dependencies that are listed in the Cargo.toml file are also listed in
/// .github/workflows/detect_job_deps.js.
/// If this test fails, you probably added a dependency in one of the Cargo.toml files, but forgot
/// to add it to the .github/workflows/detect_job_deps.js file (under `add_all_jobs_to_run`).
#[test]
fn test_deps() {
    let manual_deps = get_detect_job_add_job_to_run_deps();

    for (crt_name, crt_path) in [
        ("casm", "crates/cairo-lang-casm"),
        ("compiler", "crates/cairo-lang-compiler"),
        ("debug", "crates/cairo-lang-debug"),
        ("defs", "crates/cairo-lang-defs"),
        ("diagnostics", "crates/cairo-lang-diagnostics"),
        ("eq-solver", "crates/cairo-lang-eq-solver"),
        ("filesystem", "crates/cairo-lang-filesystem"),
        ("formatter", "crates/cairo-lang-formatter"),
        ("language-server", "crates/cairo-lang-language-server"),
        ("lowering", "crates/cairo-lang-lowering"),
        ("parser", "crates/cairo-lang-parser"),
        ("plugins", "crates/cairo-lang-plugins"),
        ("proc-macros", "crates/cairo-lang-proc-macros"),
        ("project", "crates/cairo-lang-project"),
        ("runner", "crates/cairo-lang-runner"),
        ("semantic", "crates/cairo-lang-semantic"),
        ("sierra", "crates/cairo-lang-sierra"),
        ("sierra-ap-change", "crates/cairo-lang-sierra-ap-change"),
        ("sierra-gas", "crates/cairo-lang-sierra-gas"),
        ("sierra-generator", "crates/cairo-lang-sierra-generator"),
        ("sierra-to-casm", "crates/cairo-lang-sierra-to-casm"),
        ("starknet", "crates/cairo-lang-starknet"),
        ("syntax", "crates/cairo-lang-syntax"),
        ("syntax-codegen", "crates/cairo-lang-syntax-codegen"),
        ("test-runner", "crates/cairo-lang-test-runner"),
        ("utils", "crates/cairo-lang-utils"),
        ("tests", "tests"),
    ] {
        let cur_cargo_toml_deps = get_cargo_toml_deps(&format!("{crt_path}/Cargo.toml",));

        let cur_manual_deps = manual_deps
            .get(crt_name)
            .unwrap_or_else(|| panic!("crate '{crt_name}' does not have a `add_job_to_run` call"))
            .clone();
        assert_eq!(
            HashSet::<String>::from_iter(cur_cargo_toml_deps),
            HashSet::<String>::from_iter(cur_manual_deps)
        );
    }
}

/// `file_path` is the path under the root of the repo.
fn get_cargo_toml_deps(file_path: &str) -> Vec<String> {
    let full_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned().join(file_path);
    let mut file = fs::File::open(full_path.clone())
        .unwrap_or_else(|_| panic!("File not found: {full_path:?}"));

    let mut cargo_toml_str = "".to_string();
    file.read_to_string(&mut cargo_toml_str).unwrap();

    let mut dependencies = Vec::new();
    let config = cargo_toml_str.parse::<Value>().unwrap();
    if let Value::Table(t) = config {
        for (k, v) in t {
            if k == "dependencies" || k == "dev-dependencies" {
                if let Value::Table(inner_table) = v {
                    for (dep_name, dep_value) in inner_table {
                        if dep_name.starts_with("cairo-lang-") {
                            assert!(dep_value["path"].as_str().unwrap().contains(&dep_name));
                            dependencies.push(dep_name);
                        }
                    }
                }
            }
        }
    }

    dependencies
}

// TODO(yuval): this is an ugly function parsing very specific code (also depending on formatting).
// Change the dependencies to be defined in an external file (e.g. json). Then - here parse the
// json, and in `detect_crates_to_test.js`, call `add_job_to_run` on each crate according to the
// dependencies specified in the json file.
// TODO(yuval): an even better way is to directly parse the Cargo.toml files from
// detect_crates_to_test.js. This will make this test redundant
/// Get the manual deps listed in the call to `add_job_to_run` in
/// ".github/workflows/detect_crates_to_test.js".
fn get_detect_job_add_job_to_run_deps() -> HashMap<String, Vec<String>> {
    let filename = ".github/workflows/detect_job_deps.js";
    let full_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned().join(filename);
    let file = fs::File::open(full_path.clone())
        .unwrap_or_else(|_| panic!("File not found: {full_path:?}"));

    let mut job_deps: HashMap<String, Vec<String>> = HashMap::new();
    let mut lines = io::BufReader::new(file).lines();
    while let Some(Ok(line)) = lines.next() {
        let line = line.trim();
        if line.starts_with("add_job_to_run") {
            if line.contains("],") {
                // One liner call.
                let job_name =
                    line.strip_prefix("add_job_to_run(\"").unwrap().split_once('"').unwrap().0;
                job_deps.insert(job_name.to_string(), get_single_line_deps(line));
                continue;
            }

            // Multiline call.

            // job_name parameter.
            let line = next_line_or_panic(&mut lines);
            let job_name = line.strip_prefix('"').unwrap();
            let job_name = job_name.strip_suffix("\",").unwrap();

            // Skip modifiedDeps parameter.
            let line = next_line_or_panic(&mut lines);
            assert_eq!(line.trim(), "modifiedDeps,");

            // Assert next line is "[".
            let line = next_line_or_panic(&mut lines);
            let cur_deps = if line.starts_with('[') && line.ends_with("],") {
                // Single line array of deps.
                get_single_line_deps(&line)
            } else {
                // Multiline array of deps.
                assert_eq!(line.trim(), "[");

                let mut cur_deps = Vec::new();
                loop {
                    let line = next_line_or_panic(&mut lines);
                    if line.contains("],") {
                        break;
                    }
                    let dep = line.strip_prefix('"').unwrap();
                    let dep = dep.strip_suffix("\",").unwrap().to_string();
                    cur_deps.push(format!("cairo-lang-{dep}"));
                }

                // Skip jobsToRun parameter.
                let line = next_line_or_panic(&mut lines);
                assert_eq!(line.trim(), "jobsToRun");

                // Assert next line is ");".
                let line = next_line_or_panic(&mut lines);
                assert_eq!(line.trim(), ");");
                cur_deps
            };

            job_deps.insert(job_name.to_string(), cur_deps);
        }
    }

    job_deps
}

fn get_single_line_deps(line: &str) -> Vec<String> {
    line.split_once('[')
        .unwrap()
        .1
        .split_once(']')
        .unwrap()
        .0
        .split_terminator(',')
        .map(|dep| {
            format!(
                "cairo-lang-{}",
                dep.trim().strip_prefix('"').unwrap().strip_suffix('"').unwrap()
            )
        })
        .collect()
}

fn next_line_or_panic(lines: &mut Lines<BufReader<File>>) -> String {
    let Some(Ok(line)) = lines.next() else { panic!(); };
    line.trim().to_string()
}

/// Helper operations on `Option<T>`.
pub trait ResultHelper<E> {
    fn on_err<F: FnOnce(&E)>(self, f: F) -> Self;
}
impl<T, E> ResultHelper<E> for Result<T, E> {
    fn on_err<F: FnOnce(&E)>(self, f: F) -> Self {
        match &self {
            Ok(_) => self,
            Err(e) => {
                f(e);
                self
            }
        }
    }
}
