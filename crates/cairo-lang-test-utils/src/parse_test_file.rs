#[cfg(test)]
#[path = "parse_test_file_test.rs"]
mod test;
use std::fs;
use std::io::{self, BufRead};
use std::path::Path;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ResultHelper;
use colored::Colorize;

const TAG_PREFIX: &str = "//! > ";
const TEST_SEPARATOR: &str =
    "==========================================================================";

#[derive(Default)]
struct Tag {
    name: String,
    content: String,
}

/// Represents a single test from the test file.
#[derive(Clone)]
pub struct Test {
    pub attributes: OrderedHashMap<String, String>,
    pub line_num: usize,
}

#[derive(Default)]
struct TestBuilder {
    tests: OrderedHashMap<String, Test>,
    current_test: Option<Test>,
    current_test_name: Option<String>,
    current_tag: Option<Tag>,
}

pub fn parse_test_file(filename: &Path) -> io::Result<OrderedHashMap<String, Test>> {
    let file = fs::File::open(filename).on_err(|_| log::error!("File not found: {filename:?}"))?;
    let mut lines = io::BufReader::new(file).lines();
    let mut builder = TestBuilder::default();
    let mut line_num: usize = 0;
    while let Some(Ok(line)) = lines.next() {
        line_num += 1;
        if let Some(line) = line.strip_prefix(TAG_PREFIX) {
            if builder.current_test_name.is_none() {
                builder.set_test_name(line.into(), line_num);
            } else if line.starts_with("===") {
                // Separate tests.
                assert_eq!(line, TEST_SEPARATOR, "Wrong test separator on line {line_num}.");
                builder.new_test()
            } else {
                builder.open_tag(line.into());
            }
        } else {
            builder.add_content_line(line);
        }
    }
    Ok(builder.finalize())
}

pub fn dump_to_test_file(
    tests: OrderedHashMap<String, Test>,
    filename: &str,
) -> Result<(), std::io::Error> {
    let mut test_strings = Vec::new();
    for (test_name, test) in tests {
        let mut tag_strings = vec![TAG_PREFIX.to_string() + &test_name];
        for (tag, content) in test.attributes {
            tag_strings.push(
                TAG_PREFIX.to_string()
                    + &tag
                    + if content.is_empty() { "" } else { "\n" }
                    + &content,
            );
        }
        test_strings.push(tag_strings.join("\n\n"));
    }
    fs::write(
        filename,
        test_strings.join(&("\n\n".to_string() + TAG_PREFIX + TEST_SEPARATOR + "\n\n")) + "\n",
    )
}

impl TestBuilder {
    /// Closes a tag if one is open, otherwise does nothing.
    fn close_open_tag(&mut self) {
        if let Some(tag) = &mut self.current_tag {
            let attributes = &mut self.current_test.as_mut().unwrap().attributes;
            assert!(
                !attributes.contains_key(&tag.name),
                "Duplicate tag '{}' found in test (test name: {}).",
                tag.name,
                self.current_test_name.as_ref().unwrap_or(&"<unknown>".into())
            );
            attributes.insert(std::mem::take(&mut tag.name), tag.content.trim().to_string());
            self.current_tag = None;
        }
    }

    fn open_tag(&mut self, line: String) {
        self.close_open_tag();
        self.current_tag = Some(Tag { name: line, content: "".into() });
    }

    fn set_test_name(&mut self, line: String, line_num: usize) {
        self.current_test_name = Some(line);
        self.current_test = Some(Test { attributes: OrderedHashMap::default(), line_num });
    }

    fn add_content_line(&mut self, line: String) {
        if let Some(tag) = &mut self.current_tag {
            if !tag.content.is_empty() {
                tag.content += "\n"
            }
            tag.content += &line;
        } else {
            // Only allow empty lines outside tags.
            assert!(line.is_empty(), "No tag found for content line: '{line}'.");
        }
    }

    fn new_test(&mut self) {
        self.close_open_tag();
        let name = self.current_test_name.as_ref().expect("No name found for test.");
        let old_test =
            self.tests.insert(name.clone(), std::mem::take(&mut self.current_test).unwrap());
        assert!(old_test.is_none(), "Found two tests named {name}.");
        self.current_test_name = None;
        self.current_tag = None;
    }

    fn finalize(&mut self) -> OrderedHashMap<String, Test> {
        self.new_test();
        std::mem::take(&mut self.tests)
    }
}

/// Trait for running a parsed test file.
pub trait TestFileRunner {
    /// Reads tags from the input map, and returns the output map, that should match the expected
    /// outputs.
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        runner_args: &OrderedHashMap<String, String>,
    ) -> Result<OrderedHashMap<String, String>, String>;
}

/// Creates a test that reads test files for a given function.
/// test_name - the name of the test.
/// filenames - a vector of tests files the test applies to.
/// runner - the struct implementing `TestFileRunner + Default`.
///
/// The structure of the file must be of the following form:
/// ```text
/// //! > test description
///
/// //! > test_runner_name
/// TestToUpperRunner
///
/// //! > input1
/// hello
///
/// //! > input2
/// world
///
/// //! > expected_output1
/// HELLO
///
/// //! > expected_output2
/// WORLD
///
/// //! > ==========================================================================
///
/// <another test>
/// ```
///
/// The call to the macro looks like:
/// ```ignore
/// test_file_test_with_runner!(
///     test_suite_name,
///     "path/to/test/dir",
///     {
///         test_name1: "test_file1",
///         test_name2: "test_file2",
///     },
///     TestToUpperRunner
/// );
/// ```
#[macro_export]
macro_rules! test_file_test_with_runner {
    ($suite:ident, $base_dir:expr, { $($test_name:ident : $test_file:expr),* $(,)? }, $runner:ident) => {
        mod $suite {
            use super::*;
        $(
            #[test_log::test]
            fn $test_name() -> Result<(), std::io::Error> {
                let path: std::path::PathBuf = [env!("CARGO_MANIFEST_DIR"), $base_dir, $test_file].iter().collect();
                cairo_lang_test_utils::parse_test_file::run_test_file(
                    path.as_path(),
                    stringify!($runner),
                    &mut $runner::default(),
                )
            }
        )*
        }
    };
}

/// Simple runner wrapping a test function.
pub struct SimpleRunner {
    pub func: fn(
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> Result<OrderedHashMap<String, String>, String>,
}
impl TestFileRunner for SimpleRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        runner_args: &OrderedHashMap<String, String>,
    ) -> Result<OrderedHashMap<String, String>, String> {
        (self.func)(inputs, runner_args)
    }
}

/// Creates a test that reads test files for a given function.
/// test_name - the name of the test.
/// filenames - a vector of test files the test applies to.
/// func - the function to be applied on the test params to generate the tested result.
///
/// The signature of `func` should be of the form:
/// ```ignore
/// fn func(
///     inputs: &OrderedHashMap<String, String>,
///     args: &OrderedHashMap<String, String>,
/// ) -> OrderedHashMap<String, String>;
/// ```
/// And `func` can read the tags from the file from the `inputs` map. It should return the expected
/// outputs with the same tags as the file.
/// `args` can be provided in the "test_runner_name" tag and can be used in `func` as needed.
///
/// The structure of the file must be of the following form:
/// ```text
/// //! > <test description>
///
/// //! > test_runner_name
/// test_to_upper(optional_arg1: val1, optional_arg2: val2)
///
/// //! > input1
/// hello
///
/// //! > input2
/// world
///
/// //! > expected_output1
/// HELLO
///
/// //! > expected_output2
/// WORLD
///
/// //! > ==========================================================================
///
/// <another test>
/// ```
///
/// The call to the macro looks like:
/// ```ignore
/// test_file_test!(
///     test_suite_name,
///     "path/to/test/dir",
///     {
///         test_name1: "test_file1",
///         test_name2: "test_file2",
///     },
///     test_to_upper
/// );
/// ```
#[macro_export]
macro_rules! test_file_test {
    ($suite:ident, $base_dir:expr, { $($test_name:ident : $test_file:expr),* $(,)? }, $test_func:ident) => {
        mod $suite {
            use super::*;
        $(
            #[test_log::test]
            fn $test_name() -> Result<(), std::io::Error> {
                let path: std::path::PathBuf = [env!("CARGO_MANIFEST_DIR"), $base_dir, $test_file].iter().collect();
                cairo_lang_test_utils::parse_test_file::run_test_file(
                    path.as_path(),
                    stringify!($test_func),
                    &mut cairo_lang_test_utils::parse_test_file::SimpleRunner { func: $test_func },
                )
            }
        )*
        }
    };
}

/// Runs a test based on file at `path` named `test_func_name` by running `test_func` on it.
/// Fixes the test file if the `CAIRO_FIX_TESTS` environment variable is set to `1`.
pub fn run_test_file(
    path: &Path,
    runner_name: &str,
    runner: &mut dyn TestFileRunner,
) -> Result<(), std::io::Error> {
    let filename = path.file_name().unwrap().to_str().unwrap();
    let is_fix_mode = std::env::var("CAIRO_FIX_TESTS") == Ok("1".into());
    let tests = parse_test_file(path)?;
    let mut new_tests = OrderedHashMap::<String, Test>::default();

    let mut errors = Vec::new();
    let mut passed_tests = 0;
    let mut failed_tests = Vec::new();
    for (test_name, test) in tests {
        let line_num = test.line_num;
        let test_path = format!(r#"{runner_name}::{filename}::"{test_name}" (line: {line_num})"#);
        let full_filename = std::fs::canonicalize(path)?;
        let full_filename_str = full_filename.to_str().unwrap();

        let get_attr = |key: &str| {
            test.attributes.get(key).unwrap_or_else(|| {
                panic!(
                    "Missing attribute '{key}' in test '{test_path}'.\nIn \
                     {full_filename_str}:{line_num}"
                )
            })
        };

        let runner_tag = get_attr("test_runner_name").as_str();
        let (name, runner_args) = if let Some((name, args_str)) = runner_tag.split_once('(') {
            (
                name,
                args_str
                    .strip_suffix(')')
                    .unwrap()
                    .split(',')
                    .map(|param| {
                        let (param_name, param_value) = param.split_once(':').unwrap();
                        (param_name.to_string(), param_value.to_string())
                    })
                    .collect::<OrderedHashMap<_, _>>(),
            )
        } else {
            (runner_tag, OrderedHashMap::default())
        };
        pretty_assertions::assert_eq!(name, runner_name);

        // Run the test.
        log::debug!("Running test: {test_path}");
        let outputs = match runner.run(&test.attributes, &runner_args) {
            Ok(outputs) => outputs,
            Err(err) => {
                errors.push(format!(
                    "Test \"{test_name}\" failed.\nIn \
                     {full_filename_str}:{line_num}.\n{err}\nRerun with CAIRO_FIX_TESTS=1 to fix."
                ));
                failed_tests.push(test_path);
                if is_fix_mode {
                    new_tests.insert(test_name.to_string(), test.clone());
                }
                continue;
            }
        };

        if is_fix_mode {
            let mut new_test = test.clone();
            for (key, value) in outputs {
                new_test.attributes.insert(key.to_string(), value.trim().to_string());
            }
            new_tests.insert(test_name.to_string(), new_test);
            passed_tests += 1;
        } else {
            let mut cur_test_errors = Vec::new();
            for (key, value) in outputs {
                let expected_value = get_attr(&key);
                let actual_value = value.trim();
                if actual_value != expected_value {
                    cur_test_errors.push(format!(
                        "Output tag '{key}' does not \
                         match.\nActual:\n{actual_value}\nExpected:\n{expected_value}\n\n",
                    ));
                }
            }
            if cur_test_errors.is_empty() {
                passed_tests += 1;
            } else {
                errors.push(format!(
                    "Test \"{test_name}\" failed.\nIn {full_filename_str}:{line_num}.\nRerun with \
                     CAIRO_FIX_TESTS=1 to fix.\n{err}",
                    err = cur_test_errors.join("")
                ));
                failed_tests.push(test_path);
            }
        }
    }
    if is_fix_mode {
        dump_to_test_file(new_tests, path.to_str().unwrap())?;
    }

    assert!(
        errors.is_empty(),
        "\n\n{}\n\n{}\n",
        errors.join("\n\n"),
        summary(passed_tests, &failed_tests)
    );

    Ok(())
}

fn summary(passed_tests: usize, failed_tests: &[String]) -> String {
    let passed = format!("{} passed", passed_tests).green();
    let failed = format!("{} failed", failed_tests.len()).red();
    format!("Summary: {passed}, {failed}:\n{}", failed_tests.join("\n"))
}
