#[cfg(test)]
#[path = "parse_test_file_test.rs"]
mod test;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

use crate::ordered_hash_map::OrderedHashMap;

const TAG_PREFIX: &str = "//! > ";

#[derive(Default)]
struct Tag {
    name: String,
    content: String,
}

/// Represents a single test from the test file.
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
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines();
    let mut builder = TestBuilder::default();
    let mut line_num: usize = 0;
    while let Some(Ok(line)) = lines.next() {
        line_num += 1;
        if let Some(line) = line.strip_prefix(TAG_PREFIX) {
            if builder.current_test_name == None {
                builder.set_test_name(line.into(), line_num);
            } else if line.starts_with("===") {
                // Separate tests.
                assert_eq!(
                    line,
                    "==========================================================================",
                    "Wrong test separator."
                );
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
        self.tests.insert(
            self.current_test_name.as_ref().expect("No name found for test.").into(),
            std::mem::take(&mut self.current_test).unwrap(),
        );
        self.current_test_name = None;
        self.current_tag = None;
    }

    fn finalize(&mut self) -> OrderedHashMap<String, Test> {
        self.new_test();
        std::mem::take(&mut self.tests)
    }
}

/// Creates a test that reads test files for a given function.
/// test_name - the name of the test.
/// filenames - a vector of tests files the test applies to.
/// func - the function to be applied on the test params to generate the tested result.
///
/// The signature of `func` should be of the form:
/// ```ignore
/// fn func(
///     db: &mut SomeCrateDatabaseForTesting,
///     inputs: &OrderedHashMap<String, String>
/// ) -> OrderedHashMap<String, String>;
/// ```
/// And `func` can read the tags from the file from the input map. It should return the expected
/// outputs with the same tags as the file, in the output map.
///
/// The structure of the file must be of the following form:
/// ```text
/// //! > test description
///
/// //! > test_function_name
/// test_to_upper
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
/// Each crate should define its own wrapper for it with the relevant Database for testing, e.g.:
/// ```ignore
/// #[macro_export]
/// macro_rules! parser_test {
///     ($test_name:ident, $filenames:expr, $func:ident) => {
///         utils::test_file_test!($test_name, $filenames, ParserDatabaseForTesting, $func);
///     };
/// }
/// ```
///
/// Then, the call to the macro looks like:
/// ```ignore
/// parser_test!(unique_test_name, [<test_file1>, <test_file2], test_to_upper);
/// ```
#[macro_export]
macro_rules! test_file_test {
    ($test_name:ident, $filenames:expr, $db_type:ty, $func:ident) => {
        #[test]
        fn $test_name() -> Result<(), std::io::Error> {
            // TODO(mkaput): consider extracting this part into a function and passing macro args
            // via refs or closures. It may reduce compilation time sizeably.
            for filename in $filenames {
                let tests = utils::parse_test_file(std::path::Path::new(filename))?;
                // TODO(alont): global tags for all tests in a file.
                for (name, test) in tests {
                    assert_eq!(test.attributes["test_function_name"], stringify!($func));

                    let outputs = $func(&mut <$db_type>::default(), &test.attributes);
                    let line_num = test.line_num;
                    let full_filename = std::fs::canonicalize(filename)?;
                    let full_filename_str = full_filename.to_str().unwrap();

                    for (key, value) in outputs {
                        pretty_assertions::assert_eq!(
                            value.trim(),
                            test.attributes[key],
                            "Test \"{name}\" failed.\nIn {full_filename_str}:{line_num}"
                        );
                    }
                }
            }
            Ok(())
        }
    };
}
