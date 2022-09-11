#[cfg(test)]
#[path = "parse_test_file_test.rs"]
mod test;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

use crate::ordered_hash_map::OrderedHashMap;

#[derive(Default)]
struct TestBuilder {
    tests: OrderedHashMap<String, OrderedHashMap<String, String>>,
    current_test: OrderedHashMap<String, String>,
    current_test_name: Option<String>,
    current_tag: Option<String>,
    current_content: Option<String>,
    expect_underline: Option<usize>,
}

fn parse_test_file(
    filename: &Path,
) -> io::Result<OrderedHashMap<String, OrderedHashMap<String, String>>> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines();
    let mut builder = TestBuilder::default();
    while let Some(Ok(line)) = lines.next() {
        if let Some(len) = builder.expect_underline {
            // An underline must follow the test name (no empty line allowed).
            assert_eq!(line, "-".repeat(len));
            builder.expect_underline = None;
        } else if line.is_empty() {
        } else if line.starts_with("===") {
            assert_eq!(line, "================");
            // Separate tests.
            builder.new_test()
        } else if builder.current_test_name == None {
            // First nonempty line must be test name.
            builder.set_test_name(line);
        } else if let Some(line) = line.strip_prefix(">>> ") {
            builder.close_tag();
            builder.open_tag(line.into());
        } else if let Some(content) = builder.current_content {
            // Append a line to content.
            builder.current_content = Some(content + "\n" + &line);
        } else {
            // Add first line to content.
            builder.current_content = Some(line);
        }
    }
    // Add the last tag and test.
    Ok(builder.finalize())
}

impl TestBuilder {
    fn close_tag(&mut self) {
        if let Some(tag) = &self.current_tag {
            self.current_test
                .insert(tag.into(), self.current_content.as_ref().unwrap().to_string());
            self.current_content = None;
            self.current_tag = None;
        }
    }

    fn open_tag(&mut self, line: String) {
        self.current_tag = Some(line);
    }

    fn set_test_name(&mut self, line: String) {
        self.expect_underline = Some(line.len());
        self.current_test_name = Some(line);
    }

    fn new_test(&mut self) {
        self.close_tag();
        let current_test = std::mem::take(&mut self.current_test);
        self.tests.insert(self.current_test_name.as_ref().unwrap().into(), current_test);
        self.current_test_name = None;
        self.current_tag = None;
        self.current_content = None;
    }

    fn finalize(&mut self) -> OrderedHashMap<String, OrderedHashMap<String, String>> {
        self.new_test();
        std::mem::take(&mut self.tests)
    }
}
