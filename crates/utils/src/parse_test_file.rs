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
}

fn parse_test_file(
    filename: &Path,
) -> io::Result<OrderedHashMap<String, OrderedHashMap<String, String>>> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines();
    let mut builder = TestBuilder::default();
    while let Some(Ok(line)) = lines.next() {
        if line.is_empty() {
            // Skip empty lines.
        } else if let Some(line) = line.strip_prefix("//! > ") {
            if builder.current_test_name == None {
                builder.set_test_name(line.into());
            } else if line.starts_with("===") {
                assert_eq!(line, "=".repeat(73));
                // Separate tests.
                builder.new_test()
            } else {
                builder.close_tag();
                builder.open_tag(line.into());
            }
        } else {
            builder.add_content_line(line);
        }
    }
    Ok(builder.finalize())
}

impl TestBuilder {
    fn close_tag(&mut self) {
        if let Some(tag) = &self.current_tag {
            let content = match &self.current_content {
                Some(content) => content,
                None => "",
            };
            self.current_test.insert(tag.into(), content.into());
            self.current_content = None;
            self.current_tag = None;
        }
    }

    fn open_tag(&mut self, line: String) {
        self.current_tag = Some(line);
    }

    fn set_test_name(&mut self, line: String) {
        self.current_test_name = Some(line);
    }

    fn add_content_line(&mut self, line: String) {
        if let Some(content) = &self.current_content {
            // Append a line to content.
            self.current_content = Some(content.to_owned() + "\n" + &line);
        } else {
            // Add first line to content.
            self.current_content = Some(line);
        }
    }

    fn new_test(&mut self) {
        self.close_tag();
        let current_test = std::mem::take(&mut self.current_test);
        self.tests.insert(
            self.current_test_name.as_ref().expect("No name found for test.").into(),
            current_test,
        );
        self.current_test_name = None;
        self.current_tag = None;
        self.current_content = None;
    }

    fn finalize(&mut self) -> OrderedHashMap<String, OrderedHashMap<String, String>> {
        self.new_test();
        std::mem::take(&mut self.tests)
    }
}
