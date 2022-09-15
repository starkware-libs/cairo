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

#[derive(Default)]
struct TestBuilder {
    tests: OrderedHashMap<String, OrderedHashMap<String, String>>,
    current_test: OrderedHashMap<String, String>,
    current_test_name: Option<String>,
    current_tag: Option<Tag>,
}

pub fn parse_test_file(
    filename: &Path,
) -> io::Result<OrderedHashMap<String, OrderedHashMap<String, String>>> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines();
    let mut builder = TestBuilder::default();
    while let Some(Ok(line)) = lines.next() {
        if let Some(line) = line.strip_prefix(TAG_PREFIX) {
            if builder.current_test_name == None {
                builder.set_test_name(line.into());
            } else if line.starts_with("===") {
                // Separate tests.
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
            self.current_test.insert(std::mem::take(&mut tag.name), tag.content.trim().to_string());
            self.current_tag = None;
        }
    }

    fn open_tag(&mut self, line: String) {
        self.close_open_tag();
        self.current_tag = Some(Tag { name: line, content: "".into() });
    }

    fn set_test_name(&mut self, line: String) {
        self.current_test_name = Some(line);
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
        let current_test = std::mem::take(&mut self.current_test);
        self.tests.insert(
            self.current_test_name.as_ref().expect("No name found for test.").into(),
            current_test,
        );
        self.current_test_name = None;
        self.current_tag = None;
    }

    fn finalize(&mut self) -> OrderedHashMap<String, OrderedHashMap<String, String>> {
        self.new_test();
        std::mem::take(&mut self.tests)
    }
}
