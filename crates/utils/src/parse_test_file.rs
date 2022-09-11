#[cfg(test)]
#[path = "parse_test_file_test.rs"]
mod test;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

use crate::ordered_hash_map::OrderedHashMap;

struct State {
    tests: OrderedHashMap<String, OrderedHashMap<String, String>>,
    current_test: OrderedHashMap<String, String>,
    current_test_name: Option<String>,
    current_tag: Option<String>,
    current_content: Option<String>,
    expect_underline: usize,
}

fn parse_test_file(
    filename: &Path,
) -> io::Result<OrderedHashMap<String, OrderedHashMap<String, String>>> {
    let file = File::open(filename)?;
    let lines = io::BufReader::new(file).lines();
    let mut state = State {
        tests: OrderedHashMap::default(),
        current_test: OrderedHashMap::default(),
        current_test_name: None,
        current_tag: None,
        current_content: None,
        expect_underline: 0,
    };

    for line in lines {
        let line = line.unwrap();
        if state.expect_underline != 0 {
            // An underline must follow the test name (no empty line allowed).
            if line != "-".repeat(state.expect_underline) {
                panic!();
            } else {
                state.expect_underline = 0;
            }
            continue;
        }
        if line.is_empty() {
            continue;
        } else if line.starts_with("===") {
            assert_eq!(line, "================");
            // Separate tests.
            state.add_to_test();
            state.new_test()
        } else if state.current_test_name == None {
            // First nonempty line must be test name.
            state.expect_underline = line.len();
            state.current_test_name = Some(line.clone());
        } else if line.starts_with(">>> ") {
            // Close previous tag.
            state.add_to_test();
            // Open new tag.
            state.current_tag = Some(line.strip_prefix(">>> ").unwrap().to_string());
            state.current_content = Some("".to_string());
        } else {
            state.current_content = Some(state.current_content.unwrap() + &line + "\n");
        }
    }
    state.add_to_test();
    state.new_test();
    Ok(state.tests)
}

impl State {
    fn add_to_test(&mut self) {
        if self.current_tag != None {
            self.current_test.insert(
                self.current_tag.as_ref().unwrap().to_string(),
                self.current_content.as_ref().unwrap().to_string(),
            );
        }
    }

    fn new_test(&mut self) {
        let current_test = std::mem::take(&mut self.current_test);
        self.tests.insert(self.current_test_name.as_ref().unwrap().into(), current_test);
        self.current_test_name = None;
        self.current_tag = None;
        self.current_content = None;
    }
}
