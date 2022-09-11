use std::path::Path;

use crate::parse_test_file;

#[test]
fn test_parse_test_file() {
    let tests = parse_test_file::parse_test_file(Path::new("test_data/test_example"))
        .expect("Error reading file.");
    let test1 = tests.get(&"Test Example".to_string()).expect("Get failed.");
    assert_eq!(test1.get(&"Expression".to_string()).expect("Get failed."), &"foo".to_string());
    assert_eq!(test1.get(&"Expected".to_string()).expect("Get failed."), &"bar".to_string());

    let test2 = tests.get(&"Another Test Example".to_string()).expect("Get failed.");
    assert_eq!(
        test2.get(&"Expression".to_string()).expect("Get failed."),
        &"foo\n//! bar".to_string()
    );
    assert_eq!(test2.get(&"Expected".to_string()).expect("Get failed."), &"baz".to_string());
}
