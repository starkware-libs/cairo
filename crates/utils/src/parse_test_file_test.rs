use std::path::Path;

use test_log::test;

use crate::parse_test_file;

#[test]
fn test_parse_test_file() -> Result<(), std::io::Error> {
    let tests = parse_test_file::parse_test_file(Path::new("test_data/test_example"))?;
    let test1 = &tests["Test Example"];
    assert_eq!(test1.attributes["Expression"], "foo");
    assert_eq!(test1.attributes["Expected"], "bar");

    let test2 = &tests["Another Test Example"];
    assert_eq!(test2.attributes["Expression"], "foo\n//! bar");
    assert_eq!(test2.attributes["Expected"], "baz");
    assert_eq!(test2.attributes["Empty"], "");
    Ok(())
}

#[test]
fn test_dump_to_test_file() -> Result<(), std::io::Error> {
    let tests = parse_test_file::parse_test_file(Path::new("test_data/test_example"))?;
    parse_test_file::dump_to_test_file(tests, "test_data/test_example_expected")?;
    assert_eq!(
        std::fs::read_to_string("test_data/test_example")?,
        std::fs::read_to_string("test_data/test_example_expected")?
    );
    std::fs::remove_file("test_data/test_example_expected")
}
