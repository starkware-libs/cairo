use std::path::Path;

use crate::parse_test_file;

#[test]
fn test_parse_test_file() -> Result<(), std::io::Error> {
    let tests = parse_test_file::parse_test_file(Path::new("test_data/test_example"))?;
    let test1 = &tests["Test Example"];
    assert_eq!(test1["Expression"], "foo");
    assert_eq!(test1["Expected"], "bar");

    let test2 = &tests["Another Test Example"];
    assert_eq!(test2["Expression"], "foo\n//! bar");
    assert_eq!(test2["Expected"], "baz");
    Ok(())
}
