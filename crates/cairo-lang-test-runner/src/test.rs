use itertools::Itertools;

use crate::{TestCompilation, TestCompiler};

#[test]
fn test_compiled_serialization() {
    use std::path::PathBuf;
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");

    let compiler = TestCompiler::try_new(&path, true).unwrap();
    let compiled = compiler.build().unwrap();
    let serialized = serde_json::to_string_pretty(&compiled).unwrap();
    let deserialized: TestCompilation = serde_json::from_str(&serialized).unwrap();

    assert_eq!(compiled.sierra_program, deserialized.sierra_program);
    assert_eq!(compiled.function_set_costs, deserialized.function_set_costs);
    assert_eq!(compiled.named_tests, deserialized.named_tests);
    assert_eq!(
        compiled.contracts_info.values().collect_vec(),
        deserialized.contracts_info.values().collect_vec()
    );
}
