use cairo_compiler::db::RootDatabase;
use cairo_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_semantic::test_utils::setup_test_module;
use cairo_sierra_generator::db::SierraGenGroup;
use cairo_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_sierra_to_casm::test_utils::build_metadata;
use cairo_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

cairo_test_utils::test_file_test!(
    libfunc_e2e,
    "e2e_test_data/libfuncs",
    {
        array: "array",
        box_: "box",
        nullable: "nullable",
        u128: "u128",
    },
    run_small_e2e_test
);

fn run_small_e2e_test(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &mut RootDatabase::default();
    // Parse code and create semantic model.
    let test_module = setup_test_module(db, inputs["cairo"].as_str()).unwrap();
    assert!(!check_and_eprint_diagnostics(db));

    // Compile to Sierra.
    let sierra_program = db.get_sierra_program(vec![test_module.crate_id]).unwrap();
    let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);
    let sierra_program_str = sierra_program.to_string();

    // Compute the metadata.
    let metadata = build_metadata(&sierra_program, true);
    let function_costs_str = metadata
        .gas_info
        .function_costs
        .iter()
        .map(|(func_id, cost)| format!("{func_id}: {cost:?}"))
        .join("\n");

    // Compile to casm.
    let casm = cairo_sierra_to_casm::compiler::compile(&sierra_program, &metadata, true)
        .unwrap()
        .to_string();

    OrderedHashMap::from([
        ("casm".into(), casm),
        ("function_costs".into(), function_costs_str),
        ("sierra_code".into(), sierra_program_str),
    ])
}
