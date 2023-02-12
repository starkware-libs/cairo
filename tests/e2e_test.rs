use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_to_casm::test_utils::build_metadata;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

cairo_lang_test_utils::test_file_test!(
    libfunc_e2e,
    "e2e_test_data/libfuncs",
    {
        array: "array",
        bitwise: "bitwise",
        box_: "box",
        builtin_costs: "builtin_costs",
        casts: "casts",
        dict_felt_to: "dict_felt_to",
        ec: "ec",
        get_gas_all: "get_gas_all",
        nullable: "nullable",
        u8: "u8",
        u16: "u16",
        u32: "u32",
        u64: "u64",
        u128: "u128",
        bool: "bool",
    },
    run_small_e2e_test
);

cairo_lang_test_utils::test_file_test!(
    starknet_libfunc_e2e,
    "e2e_test_data/libfuncs/starknet",
    {
        storage: "storage",
        interoperability: "interoperability",
        emit_event: "emit_event",
        getters: "getters",
    },
    run_small_e2e_test
);

fn run_small_e2e_test(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let mut db = RootDatabase::builder().detect_corelib().build().unwrap();
    // Parse code and create semantic model.
    let test_module = setup_test_module(&mut db, inputs["cairo"].as_str()).unwrap();
    DiagnosticsReporter::stderr().ensure(&mut db).unwrap();

    // Compile to Sierra.
    let sierra_program = db.get_sierra_program(vec![test_module.crate_id]).unwrap();
    let sierra_program = replace_sierra_ids_in_program(&db, &sierra_program);
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
    let casm = cairo_lang_sierra_to_casm::compiler::compile(&sierra_program, &metadata, true)
        .unwrap()
        .to_string();

    OrderedHashMap::from([
        ("casm".into(), casm),
        ("function_costs".into(), function_costs_str),
        ("sierra_code".into(), sierra_program_str),
    ])
}
