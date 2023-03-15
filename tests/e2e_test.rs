use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_to_casm::test_utils::build_metadata;
use cairo_lang_test_utils::parse_test_file::TestFileRunner;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

cairo_lang_test_utils::test_file_test_with_runner!(
    libfunc_e2e,
    "e2e_test_data/libfuncs",
    {
        array: "array",
        snapshot: "snapshot",
        bitwise: "bitwise",
        box_: "box",
        builtin_costs: "builtin_costs",
        casts: "casts",
        dict_felt252_to: "dict_felt252_to",
        ec: "ec",
        withdraw_gas_all: "withdraw_gas_all",
        nullable: "nullable",
        u8: "u8",
        u16: "u16",
        u32: "u32",
        u64: "u64",
        u128: "u128",
        bool: "bool",
        enum_: "enum",
        enum_snapshot: "enum_snapshot",
    },
    SmallE2ETestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    starknet_libfunc_e2e,
    "e2e_test_data/libfuncs/starknet",
    {
        class_hash: "class_hash",
        contract_address: "contract_address",
        storage_address: "storage_address",
        syscalls: "syscalls",
    },
    SmallE2ETestRunner
);

struct SmallE2ETestRunner {
    db: RootDatabase,
}
impl Default for SmallE2ETestRunner {
    fn default() -> Self {
        Self { db: RootDatabase::builder().detect_corelib().build().unwrap() }
    }
}
impl TestFileRunner for SmallE2ETestRunner {
    fn run(&mut self, inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
        // Parse code and create semantic model.
        let test_module = setup_test_module(&mut self.db, inputs["cairo"].as_str()).unwrap();
        DiagnosticsReporter::stderr().ensure(&mut self.db).unwrap();

        // Compile to Sierra.
        let sierra_program = self.db.get_sierra_program(vec![test_module.crate_id]).unwrap();
        let sierra_program = replace_sierra_ids_in_program(&self.db, &sierra_program);
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
}
