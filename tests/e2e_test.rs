use std::ops::DerefMut;
use std::sync::{LazyLock, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_filesystem::flag::{Flag, FlagsGroup};
use cairo_lang_filesystem::ids::FlagLongId;
use cairo_lang_lowering::db::lowering_group_input;
use cairo_lang_lowering::optimizations::config::{OptimizationConfig, Optimizations};
use cairo_lang_runner::{Arg, RunResultValue, SierraCasmRunner};
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_sierra::extensions::gas::{CostTokenMap, CostTokenType};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::{Function, Program};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_to_casm::compiler;
use cairo_lang_sierra_to_casm::metadata::{MetadataComputationConfig, calc_metadata};
use cairo_lang_sierra_type_size::ProgramRegistryInfo;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::test_lock;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;
use regex::Regex;
use salsa::Setter;
use starknet_types_core::felt::Felt as Felt252;

/// Salsa databases configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
static SHARED_DB_WITH_GAS_NO_OPTS: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    let mut db = RootDatabase::builder().detect_corelib().build().unwrap();
    lowering_group_input(&db).set_optimizations(&mut db).to(Some(Optimizations::Enabled(
        OptimizationConfig::default().with_skip_const_folding(true),
    )));
    Mutex::new(db)
});
static SHARED_DB_NO_GAS_NO_OPTS: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    let mut db = RootDatabase::builder().detect_corelib().skip_auto_withdraw_gas().build().unwrap();
    lowering_group_input(&db).set_optimizations(&mut db).to(Some(Optimizations::Enabled(
        OptimizationConfig::default().with_skip_const_folding(true),
    )));
    Mutex::new(db)
});
static SHARED_DB_WITH_OPTS: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    let mut db = RootDatabase::builder().detect_corelib().skip_auto_withdraw_gas().build().unwrap();
    lowering_group_input(&db)
        .set_optimizations(&mut db)
        .to(Some(Optimizations::Enabled(Default::default())));
    Mutex::new(db)
});
static SHARED_DB_FUTURE_SIERRA: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    let mut db = RootDatabase::builder().detect_corelib().build().unwrap();
    lowering_group_input(&db).set_optimizations(&mut db).to(Some(Optimizations::Enabled(
        OptimizationConfig::default().with_skip_const_folding(true),
    )));
    db.set_flag(FlagLongId(Flag::FUTURE_SIERRA.into()), Some(Flag::FutureSierra(true)));
    Mutex::new(db)
});

cairo_lang_test_utils::test_file_test_with_runner!(
    general_e2e,
    "e2e_test_data",
    {
        cmp: "cmp",
    },
    SmallE2ETestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    libfunc_e2e,
    "e2e_test_data/libfuncs",
    {
        array: "array",
        bitwise: "bitwise",
        blake: "blake",
        bool: "bool",
        bounded_int: "bounded_int",
        box_: "box",
        builtin_costs: "builtin_costs",
        casts: "casts",
        circuit: "circuit",
        coupon: "coupon",
        ec: "ec",
        enum_: "enum",
        enum_snapshot: "enum_snapshot",
        felt252_dict: "felt252_dict",
        felt252_downcast: "felt252_downcast",
        felt252: "felt252",
        fixed_size_array: "fixed_size_array",
        gas_reserve: "gas_reserve",
        i128: "i128",
        i16: "i16",
        i32: "i32",
        i64: "i64",
        i8: "i8",
        nullable: "nullable",
        poseidon: "poseidon",
        qm31: "qm31",
        range: "range",
        snapshot: "snapshot",
        u128: "u128",
        u16: "u16",
        bytes31: "bytes31",
        u256: "u256",
        u32: "u32",
        u512: "u512",
        u64: "u64",
        u8: "u8",
        casm_run_sanity: "casm_run_sanity",
    },
    SmallE2ETestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    libfunc_e2e_skip_add_gas,
    "e2e_test_data/libfuncs",
    {
        gas: "gas",
    },
    SmallE2ETestRunnerSkipAddGas
);

cairo_lang_test_utils::test_file_test_with_runner!(
    libfunc_e2e_withopts,
    "e2e_test_data/libfuncs",
    {
        consts: "consts",
    },
    WithOptsE2ETestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    starknet_libfunc_e2e,
    "e2e_test_data/libfuncs/starknet",
    {
        class_hash: "class_hash",
        contract_address: "contract_address",
        secp256k1: "secp256k1",
        secp256r1: "secp256r1",
        storage_address: "storage_address",
        syscalls: "syscalls",
    },
    SmallE2ETestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    metadata_e2e,
    "e2e_test_data",
    {
        metadata_computation: "metadata_computation",
    },
    SmallE2ETestRunnerMetadataComputation
);

#[derive(Default)]
struct SmallE2ETestRunner;
impl TestFileRunner for SmallE2ETestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let future_sierra = args.get("future_sierra").is_some_and(|v| v == "true");
        let include_trace = args.get("trace").is_some_and(|v| v == "true");
        let skip_gas = args.get("skip_gas").is_some_and(|v| v == "true");

        let test_data = if let Some(s) = args.get("test_data") {
            match TestData::parse(s) {
                Ok(td) => Some(td),
                Err(e) => {
                    return TestRunnerResult { outputs: OrderedHashMap::default(), error: Some(e) };
                }
            }
        } else {
            None
        };

        // Trace requires test_data to be specified, since we need a function to run.
        if include_trace && test_data.is_none() {
            return TestRunnerResult {
                outputs: OrderedHashMap::default(),
                error: Some(
                    "trace: true requires test_data to be specified. Use format: \
                     SmallE2ETestRunner(trace: true, test_data: function_name(input)=output)"
                        .to_string(),
                ),
            };
        }

        run_e2e_test(
            inputs,
            E2eTestParams {
                future_sierra,
                include_trace,
                test_data,
                add_withdraw_gas: !skip_gas,
                ..E2eTestParams::default()
            },
        )
    }
}

#[derive(Default)]
struct WithOptsE2ETestRunner;
impl TestFileRunner for WithOptsE2ETestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        run_e2e_test(
            inputs,
            E2eTestParams { skip_optimization_passes: false, ..Default::default() },
        )
    }
}

#[derive(Default)]
struct SmallE2ETestRunnerSkipAddGas;
impl TestFileRunner for SmallE2ETestRunnerSkipAddGas {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        run_e2e_test(inputs, E2eTestParams { add_withdraw_gas: false, ..E2eTestParams::default() })
    }
}

#[derive(Default)]
struct SmallE2ETestRunnerMetadataComputation;
impl TestFileRunner for SmallE2ETestRunnerMetadataComputation {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        run_e2e_test(
            inputs,
            E2eTestParams {
                add_withdraw_gas: false,
                metadata_computation: true,
                skip_optimization_passes: true,
                ..Default::default()
            },
        )
    }
}

/// Represents test data for function execution: function_name(input)=output
#[derive(Clone, Debug)]
struct TestData {
    function_name: String,
    input: Felt252,
    expected_output: Felt252,
}

impl TestData {
    /// Parses test_data format: "function_name(input)=output"
    /// Allows whitespace around all components.
    fn parse(s: &str) -> Result<Self, String> {
        // Regex pattern breakdown:
        // ^\s*          - Start of string, optional leading whitespace
        // (\w+)         - Capture group 1: function name (alphanumeric + underscore)
        // \s*\(\s*      - Optional whitespace, opening parenthesis, optional whitespace
        // (-?\d+)       - Capture group 2: input number (optional minus sign, then digits)
        // \s*\)\s*      - Optional whitespace, closing parenthesis, optional whitespace
        // =             - Equals sign
        // \s*           - Optional whitespace after equals
        // (-?\d+)       - Capture group 3: expected output number (optional minus, digits)
        // \s*$          - Optional trailing whitespace, end of string
        let re = Regex::new(r"^\s*(\w+)\s*\(\s*(-?\d+)\s*\)\s*=\s*(-?\d+)\s*$")
            .expect("Invalid regex pattern");

        let captures = re.captures(s).ok_or_else(|| {
            format!(
                "Invalid test_data format: '{}'. Expected format: 'function_name(input)=output' \
                 where function_name contains only alphanumeric/underscore characters and both \
                 input and output are valid Felt252 decimal values",
                s
            )
        })?;

        let function_name = captures.get(1).unwrap().as_str().to_string();
        let input_str = captures.get(2).unwrap().as_str();
        let output_str = captures.get(3).unwrap().as_str();

        let input = Felt252::from_dec_str(input_str)
            .map_err(|e| format!("Failed to parse input as a felt '{}': {}", input_str, e))?;
        let expected_output = Felt252::from_dec_str(output_str).map_err(|e| {
            format!("Failed to parse expected output as a felt '{}': {}", output_str, e)
        })?;

        Ok(TestData { function_name, input, expected_output })
    }
}

/// Represents the parameters of `run_e2e_test`.
struct E2eTestParams {
    /// Argument for `run_e2e_test` that controls whether to set the `add_withdraw_gas` flag
    /// that automatically adds `withdraw_gas` calls.
    add_withdraw_gas: bool,

    /// Argument for `run_e2e_test` that controls whether to add metadata computation information
    /// to the test outputs.
    metadata_computation: bool,

    /// Argument for `run_e2e_test` that controls whether to skip optimization passes.
    skip_optimization_passes: bool,

    /// Argument for `run_e2e_test` that controls whether to enable the `future_sierra` flag.
    future_sierra: bool,

    /// Argument for `run_e2e_test` that controls whether to include CASM trace and memory in the
    /// output.
    include_trace: bool,

    /// Test data for function execution validation.
    test_data: Option<TestData>,
}

/// Implements default for `E2eTestParams`.
impl Default for E2eTestParams {
    fn default() -> Self {
        Self {
            add_withdraw_gas: true,
            metadata_computation: false,
            skip_optimization_passes: true,
            future_sierra: false,
            include_trace: false,
            test_data: None,
        }
    }
}

/// Runs the e2e test.
fn run_e2e_test(
    inputs: &OrderedHashMap<String, String>,
    params: E2eTestParams,
) -> TestRunnerResult {
    let mut locked_db = test_lock(if params.future_sierra {
        &SHARED_DB_FUTURE_SIERRA
    } else if !params.skip_optimization_passes {
        &SHARED_DB_WITH_OPTS
    } else if params.add_withdraw_gas {
        &SHARED_DB_WITH_GAS_NO_OPTS
    } else {
        &SHARED_DB_NO_GAS_NO_OPTS
    });
    // Parse code and create semantic model.
    let db_ref = locked_db.deref_mut();
    let test_module = setup_test_module(db_ref, inputs["cairo_code"].as_str()).unwrap();
    let crate_input = test_module.crate_id.long(db_ref).clone().into_crate_input(db_ref);
    let db = locked_db.snapshot();
    DiagnosticsReporter::stderr()
        .with_crates(std::slice::from_ref(&crate_input))
        .ensure(&db)
        .unwrap();

    // Compile to Sierra.
    let SierraProgramWithDebug { program: sierra_program, .. } = db
        .get_sierra_program(vec![crate_input.into_crate_long_id(&db).intern(&db)])
        .expect("`get_sierra_program` failed. run with RUST_LOG=warn (or less) to see diagnostics");
    let sierra_program = replace_sierra_ids_in_program(&db, sierra_program);
    let program_info = ProgramRegistryInfo::new(&sierra_program).unwrap();
    let sierra_program_str = sierra_program.to_string();

    // Handle the `enforced_costs` argument.
    let enforced_costs: OrderedHashMap<FunctionId, CostTokenMap<i32>> =
        if let Some(enforced_costs_str) = inputs.get("enforced_costs") {
            parse_enforced_costs(&sierra_program, enforced_costs_str)
        } else {
            Default::default()
        };

    // Compute the metadata.
    let mut metadata_config = MetadataComputationConfig {
        function_set_costs: enforced_costs,
        compute_runtime_costs: params.metadata_computation,
        ..Default::default()
    };
    let metadata_with_linear =
        calc_metadata(&sierra_program, &program_info, metadata_config.clone()).unwrap();

    let config =
        compiler::SierraToCasmConfig { gas_usage_check: true, max_bytecode_size: usize::MAX };
    // Compile to casm.
    let casm = compiler::compile(&sierra_program, &program_info, &metadata_with_linear, config)
        .map(|x| x.to_string())
        .unwrap_or_else(|e| format!("failing with: `{e}`."));

    let mut res: OrderedHashMap<String, String> =
        OrderedHashMap::from([("casm".into(), casm), ("sierra_code".into(), sierra_program_str)]);
    if params.metadata_computation {
        metadata_config.linear_gas_solver = false;
        metadata_config.linear_ap_change_solver = false;
        metadata_config.skip_non_linear_solver_comparisons = true;
        let metadata_with_lp =
            calc_metadata(&sierra_program, &program_info, metadata_config).unwrap();
        res.insert("gas_solution_lp".into(), format!("{}", metadata_with_lp.gas_info));
        res.insert("gas_solution_linear".into(), format!("{}", metadata_with_linear.gas_info));
        res.insert("ap_solution_lp".into(), format!("{}", metadata_with_lp.ap_change_info));
        res.insert("ap_solution_linear".into(), format!("{}", metadata_with_linear.ap_change_info));

        // Compile again, this time with the no-solver metadata.
        compiler::compile(&sierra_program, &program_info, &metadata_with_lp, config).unwrap();
    } else {
        let function_costs_str = metadata_with_linear
            .gas_info
            .function_costs
            .iter()
            .map(|(func_id, cost)| format!("{func_id}: {cost:?}"))
            .join("\n");
        res.insert("function_costs".into(), function_costs_str);
    }

    // Handle test_data if specified.
    // This runs AFTER sierra/casm generation, so compilation outputs are always verified first.
    if let Some(test_data) = &params.test_data {
        let runner = SierraCasmRunner::new(
            sierra_program.clone(),
            if params.add_withdraw_gas { Some(Default::default()) } else { None },
            Default::default(),
            None,
        )
        .expect("Failed setting up runner for test_data.");

        let func = match runner.find_function(&test_data.function_name) {
            Ok(f) => f,
            Err(e) => {
                // Return outputs so sierra/casm can still be compared, but include the error.
                return TestRunnerResult {
                    outputs: res,
                    error: Some(format!(
                        "Function '{}' not found in the program: {}",
                        test_data.function_name, e
                    )),
                };
            }
        };

        let result = match runner.run_function_with_starknet_context_with_trace(
            func,
            vec![Arg::Value(test_data.input)],
            if params.add_withdraw_gas { Some(usize::MAX) } else { None },
            Default::default(),
            params.include_trace,
        ) {
            Ok(r) => r,
            Err(e) => {
                // Return outputs so sierra/casm can still be compared, but include the error.
                return TestRunnerResult {
                    outputs: res,
                    error: Some(format!(
                        "Failed running function '{}': {}",
                        test_data.function_name, e
                    )),
                };
            }
        };

        // Validate the output and record the result.
        let validation_result = match &result.value {
            RunResultValue::Success(values) => {
                if values.is_empty() {
                    Err(format!(
                        "Function '{}' returned no values, expected {}",
                        test_data.function_name, test_data.expected_output
                    ))
                } else {
                    let actual_output = values[0];
                    if actual_output != test_data.expected_output {
                        Err(format!(
                            "Function '{}' output mismatch: expected {}, got {}",
                            test_data.function_name, test_data.expected_output, actual_output
                        ))
                    } else {
                        Ok(())
                    }
                }
            }
            RunResultValue::Panic(panic_data) => Err(format!(
                "Function '{}' panicked with data: {:?}",
                test_data.function_name, panic_data
            )),
        };

        // Include trace and memory if trace flag is enabled (regardless of validation result).
        if params.include_trace {
            // Format trace as a table with headers.
            let trace = match result.trace {
                Some(trace) => trace,
                None => {
                    return TestRunnerResult {
                        outputs: res,
                        error: Some(
                            "trace: true was specified but no trace was returned. Ensure the \
                             function was executed correctly."
                                .to_string(),
                        ),
                    };
                }
            };
            let mut trace_lines = vec!["| PC | AP | FP |".to_string()];
            for entry in &trace {
                trace_lines.push(format!("| {} | {} | {} |", entry.pc, entry.ap, entry.fp));
            }
            res.insert("casm_trace".into(), trace_lines.join("\n"));

            // Include memory state.
            let memory_str = result
                .memory
                .iter()
                .enumerate()
                .filter_map(|(addr, value)| value.as_ref().map(|v| format!("[{}] = {}", addr, v)))
                .join("\n");
            res.insert("memory".into(), memory_str);
        }

        // If validation failed, return outputs with error so sierra/casm comparison still happens.
        if let Err(e) = validation_result {
            return TestRunnerResult { outputs: res, error: Some(e) };
        }
    }

    TestRunnerResult::success(res)
}

/// Parses the `enforced_costs` test argument. It should consist of lines of the form
///   <function_name> <cost>
/// Where `function_name` is the fully-qualified name of the function, and `cost` is the cost to
/// enforce for that function.
fn parse_enforced_costs(
    sierra_program: &Program,
    enforced_costs_str: &str,
) -> OrderedHashMap<FunctionId, CostTokenMap<i32>> {
    // Create a map from function name to function id.
    let function_name_to_id: UnorderedHashMap<&str, _> = sierra_program
        .funcs
        .iter()
        .map(|Function { id, .. }| (id.debug_name.as_ref().unwrap().as_str(), id))
        .collect();

    enforced_costs_str
        .split('\n')
        .map(|line| {
            // line is the name of the function and the enforced cost, separated by a space.
            let [name, cost_str] = line.split(' ').collect_vec()[..] else {
                panic!(
                    "Invalid enforced cost line. Expected a line of the form '<function name> \
                     <cost>'."
                );
            };

            // Get the FunctionId from the name by searching program.funcs.
            let function_id = *function_name_to_id
                .get(name)
                .unwrap_or_else(|| panic!("Function {name} was not found."));
            let cost = cost_str
                .parse::<i32>()
                .unwrap_or_else(|_| panic!("Expected a number as the enforced cost."));
            (function_id.clone(), [(CostTokenType::Const, cost)].into_iter().collect())
        })
        .collect::<OrderedHashMap<_, _>>()
}
