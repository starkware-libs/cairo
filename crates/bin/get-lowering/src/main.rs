//! Internal debug utility for printing lowering phases.

use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::add_withdraw_gas::add_withdraw_gas;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::destructs::add_destructs;
use cairo_lang_lowering::fmt::LoweredFormatter;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_lowering::optimizations::scrub_units::scrub_units;
use cairo_lang_lowering::panic::lower_panics;
use cairo_lang_lowering::FlatLowered;
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionWithBodyId, ImplGenericFunctionWithBodyId,
};
use cairo_lang_semantic::ConcreteImplLongId;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use clap::Parser;
use convert_case::Casing;
use itertools::Itertools;

/// Tests that `PhasesFormatter` is consistent with the lowering phases.
#[test]
fn test_lowering_consistency() {
    let db_val = RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();

    let db: &dyn LoweringGroup = &db_val;

    let function_id = get_func_id_by_name(
        db,
        &[db.core_crate()],
        "core::poseidon::_poseidon_hash_span_inner".to_string(),
    )
    .unwrap();
    format!("{:?}", PhasesFormatter { db, function_id });
}

/// Prints the lowering of a concrete function:
///
/// Usage example:
///     cargo run --bin get-lowering corelib/ core::poseidon::poseidon_hash_span
///
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The crate to compile.
    path: PathBuf,
    /// The concrete functions fully qualified path, if None print all functions.
    function_path: Option<String>,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,

    /// whenever to print all lowering stages or only the final lowering.
    #[arg(short, long)]
    all: bool,

    /// The output file name (default: stdout).
    output: Option<String>,
}

/// Helper class for formatting the lowering phases of a concrete function.
struct PhasesFormatter<'a> {
    db: &'a dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
}

impl fmt::Debug for PhasesFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let db = self.db;
        let function_id = self.function_id;

        let mut curr_state =
            (*db.priv_concrete_function_with_body_lowered_flat(function_id).unwrap()).clone();

        let mut phase_index = 0;
        let mut add_stage_state = |name: &str, lowered: &FlatLowered| {
            writeln!(f, "{phase_index}. {name}:").unwrap();
            let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
            write!(f, "{:?}", lowered.debug(&lowered_formatter)).unwrap();

            phase_index += 1;
        };
        add_stage_state("before_all", &curr_state);

        let mut apply_stage = |name: &'static str, stage: &dyn Fn(&mut FlatLowered)| {
            (*stage)(&mut curr_state);
            add_stage_state(name, &curr_state);
        };
        apply_stage("after_add_withdraw_gas", &|lowered| {
            add_withdraw_gas(db, function_id, lowered).unwrap()
        });
        apply_stage("after_lower_panics", &|lowered| {
            *lowered = lower_panics(db, function_id, lowered).unwrap();
        });
        apply_stage("after_add_destructs", &|lowered| add_destructs(db, function_id, lowered));
        apply_stage("scrub_units", &|lowered| scrub_units(db, lowered));

        for strategy in [db.baseline_optimization_strategy(), db.final_optimization_strategy()] {
            for phase in db.lookup_intern_strategy(strategy).0 {
                let name = format!("{phase:?}").to_case(convert_case::Case::Snake);
                phase.apply(db, function_id, &mut curr_state).unwrap();
                add_stage_state(&name, &curr_state);
            }
        }

        Ok(())
    }
}

// Returns a dictionary mapping function names to their ids for all the functions in the given
// crate.
fn get_all_funcs(
    db: &dyn LoweringGroup,
    crate_ids: &[CrateId],
) -> anyhow::Result<OrderedHashMap<String, GenericFunctionWithBodyId>> {
    let mut res: OrderedHashMap<String, GenericFunctionWithBodyId> = Default::default();
    for crate_id in crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            let free_funcs = db.module_free_functions_ids(*module_id).unwrap();
            for func_id in free_funcs.iter() {
                res.insert(
                    func_id.full_path(db.upcast()),
                    GenericFunctionWithBodyId::Free(*func_id),
                );
            }

            let impl_ids = db.module_impls_ids(*module_id).unwrap();
            for impl_def_id in impl_ids.iter() {
                let impl_funcs = db.impl_functions(*impl_def_id).unwrap();
                for impl_func in impl_funcs.values() {
                    res.insert(
                        impl_func.full_path(db.upcast()),
                        GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                            concrete_impl_id: db.intern_concrete_impl(ConcreteImplLongId {
                                impl_def_id: *impl_def_id,
                                generic_args: vec![],
                            }),
                            function: *impl_func,
                        }),
                    );
                }
            }
        }
    }

    Ok(res)
}

/// Given a function name and list of crates, returns the Concrete id of the function.
fn get_func_id_by_name(
    db: &dyn LoweringGroup,
    crate_ids: &[CrateId],
    function_path: String,
) -> anyhow::Result<ConcreteFunctionWithBodyId> {
    let all_funcs = get_all_funcs(db, crate_ids)?;
    let Some(func_id) = all_funcs.get(&function_path) else {
        anyhow::bail!("Function {} not found in the project.", function_path.as_str())
    };

    Ok(ConcreteFunctionWithBodyId::from_semantic(
        db,
        db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
            generic_function: *func_id,
            generic_args: vec![],
        }),
    ))
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let mut db_val = RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(starknet_plugin_suite())
        .build()?;

    let main_crate_ids = setup_project(&mut db_val, Path::new(&args.path))?;
    let db = &db_val;

    let res = if let Some(function_path) = args.function_path {
        let function_id = get_func_id_by_name(db, &main_crate_ids, function_path)?;

        match args.all {
            true => format!("{:?}", PhasesFormatter { db, function_id }),
            false => {
                let lowered = db.final_concrete_function_with_body_lowered(function_id).unwrap();
                let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
                format!("{:?}", lowered.debug(&lowered_formatter))
            }
        }
    } else {
        get_all_funcs(db, &main_crate_ids)?.keys().join("\n")
    };

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{res}"),
    }

    Ok(())
}
