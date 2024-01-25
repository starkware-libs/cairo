use std::ops::Deref;
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
use cairo_lang_lowering::implicits::lower_implicits;
use cairo_lang_lowering::inline::apply_inlining;
use cairo_lang_lowering::optimizations::branch_inversion::branch_inversion;
use cairo_lang_lowering::optimizations::cancel_ops::cancel_ops;
use cairo_lang_lowering::optimizations::const_folding::const_folding;
use cairo_lang_lowering::optimizations::match_optimizer::optimize_matches;
use cairo_lang_lowering::optimizations::remappings::optimize_remappings;
use cairo_lang_lowering::optimizations::reorder_statements::reorder_statements;
use cairo_lang_lowering::optimizations::return_optimization::return_optimization;
use cairo_lang_lowering::optimizations::split_structs::split_structs;
use cairo_lang_lowering::panic::lower_panics;
use cairo_lang_lowering::reorganize_blocks::reorganize_blocks;
use cairo_lang_lowering::FlatLowered;
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionWithBodyId, ImplGenericFunctionWithBodyId,
};
use cairo_lang_semantic::ConcreteImplLongId;
use cairo_lang_starknet::starknet_plugin_suite;
use clap::Parser;

#[test]
/// Tests that `PhasesFormatter` is consistent with the lowering phases.
fn test_lowering_consistency() {
    let db_val = RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();

    let db: &dyn LoweringGroup = &db_val;

    format_phases_by_name(db, &[db.core_crate()], "core::poseidon::poseidon_hash_span".to_string())
        .unwrap();
}

/// Prints the lowering of non-generic function function:
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
    // The functions fully qualified path.
    function_path: String,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,

    /// The output file name (default: stdout).
    output: Option<String>,
}

struct PhasesFormatter<'a> {
    db: &'a dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
}

impl fmt::Debug for PhasesFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let db = self.db;
        let function_id = self.function_id;
        let before_all =
            db.priv_concrete_function_with_body_lowered_flat(self.function_id).unwrap();
        assert!(
            before_all.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset blocks"
        );

        let mut add_stage_state = |name: &str, lowered: &FlatLowered| {
            writeln!(f, "{name}:").unwrap();
            let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
            write!(f, "{:?}", lowered.debug(&lowered_formatter)).unwrap();
        };
        add_stage_state("before_all", &before_all);
        let mut curr_state = before_all.deref().clone();
        let mut apply_stage = |name: &str, stage: &dyn Fn(&mut FlatLowered)| {
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
        apply_stage("after_inlining", &|lowered| apply_inlining(db, function_id, lowered).unwrap());
        apply_stage("after_return_optimization", &|lowered| return_optimization(db, lowered));
        apply_stage("after_optimize_remappings1", &optimize_remappings);
        apply_stage("after_reorder_statements1", &|lowered| reorder_statements(db, lowered));
        apply_stage("after_branch_inversion", &|lowered| branch_inversion(db, lowered));
        apply_stage("after_reorder_statements2", &|lowered| reorder_statements(db, lowered));
        apply_stage("const_folding", &|lowered| const_folding(db, lowered));
        apply_stage("after_optimize_matches1", &optimize_matches);
        apply_stage("split_structs", &split_structs);
        apply_stage("after_reorder_statements3", &|lowered| reorder_statements(db, lowered));
        apply_stage("after_optimize_remappings2", &optimize_remappings);
        apply_stage("after_optimize_matches2", &optimize_matches);
        apply_stage("after_lower_implicits", &|lowered| lower_implicits(db, function_id, lowered));
        apply_stage("after_optimize_remappings3", &optimize_remappings);
        apply_stage("cancel_ops", &cancel_ops);
        apply_stage("after_reorder_statements4", &|lowered| reorder_statements(db, lowered));
        apply_stage("after_optimize_remappings4", &optimize_remappings);
        apply_stage("after_reorganize_blocks (final)", &reorganize_blocks);

        let after_all = db.concrete_function_with_body_lowered(function_id).unwrap();

        // This asserts that we indeed follow the logic of `concrete_function_with_body_lowered`.
        // If something is changed there, it should be changed here too.
        assert_eq!(*after_all, curr_state);

        Ok(())
    }
}

fn get_func_id_by_name(
    db: &dyn LoweringGroup,
    crate_ids: &[CrateId],
    function_path: String,
) -> anyhow::Result<ConcreteFunctionWithBodyId> {
    let mut opt_func_id = None;
    for crate_id in crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            let free_funcs = db.module_free_functions_ids(*module_id).unwrap();
            for func_id in free_funcs.iter() {
                if func_id.full_path(db.upcast()) == function_path {
                    opt_func_id = Some(GenericFunctionWithBodyId::Free(*func_id));
                    break;
                }
            }

            let impl_ids = db.module_impls_ids(*module_id).unwrap();
            for impl_def_id in impl_ids.iter() {
                let impl_funcs = db.impl_functions(*impl_def_id).unwrap();
                for impl_func in impl_funcs.values() {
                    if impl_func.full_path(db.upcast()) == function_path {
                        opt_func_id =
                            Some(GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                                concrete_impl_id: db.intern_concrete_impl(ConcreteImplLongId {
                                    impl_def_id: *impl_def_id,
                                    generic_args: vec![],
                                }),
                                function: *impl_func,
                            }));
                        break;
                    }
                }
            }
        }
    }

    Ok(ConcreteFunctionWithBodyId::from_semantic(
        db,
        db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
            generic_function: opt_func_id.with_context(|| {
                format!("Function {} not found in the project.", function_path.as_str())
            })?,
            generic_args: vec![],
        }),
    ))
}

fn format_phases_by_name(
    db: &dyn LoweringGroup,
    crate_ids: &[CrateId],
    function_path: String,
) -> anyhow::Result<String> {
    let function_id = get_func_id_by_name(db, crate_ids, function_path)?;

    Ok(format!("{:?}", PhasesFormatter { db, function_id }))
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

    let res = format_phases_by_name(db, &main_crate_ids, args.function_path)?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{res}"),
    }

    Ok(())
}
