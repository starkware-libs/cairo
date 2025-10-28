//! Internal debug utility for printing lowering phases.

use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{NamedLanguageElementId, TopLevelLanguageElementId};
use cairo_lang_executable_plugin::executable_plugin_suite;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::{CrateId, CrateInput};
use cairo_lang_lowering::add_withdraw_gas::add_withdraw_gas;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::destructs::add_destructs;
use cairo_lang_lowering::fmt::LoweredFormatter;
use cairo_lang_lowering::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey,
};
use cairo_lang_lowering::optimizations::scrub_units::scrub_units;
use cairo_lang_lowering::panic::lower_panics;
use cairo_lang_lowering::{Lowered, LoweringStage};
use cairo_lang_semantic::ConcreteImplLongId;
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionWithBodyId, ImplFunctionBodyId,
    ImplGenericFunctionWithBodyId,
};
use cairo_lang_semantic::items::imp::ImplSemantic;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_plugin::test_plugin_suite;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use clap::Parser;
use convert_case::Casing;
use itertools::Itertools;
use salsa::Database;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Tests that `PhasesFormatter` is consistent with the lowering phases.
#[test]
fn test_lowering_consistency() {
    let db_val = RootDatabase::builder()
        .detect_corelib()
        .with_default_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();

    let db: &dyn Database = &db_val;

    let function_id = get_func_id_by_name(
        db,
        &[cairo_lang_semantic::corelib::CorelibSemantic::core_crate(db)],
        "core::poseidon::_poseidon_hash_span_inner".to_string(),
    )
    .unwrap();
    let _unused = PhasesDisplay { db, function_id }.to_string();
}

/// Prints the lowering of a concrete function:
///
/// Usage example:
///     cargo run --bin get-lowering corelib/ core::poseidon::poseidon_hash_span
///
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[command(version, verbatim_doc_comment)]
struct Args {
    /// The crate to compile.
    path: PathBuf,
    /// The concrete functions fully qualified path, if None print all functions.
    function_path: Option<String>,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,

    /// whether to print all lowering stages or only the final lowering.
    #[arg(short, long)]
    all: bool,

    /// Disables gas handling.
    #[arg(short, long)]
    no_gas: bool,

    /// Use the executable plugin suite instead of the Starknet suite
    #[arg(short, long)]
    executable: bool,

    /// The index of the generated function to output.
    #[arg(long)]
    generated_function_index: Option<usize>,

    /// The output file name (default: stdout).
    output: Option<String>,
}

/// Helper class for formatting the lowering phases of a concrete function.
struct PhasesDisplay<'a> {
    db: &'a dyn Database,
    function_id: ConcreteFunctionWithBodyId<'a>,
}

impl<'a> fmt::Display for PhasesDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let db = self.db;
        let function_id = self.function_id;

        let mut curr_state =
            (*db.lowered_body(function_id, LoweringStage::Monomorphized).unwrap()).clone();

        let mut phase_index = 0;
        let mut add_stage_state = |name: &str, lowered: &Lowered<'_>| {
            writeln!(f, "{phase_index}. {name}: {}", LoweredDisplay::new(db, lowered)).unwrap();
            phase_index += 1;
        };
        add_stage_state("before_all", &curr_state);

        let mut apply_stage = |name: &'static str, stage: &dyn Fn(&mut Lowered<'a>)| {
            (*stage)(&mut curr_state);
            add_stage_state(name, &curr_state);
        };
        apply_stage("after_add_withdraw_gas", &|lowered| {
            add_withdraw_gas(db, function_id, lowered).unwrap()
        });
        apply_stage("after_lower_panics", &|lowered| {
            lower_panics(db, function_id, lowered).unwrap();
        });
        apply_stage("after_add_destructs", &|lowered| add_destructs(db, function_id, lowered));
        apply_stage("scrub_units", &|lowered| scrub_units(db, lowered));
        let pre_opts = db.lowered_body(self.function_id, LoweringStage::PreOptimizations).unwrap();
        let post_base_opts =
            db.lowered_body(self.function_id, LoweringStage::PostBaseline).unwrap();
        let final_state = db.lowered_body(self.function_id, LoweringStage::Final).unwrap();
        assert_eq!(
            LoweredDisplay::new(db, &curr_state).to_string(),
            LoweredDisplay::new(db, pre_opts).to_string()
        );
        for (strategy, expected) in [
            (db.baseline_optimization_strategy(), post_base_opts),
            (db.final_optimization_strategy(), final_state),
        ] {
            for phase in strategy.long(db).0.clone() {
                let name = format!("{phase:?}").to_case(convert_case::Case::Snake);
                phase.apply(db, function_id, &mut curr_state).unwrap();
                add_stage_state(&name, &curr_state);
            }
            assert_eq!(
                LoweredDisplay::new(db, &curr_state).to_string(),
                LoweredDisplay::new(db, expected).to_string()
            );
        }

        Ok(())
    }
}

/// Helper for displaying the lowered representation of a concrete function.
struct LoweredDisplay<'a> {
    db: &'a dyn Database,
    lowered: &'a Lowered<'a>,
}
impl<'a> LoweredDisplay<'a> {
    fn new(db: &'a dyn Database, lowered: &'a Lowered<'_>) -> Self {
        Self { db, lowered }
    }
}

impl fmt::Display for LoweredDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lowered_formatter = LoweredFormatter::new(self.db, &self.lowered.variables);
        let dbg = self.lowered.debug(&lowered_formatter);
        write!(f, "{dbg:?}")
    }
}

// Returns a dictionary mapping function names to their ids for all the functions in the given
// crate.
fn get_all_funcs<'db>(
    db: &'db dyn Database,
    crate_ids: &[CrateId<'db>],
) -> anyhow::Result<OrderedHashMap<String, GenericFunctionWithBodyId<'db>>> {
    let mut res: OrderedHashMap<String, GenericFunctionWithBodyId<'db>> = Default::default();
    for crate_id in crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            let free_funcs = db.module_free_functions_ids(*module_id).unwrap();
            for func_id in free_funcs.iter() {
                res.insert(func_id.full_path(db), GenericFunctionWithBodyId::Free(*func_id));
            }

            let impl_ids = db.module_impls_ids(*module_id).unwrap();
            for impl_def_id in impl_ids.iter() {
                let impl_funcs = db.impl_functions(*impl_def_id).unwrap();
                for impl_func in impl_funcs.values() {
                    res.insert(
                        impl_func.full_path(db),
                        GenericFunctionWithBodyId::Impl(ImplGenericFunctionWithBodyId {
                            concrete_impl_id: ConcreteImplLongId {
                                impl_def_id: *impl_def_id,
                                generic_args: vec![],
                            }
                            .intern(db),
                            function_body: ImplFunctionBodyId::Impl(*impl_func),
                        }),
                    );
                }
            }
        }
    }

    Ok(res)
}

/// Given a function name and list of crates, returns the Concrete id of the function.
fn get_func_id_by_name<'db>(
    db: &'db dyn Database,
    crate_ids: &[CrateId<'db>],
    function_path: String,
) -> anyhow::Result<ConcreteFunctionWithBodyId<'db>> {
    let all_funcs = get_all_funcs(db, crate_ids)?;
    let Some(func_id) = all_funcs.get(&function_path) else {
        anyhow::bail!("Function {} not found in the project.", function_path.as_str())
    };

    Ok(ConcreteFunctionWithBodyId::from_semantic(
        db,
        ConcreteFunctionWithBody { generic_function: *func_id, generic_args: vec![] }.intern(db),
    ))
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let mut db_builder = RootDatabase::builder();
    let mut cfg = CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")]);
    if args.no_gas {
        cfg.insert(Cfg::kv("gas", "disabled"));
        db_builder.skip_auto_withdraw_gas();
    }

    db_builder.with_cfg(cfg);
    db_builder.detect_corelib();
    db_builder.with_default_plugin_suite(test_plugin_suite());
    let plugin_suite =
        if args.executable { executable_plugin_suite() } else { starknet_plugin_suite() };

    db_builder.with_default_plugin_suite(plugin_suite);

    let mut db_val = db_builder.build()?;

    let main_crate_inputs = setup_project(&mut db_val, Path::new(&args.path))?;
    let main_crate_ids = CrateInput::into_crate_ids(&db_val, main_crate_inputs.clone());
    let db = &db_val;

    let res = if let Some(function_path) = args.function_path {
        let mut function_id = get_func_id_by_name(db, &main_crate_ids, function_path)?;
        if let Some(generated_function_index) = args.generated_function_index {
            let multi = db
                .priv_function_with_body_multi_lowering(
                    function_id.base_semantic_function(db).function_with_body_id(db),
                )
                .unwrap();
            let keys = multi
                .generated_lowerings
                .keys()
                .sorted_by_key(|key| match key {
                    GeneratedFunctionKey::Loop(id) => (id.0.lookup(db).span_without_trivia(db), ""),
                    GeneratedFunctionKey::TraitFunc(trait_function, id) => (
                        id.syntax_node(db).span_without_trivia(db),
                        trait_function.name(db).long(db).as_str(),
                    ),
                })
                .take(generated_function_index + 1)
                .collect_vec();

            let key = **keys.get(generated_function_index).with_context(|| {
                format!(
                    "Invalid generated function index. There are {} generated functions in the \
                     function",
                    keys.len()
                )
            })?;

            function_id = ConcreteFunctionWithBodyId::new(
                db,
                ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
                    parent: function_id.base_semantic_function(db),
                    key,
                }),
            );
        }

        let Ok(lowered) = db.lowered_body(function_id, LoweringStage::Final) else {
            // Run DiagnosticsReporter only in case of failure.
            DiagnosticsReporter::default()
                .with_crates(&main_crate_inputs)
                .ensure(db)
                .with_context(|| "Failed to compile")?;
            anyhow::bail!("Failed to get lowered function.")
        };

        if args.all {
            PhasesDisplay { db, function_id }.to_string()
        } else {
            LoweredDisplay::new(db, lowered).to_string()
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
