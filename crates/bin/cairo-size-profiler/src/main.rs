//! This program compiles and analyzes the code size of a Cairo project.
//!
//! It supports both single files and entire projects, and can analyze either contracts or
//! executable functions. Depending on the provided arguments, it uses the appropriate plugin suite
//! for compilation.
//!
//! The program outputs size-related metrics, including weights by concrete and generic libfuncs, as
//! well as by user-defined functions. It also provides insights into the size contributions of
//! high-level user functions, including inlined instances.
//!
//! Exits with a status code of 1 if compilation or execution fails, and 0 otherwise.

use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_executable::compile::{find_executable_functions, originating_function_path};
use cairo_lang_executable_plugin::executable_plugin_suite;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::CrateInput;
use cairo_lang_lowering::optimizations::config::Optimizations;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_runnable_utils::builder::RunnableBuilder;
use cairo_lang_runner::profiling::user_function_idx_by_sierra_statement_idx;
use cairo_lang_sierra::program::{Statement, StatementIdx};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::{DebugReplacer, SierraIdReplacer};
use cairo_lang_starknet::compile::{SemanticEntryPoints, extract_semantic_entrypoints};
use cairo_lang_starknet::contract::find_contracts;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use clap::Parser;
use itertools::{Itertools, chain};
use salsa::Database;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Compiles a Cairo project and analyzes the size-related costs of its components.
/// Exits with a status code of 1 if compilation or analysis fails, and 0 otherwise.
#[derive(Parser, Debug)]
#[command(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path to compile and run.
    path: PathBuf,
    /// Whether the path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// Allows the compilation to succeed with warnings.
    #[arg(long)]
    allow_warnings: bool,
    /// A path to a contract to analyze the size of.
    ///
    /// Implies usage of the Starknet plugin.
    #[arg(long, conflicts_with = "executable")]
    contract: Option<String>,
    /// A path to a function to analyze the size of.
    ///
    /// Implies executable plugin.
    #[arg(long, conflicts_with = "contract")]
    executable: Option<String>,

    /// The inline threshold to use with the `InlineSmallFunctions` strategy.
    #[arg(long)]
    inline_threshold: Option<usize>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let mut db_builder = RootDatabase::builder();
    db_builder.detect_corelib();
    if args.contract.is_some() {
        db_builder.with_default_plugin_suite(starknet_plugin_suite());
    }
    if args.executable.is_some() {
        db_builder
            .skip_auto_withdraw_gas()
            .with_cfg(CfgSet::from_iter([Cfg::kv("gas", "disabled")]))
            .with_default_plugin_suite(executable_plugin_suite());
    }
    if let Some(inline_threshold) = args.inline_threshold {
        db_builder.with_optimizations(Optimizations::enabled_with_default_movable_functions(
            InliningStrategy::InlineSmallFunctions(inline_threshold),
        ));
    }
    let db = &mut db_builder.build()?;

    let main_crate_inputs = setup_project(db, Path::new(&args.path))?;

    let mut reporter = DiagnosticsReporter::stderr().with_crates(&main_crate_inputs);
    if args.allow_warnings {
        reporter = reporter.allow_warnings();
    }
    let mut check_diags = || {
        if reporter.check(db) {
            anyhow::bail!("failed to compile: {}", args.path.display());
        } else {
            Ok(())
        }
    };
    let main_crate_ids = CrateInput::into_crate_ids(db, main_crate_inputs);
    let sierra = if let Some(executable_path) = args.executable {
        let executables = find_executable_functions(db, main_crate_ids, None);
        let Some(executable) =
            executables.iter().find(|f| originating_function_path(db, **f) == executable_path)
        else {
            anyhow::bail!(
                "Executable not found. Available executables:\n{}",
                executables.into_iter().map(|f| originating_function_path(db, f)).join("\n")
            );
        };
        check_diags()?;
        db.get_sierra_program_for_functions(vec![*executable])
    } else if let Some(contract_path) = &args.contract {
        let contracts = find_contracts(db, &main_crate_ids);
        let Some(contract) =
            contracts.iter().find(|contract| contract.submodule_id.full_path(db) == *contract_path)
        else {
            anyhow::bail!(
                "Contract not found. Available contracts:\n{}",
                contracts
                    .into_iter()
                    .map(|contract| contract.submodule_id.full_path(db))
                    .join("\n")
            );
        };
        check_diags()?;

        let SemanticEntryPoints { external, l1_handler, constructor } =
            extract_semantic_entrypoints(db, contract)?;
        db.get_sierra_program_for_functions(
            chain!(&external, &l1_handler, &constructor).map(|f| f.value).collect(),
        )
    } else {
        check_diags()?;
        db.get_sierra_program(main_crate_ids)
    }
    .to_option()
    .with_context(|| "Compilation failed without any diagnostics.")?;

    let replacer = DebugReplacer { db };
    let builder = RunnableBuilder::new(
        replacer.apply(&sierra.program),
        args.contract.is_some().then(Default::default),
    )?;
    let casm = builder.casm_program();
    let sierra_program = builder.sierra_program();
    let mut concrete_libfunc_size = OrderedHashMap::<String, usize>::default();
    let mut generic_libfunc_size = OrderedHashMap::<String, usize>::default();
    let mut final_user_function_size = OrderedHashMap::<String, usize>::default();
    let mut user_function_size = OrderedHashMap::<String, usize>::default();
    for (i, info) in casm.debug_info.sierra_statement_info.iter().enumerate() {
        let casm_size = info.end_offset - info.start_offset;
        let idx = StatementIdx(i);
        let statement = &sierra_program.statements[i];
        let concrete = match statement {
            Statement::Invocation(invocation) => invocation.libfunc_id.to_string(),
            Statement::Return(_) => "return".to_string(),
        };
        let generic = if let Some(generic_prefix) = concrete.find("<") {
            concrete.split_at(generic_prefix).0.to_string()
        } else {
            concrete.to_string()
        };
        *generic_libfunc_size.entry(generic).or_default() += casm_size;
        *concrete_libfunc_size.entry(concrete).or_default() += casm_size;
        let user_function_idx = user_function_idx_by_sierra_statement_idx(sierra_program, idx);
        let user_function = sierra_program.funcs[user_function_idx].id.to_string();
        *final_user_function_size.entry(user_function).or_default() += casm_size;
        if let Some(locations) = &sierra.debug_info.statements_locations.locations.get(&idx) {
            // TODO(orizi): Find the number of times each function is actually inlined.
            for loc in locations.iter() {
                let mut segments = vec![];
                let mut node = loc.syntax_node(db);
                while let Some(parent) = node.parent(db) {
                    if let Some(name) = try_extract_path_segment_name(db, node) {
                        segments.push(name.text(db).to_string(db));
                    }
                    node = parent;
                }
                let name =
                    chain!([loc.file_id(db).full_path(db)], segments.into_iter().rev()).join("::");
                *user_function_size.entry(name).or_default() += casm_size;
            }
        }
    }

    let total_size: usize = casm.instructions.iter().map(|inst| inst.body.op_size()).sum();
    println!("Total weight (felt252 count): {total_size}");

    println!("Weight by concrete libfunc:");
    for (concrete_name, weight) in filter_and_sort(concrete_libfunc_size) {
        println!("  {concrete_name}: {weight}");
    }
    println!("Weight by generic libfunc:");
    for (generic_name, weight) in filter_and_sort(generic_libfunc_size) {
        println!("  {generic_name}: {weight}");
    }
    println!("Weight by final user function (inc. generated):");
    for (name, weight) in filter_and_sort(final_user_function_size) {
        println!("  {name}: {weight}");
    }
    println!("Weight by high-level user function (inc. inlined instances):");
    for (name, weight) in filter_and_sort(user_function_size) {
        println!("  {name}: {weight}");
    }

    Ok(())
}

/// Tries to extract a relevant name from the provided syntax node.
/// Used to reconstruct function paths.
fn try_extract_path_segment_name<'db>(
    db: &'db dyn Database,
    node: cairo_lang_syntax::node::SyntaxNode<'db>,
) -> Option<ast::TerminalIdentifier<'db>> {
    match node.kind(db) {
        SyntaxKind::FunctionWithBody => {
            Some(ast::FunctionWithBody::from_syntax_node(db, node).declaration(db).name(db))
        }
        SyntaxKind::ItemModule => Some(ast::ItemModule::from_syntax_node(db, node).name(db)),
        SyntaxKind::ItemTrait => Some(ast::ItemTrait::from_syntax_node(db, node).name(db)),
        SyntaxKind::ItemImpl => Some(ast::ItemImpl::from_syntax_node(db, node).name(db)),
        _ => None,
    }
}

/// Filters out zero values and sorts by value in descending order.
fn filter_and_sort(map: OrderedHashMap<String, usize>) -> impl Iterator<Item = (String, usize)> {
    map.into_iter().filter(|(_, v)| *v > 0).sorted_by_key(|(_, v)| usize::MAX - *v)
}
