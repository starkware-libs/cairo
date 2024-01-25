use std::fs;
use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_lowering::utils::formatted_lowered;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionWithBodyId, ImplGenericFunctionWithBodyId,
};
use cairo_lang_semantic::ConcreteImplLongId;
use cairo_lang_starknet::starknet_plugin_suite;
use clap::Parser;

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

    let mut opt_func_id = None;
    for crate_id in &main_crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            let free_funcs = db.module_free_functions_ids(*module_id).unwrap();
            for func_id in free_funcs.iter() {
                if func_id.full_path(db) == args.function_path {
                    opt_func_id = Some(GenericFunctionWithBodyId::Free(*func_id));
                    break;
                }
            }

            let impl_ids = db.module_impls_ids(*module_id).unwrap();
            for impl_def_id in impl_ids.iter() {
                let impl_funcs = db.impl_functions(*impl_def_id).unwrap();
                for impl_func in impl_funcs.values() {
                    if impl_func.full_path(db) == args.function_path {
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
    let func_id = ConcreteFunctionWithBodyId::from_semantic(
        db,
        db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
            generic_function: opt_func_id.with_context(|| {
                format!("Function {} not found in the project.", args.function_path.as_str())
            })?,
            generic_args: vec![],
        }),
    );

    let lowered = db.concrete_function_with_body_lowered(func_id).unwrap();
    let res = formatted_lowered(db, &lowered);

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{res}"),
    }

    Ok(())
}
