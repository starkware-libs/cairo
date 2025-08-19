use std::sync::{Arc, LazyLock, Mutex};

use cairo_lang_defs::db::{DefsGroup, init_defs_group, init_external_files};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{FilesGroup, init_dev_corelib, init_files_group};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagLongId;
use cairo_lang_lowering::db::{LoweringGroup, UseApproxCodeSizeEstimator, lowering_group_input};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::{Elongate, PluginSuiteInput, SemanticGroup, init_semantic_group};
use cairo_lang_semantic::test_utils::setup_test_crate;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program;
use cairo_lang_utils::{Intern, Upcast};
use defs::ids::FreeFunctionId;
use lowering::ids::ConcreteFunctionWithBodyLongId;
use lowering::optimizations::config::OptimizationConfig;
use salsa::Setter;
use semantic::inline_macros::get_default_plugin_suite;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self, LabelLongId};
use crate::program_generator::SierraProgramWithDebug;
use crate::replace_ids::replace_sierra_ids_in_program;
use crate::utils::{jump_statement, return_statement, simple_statement};

impl UseApproxCodeSizeEstimator for SierraGenDatabaseForTesting {}

#[salsa::db]
#[derive(Clone)]
pub struct SierraGenDatabaseForTesting {
    storage: salsa::Storage<SierraGenDatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for SierraGenDatabaseForTesting {}

pub static SHARED_DB: LazyLock<Mutex<SierraGenDatabaseForTesting>> =
    LazyLock::new(|| Mutex::new(SierraGenDatabaseForTesting::new_empty()));
pub static SHARED_DB_WITHOUT_AD_WITHDRAW_GAS: LazyLock<Mutex<SierraGenDatabaseForTesting>> =
    LazyLock::new(|| {
        let mut db = SierraGenDatabaseForTesting::new_empty();
        let add_withdraw_gas_flag_id = FlagLongId("add_withdraw_gas".into());
        db.set_flag(add_withdraw_gas_flag_id, Some(Arc::new(Flag::AddWithdrawGas(false))));
        Mutex::new(db)
    });
impl SierraGenDatabaseForTesting {
    pub fn new_empty() -> Self {
        let mut res = SierraGenDatabaseForTesting { storage: Default::default() };
        init_external_files(&mut res);
        init_files_group(&mut res);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        let plugin_suite = get_default_plugin_suite();
        res.set_default_plugins_from_suite(plugin_suite);

        lowering_group_input(&res)
            .set_optimization_config(&mut res)
            .to(Some(OptimizationConfig::default().with_minimal_movable_functions()));

        let corelib_path = detect_corelib().expect("Corelib not found in default location.");
        init_dev_corelib(&mut res, corelib_path);
        res
    }
    pub fn without_add_withdraw_gas() -> Self {
        SHARED_DB_WITHOUT_AD_WITHDRAW_GAS.lock().unwrap().snapshot()
    }
    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> SierraGenDatabaseForTesting {
        SierraGenDatabaseForTesting { storage: self.storage.clone() }
    }
}
impl Default for SierraGenDatabaseForTesting {
    fn default() -> Self {
        SHARED_DB.lock().unwrap().snapshot()
    }
}
impl<'db> Upcast<'db, dyn salsa::Database> for SierraGenDatabaseForTesting {
    fn upcast(&'db self) -> &'db dyn salsa::Database {
        self
    }
}
impl<'db> Upcast<'db, dyn DefsGroup> for SierraGenDatabaseForTesting {
    fn upcast(&'db self) -> &'db dyn DefsGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn SemanticGroup> for SierraGenDatabaseForTesting {
    fn upcast(&'db self) -> &'db dyn SemanticGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn LoweringGroup> for SierraGenDatabaseForTesting {
    fn upcast(&'db self) -> &'db dyn LoweringGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn ParserGroup> for SierraGenDatabaseForTesting {
    fn upcast(&'db self) -> &'db dyn ParserGroup {
        self
    }
}

impl Elongate for SierraGenDatabaseForTesting {
    fn elongate(&self) -> &dyn SemanticGroup {
        self
    }
}

/// Compiles `content` to sierra and replaces the sierra ids to make it readable.
pub fn checked_compile_to_sierra(content: &str) -> cairo_lang_sierra::program::Program {
    let db = SierraGenDatabaseForTesting::default();
    let crate_id = setup_db_and_get_crate_id(&db, content);

    let SierraProgramWithDebug { program, .. } =
        Arc::unwrap_or_clone(db.get_sierra_program(vec![crate_id]).expect(
            "`get_sierra_program` failed. run with RUST_LOG=warn (or less) to see diagnostics",
        ));
    replace_sierra_ids_in_program(&db, &program)
}

/// Adds `content` to a salsa db and returns the crate id that points to it.
pub fn setup_db_and_get_crate_id<'db>(
    db: &'db SierraGenDatabaseForTesting,
    content: &str,
) -> cairo_lang_filesystem::ids::CrateId<'db> {
    let crate_id = setup_test_crate(db, content);
    let module_id = ModuleId::CrateRoot(crate_id);
    db.module_semantic_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected semantic diagnostics");
    db.module_lowering_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected lowering diagnostics.");
    crate_id
}

pub fn get_dummy_function(db: &dyn SierraGenGroup) -> FreeFunctionId<'_> {
    let crate_id = setup_test_crate(db, "fn test(){}");
    let module_id = ModuleId::CrateRoot(crate_id);
    db.module_free_functions_ids(module_id).unwrap()[0]
}

/// Generates a dummy statement with the given name, inputs and outputs.
pub fn dummy_simple_statement<'db>(
    db: &dyn SierraGenGroup,
    name: &str,
    inputs: &[&str],
    outputs: &[&str],
) -> pre_sierra::StatementWithLocation<'db> {
    simple_statement(
        dummy_concrete_lib_func_id(db, name),
        &as_var_id_vec(inputs),
        &as_var_id_vec(outputs),
    )
}

fn dummy_concrete_lib_func_id(db: &dyn SierraGenGroup, name: &str) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string(name),
        generic_args: vec![],
    })
}

/// Returns a vector of variable ids based on the inputs mapped into variable ids.
pub fn as_var_id_vec(ids: &[&str]) -> Vec<cairo_lang_sierra::ids::VarId> {
    ids.iter().map(|id| (*id).into()).collect()
}

/// Generates a dummy statement with two branches. One branch is Fallthrough and the other is to the
/// given label.
pub fn dummy_simple_branch<'db>(
    db: &'db dyn SierraGenGroup,
    name: &str,
    args: &[&str],
    target: usize,
) -> pre_sierra::StatementWithLocation<'db> {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id: dummy_concrete_lib_func_id(db, name),
        args: as_var_id_vec(args),
        branches: vec![
            program::GenBranchInfo {
                target: program::GenBranchTarget::Statement(label_id_from_usize(db, target)),
                results: vec![],
            },
            program::GenBranchInfo {
                target: program::GenBranchTarget::Fallthrough,
                results: vec![],
            },
        ],
    }))
    .into_statement_without_location()
}

/// Generates a dummy return statement.
pub fn dummy_return_statement<'db>(args: &[&str]) -> pre_sierra::StatementWithLocation<'db> {
    return_statement(as_var_id_vec(args)).into_statement_without_location()
}

/// Generates a dummy label.
pub fn dummy_label<'db>(
    db: &'db dyn SierraGenGroup,
    id: usize,
) -> pre_sierra::StatementWithLocation<'db> {
    pre_sierra::Statement::Label(pre_sierra::Label { id: label_id_from_usize(db, id) })
        .into_statement_without_location()
}

/// Generates a dummy jump to label statement.
pub fn dummy_jump_statement<'db>(
    db: &'db dyn SierraGenGroup,
    id: usize,
) -> pre_sierra::StatementWithLocation<'db> {
    jump_statement(dummy_concrete_lib_func_id(db, "jump"), label_id_from_usize(db, id))
        .into_statement_without_location()
}

/// Returns the [pre_sierra::LabelId] for the given `id`.
pub fn label_id_from_usize(db: &dyn SierraGenGroup, id: usize) -> pre_sierra::LabelId<'_> {
    let free_function_id = get_dummy_function(db);
    let semantic_function = semantic::items::functions::ConcreteFunctionWithBody {
        generic_function: semantic::items::functions::GenericFunctionWithBodyId::Free(
            free_function_id,
        ),
        generic_args: vec![],
    }
    .intern(db);
    let parent = ConcreteFunctionWithBodyLongId::Semantic(semantic_function).intern(db);
    LabelLongId { parent, id }.intern(db)
}

/// Generates a dummy [PushValues](pre_sierra::Statement::PushValues) statement.
///
/// values is a list of pairs (src, dst) where src refers to a variable that should be pushed onto
/// the stack, and dst is the variable after placing it on the stack.
pub fn dummy_push_values<'db>(
    db: &'db dyn SierraGenGroup,
    values: &[(&str, &str)],
) -> pre_sierra::StatementWithLocation<'db> {
    dummy_push_values_ex(
        db,
        &values.iter().map(|(src, dst)| (*src, *dst, false)).collect::<Vec<_>>(),
    )
}

/// Same as [dummy_push_values] except that it also accepts a value for `dup`.
pub fn dummy_push_values_ex<'db>(
    db: &'db dyn SierraGenGroup,
    values: &[(&str, &str, bool)],
) -> pre_sierra::StatementWithLocation<'db> {
    let felt252_ty =
        db.get_concrete_type_id(db.core_info().felt252).expect("Can't find core::felt252.");
    pre_sierra::Statement::PushValues(
        values
            .iter()
            .map(|(src, dst, dup)| pre_sierra::PushValue {
                var: (*src).into(),
                var_on_stack: (*dst).into(),
                ty: felt252_ty.clone(),
                dup: *dup,
            })
            .collect(),
    )
    .into_statement_without_location()
}

/// Creates a test for a given function that reads test files.
///
/// filenames - a vector of tests files the test will apply to.
/// db - the salsa DB to use for the test.
/// func - the function to be applied on the test params to generate the tested result.
/// params - the function parameters. For functions specialized here the parameters can be omitted.
#[macro_export]
macro_rules! diagnostics_test {
    ($test_name:ident, $filenames:expr, $db:expr, $func:expr, $($param:expr),*) => {
        #[test]
        fn $test_name() -> Result<(), std::io::Error> {
            let mut db = $db;
            for filename in $filenames{
                let tests = cairo_lang_utils::parse_test_file::parse_test_file(
                    std::path::Path::new(filename)
                )?;
                for (name, test) in tests {
                    let test_expr = $func(
                        &mut db,
                        $(&test.attributes[$param],)*
                    )
                    .unwrap();
                    verify_exception(&db, test_expr, &test.attributes["Expected Result"], &name);
                }
            }
            Ok(())
        }
    };

    ($test_name:ident, $filenames:expr, $db:expr, setup_test_block) => {
        diagnostics_test!(
            $test_name,
            $filenames,
            $db,
            setup_test_block,
            "Expr Code",
            "Module Code",
            "Function Body"
        );
    };
}
