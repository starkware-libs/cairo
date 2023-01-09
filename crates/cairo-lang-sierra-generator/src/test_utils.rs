use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use cairo_lang_lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use cairo_lang_semantic::test_utils::setup_test_crate;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;
use salsa::{InternId, InternKey};
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::db::{SierraGenDatabase, SierraGenGroup};
use crate::pre_sierra;
use crate::replace_ids::replace_sierra_ids_in_program;
use crate::utils::{jump_statement, return_statement, simple_statement};

#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    SyntaxDatabase
)]
pub struct SierraGenDatabaseForTesting {
    storage: salsa::Storage<SierraGenDatabaseForTesting>,
}
impl salsa::Database for SierraGenDatabaseForTesting {}
impl Default for SierraGenDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res);
        res.set_semantic_plugins(get_default_plugins());
        res
    }
}
impl AsFilesGroupMut for SierraGenDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn semantic::db::SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn lowering::db::LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for SierraGenDatabaseForTesting {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}

/// Compiles `content` to sierra and replaces the sierra ids to make it readable.
pub fn checked_compile_to_sierra(content: &str) -> cairo_lang_sierra::program::Program {
    let (db, crate_id) = setup_db_and_get_crate_id(content);

    let program = db.get_sierra_program(vec![crate_id]).unwrap();
    replace_sierra_ids_in_program(&db, &program)
}

/// Adds `content` to a salsa db and returns the crate id that points to it.
pub fn setup_db_and_get_crate_id(
    content: &str,
) -> (SierraGenDatabaseForTesting, cairo_lang_filesystem::ids::CrateId) {
    let mut db_val = SierraGenDatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = setup_test_crate(db, content);
    let module_id = ModuleId::CrateRoot(crate_id);
    db.module_semantic_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected semantic diagnostics");
    db.module_lowering_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected lowering diagnostics.");
    (db_val, crate_id)
}

/// Generates a dummy statement with the given name, inputs and outputs.
pub fn dummy_simple_statement(
    db: &dyn SierraGenGroup,
    name: &str,
    inputs: &[&str],
    outputs: &[&str],
) -> pre_sierra::Statement {
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
pub fn dummy_simple_branch(
    db: &dyn SierraGenGroup,
    name: &str,
    args: &[&str],
    target: usize,
) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id: dummy_concrete_lib_func_id(db, name),
        args: as_var_id_vec(args),
        branches: vec![
            program::GenBranchInfo {
                target: program::GenBranchTarget::Statement(label_id_from_usize(target)),
                results: vec![],
            },
            program::GenBranchInfo {
                target: program::GenBranchTarget::Fallthrough,
                results: vec![],
            },
        ],
    }))
}

/// Generates a dummy return statement.
pub fn dummy_return_statement(args: &[&str]) -> pre_sierra::Statement {
    return_statement(as_var_id_vec(args))
}

/// Generates a dummy label.
pub fn dummy_label(id: usize) -> pre_sierra::Statement {
    pre_sierra::Statement::Label(pre_sierra::Label { id: label_id_from_usize(id) })
}

/// Generates a dummy jump to label statement.
pub fn dummy_jump_statement(db: &dyn SierraGenGroup, id: usize) -> pre_sierra::Statement {
    jump_statement(dummy_concrete_lib_func_id(db, "jump"), label_id_from_usize(id))
}

/// Returns the [pre_sierra::LabelId] for the given `id`.
pub fn label_id_from_usize(id: usize) -> pre_sierra::LabelId {
    pre_sierra::LabelId::from_intern_id(InternId::from(id))
}

/// Generates a dummy [PushValues](pre_sierra::Statement::PushValues) statement.
///
/// values is a list of pairs (src, dst) where src refers to a variable that should be pushed onto
/// the stack, and dst is the variable after placing it on the stack.
pub fn dummy_push_values(
    db: &dyn SierraGenGroup,
    values: &[(&str, &str)],
) -> pre_sierra::Statement {
    dummy_push_values_ex(
        db,
        &values.iter().map(|(src, dst)| (*src, *dst, None)).collect::<Vec<_>>(),
    )
}

/// Same as [dummy_push_values] except that it also accepts value for `dup_var`.
pub fn dummy_push_values_ex(
    db: &dyn SierraGenGroup,
    values: &[(&str, &str, Option<&str>)],
) -> pre_sierra::Statement {
    let felt_ty = db.get_concrete_type_id(db.core_felt_ty()).expect("Can't find core::felt.");
    pre_sierra::Statement::PushValues(
        values
            .iter()
            .map(|(src, dst, dup_var)| pre_sierra::PushValue {
                var: (*src).into(),
                var_on_stack: (*dst).into(),
                ty: felt_ty.clone(),
                dup_var: dup_var.map(|x| (*x).into()),
            })
            .collect(),
    )
}

/// Creates a test for a given function that reads test files.
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
