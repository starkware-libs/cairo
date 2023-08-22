use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use cairo_lang_defs::db::{DefsDatabase, DefsGroup};
use cairo_lang_defs::ids::{FunctionWithBodyId, ModuleId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder};
use cairo_lang_filesystem::db::{
    init_dev_corelib, init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{
    CrateId, CrateLongId, Directory, FileKind, FileLongId, VirtualFile,
};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{extract_matches, OptionFrom, Upcast};
use once_cell::sync::Lazy;

use crate::db::{SemanticDatabase, SemanticGroup};
use crate::inline_macros::get_default_inline_macro_plugins;
use crate::items::functions::GenericFunctionId;
use crate::{semantic, ConcreteFunctionWithBodyId, SemanticDiagnostic};

#[salsa::database(SemanticDatabase, DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct SemanticDatabaseForTesting {
    storage: salsa::Storage<SemanticDatabaseForTesting>,
}
impl salsa::Database for SemanticDatabaseForTesting {}
impl salsa::ParallelDatabase for SemanticDatabaseForTesting {
    fn snapshot(&self) -> salsa::Snapshot<SemanticDatabaseForTesting> {
        salsa::Snapshot::new(SemanticDatabaseForTesting { storage: self.storage.snapshot() })
    }
}
impl SemanticDatabaseForTesting {
    pub fn new_empty() -> Self {
        let mut res = SemanticDatabaseForTesting { storage: Default::default() };
        init_files_group(&mut res);
        res.set_macro_plugins(get_default_plugins());
        res.set_inline_macro_plugins(get_default_inline_macro_plugins().into());
        let corelib_path = detect_corelib().expect("Corelib not found in default location.");
        init_dev_corelib(&mut res, corelib_path);
        res
    }
    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> SemanticDatabaseForTesting {
        SemanticDatabaseForTesting { storage: self.storage.snapshot() }
    }
}
pub static SHARED_DB: Lazy<Mutex<SemanticDatabaseForTesting>> =
    Lazy::new(|| Mutex::new(SemanticDatabaseForTesting::new_empty()));
impl Default for SemanticDatabaseForTesting {
    fn default() -> Self {
        SHARED_DB.lock().unwrap().snapshot()
    }
}
impl AsFilesGroupMut for SemanticDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for SemanticDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for SemanticDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for SemanticDatabaseForTesting {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for SemanticDatabaseForTesting {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}

pub struct WithStringDiagnostics<T> {
    value: T,
    diagnostics: String,
}
impl<T> WithStringDiagnostics<T> {
    /// Verifies that there are no diagnostics (fails otherwise), and returns the inner value.
    pub fn unwrap(self) -> T {
        assert!(self.diagnostics.is_empty(), "Unexpected diagnostics:\n{}", self.diagnostics);
        self.value
    }

    /// Returns the inner value and the diagnostics (as a string).
    pub fn split(self) -> (T, String) {
        (self.value, self.diagnostics)
    }

    /// Returns the diagnostics (as a string).
    pub fn get_diagnostics(self) -> String {
        self.diagnostics
    }
}

/// Helper struct for the return value of [setup_test_module].
pub struct TestModule {
    pub crate_id: CrateId,
    pub module_id: ModuleId,
}

/// Sets up a crate with given content, and returns its crate id.
pub fn setup_test_crate(db: &dyn SemanticGroup, content: &str) -> CrateId {
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "lib.cairo".into(),
        content: Arc::new(content.into()),
        diagnostics_mappings: Default::default(),
        kind: FileKind::Module,
    }));

    db.intern_crate(CrateLongId::Virtual {
        name: "test".into(),
        root: Directory::Virtual {
            files: BTreeMap::from([("lib.cairo".into(), file_id)]),
            dirs: Default::default(),
        },
    })
}

/// Sets up a module with given content, and returns its module id.
pub fn setup_test_module(
    db: &(dyn SemanticGroup + 'static),
    content: &str,
) -> WithStringDiagnostics<TestModule> {
    let crate_id = setup_test_crate(db, content);
    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_main_file(module_id).unwrap();

    let syntax_diagnostics = db.file_syntax_diagnostics(file_id).format(Upcast::upcast(db));
    let semantic_diagnostics = db.module_semantic_diagnostics(module_id).unwrap().format(db);

    WithStringDiagnostics {
        value: TestModule { crate_id, module_id },
        diagnostics: format!("{syntax_diagnostics}{semantic_diagnostics}"),
    }
}

/// Helper struct for the return value of [setup_test_function].
pub struct TestFunction {
    pub module_id: ModuleId,
    pub function_id: FunctionWithBodyId,
    pub concrete_function_id: ConcreteFunctionWithBodyId,
    pub signature: semantic::Signature,
    pub body: semantic::ExprId,
}

/// Returns the semantic model of a given function.
/// function_name - name of the function.
/// module_code - extra setup code in the module context.
pub fn setup_test_function(
    db: &(dyn SemanticGroup + 'static),
    function_code: &str,
    function_name: &str,
    module_code: &str,
) -> WithStringDiagnostics<TestFunction> {
    let content = if module_code.is_empty() {
        function_code.to_string()
    } else {
        format!("{module_code}\n{function_code}")
    };
    let (test_module, diagnostics) = setup_test_module(db, &content).split();
    let generic_function_id = db
        .module_item_by_name(test_module.module_id, function_name.into())
        .expect("Failed to load module")
        .and_then(GenericFunctionId::option_from)
        .unwrap_or_else(|| panic!("Function '{function_name}' was not found."));
    let free_function_id = extract_matches!(generic_function_id, GenericFunctionId::Free);
    let function_id = FunctionWithBodyId::Free(free_function_id);
    WithStringDiagnostics {
        value: TestFunction {
            module_id: test_module.module_id,
            function_id,
            concrete_function_id: ConcreteFunctionWithBodyId::from_no_generics_free(
                db,
                free_function_id,
            )
            .unwrap(),
            signature: db.function_with_body_signature(function_id).unwrap(),
            body: db.function_body_expr(function_id).unwrap(),
        },
        diagnostics,
    }
}

/// Helper struct for the return value of [setup_test_expr] and [setup_test_block].
pub struct TestExpr {
    pub module_id: ModuleId,
    pub function_id: FunctionWithBodyId,
    pub signature: semantic::Signature,
    pub body: semantic::ExprId,
    pub expr_id: semantic::ExprId,
}

/// Returns the semantic model of a given expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
pub fn setup_test_expr(
    db: &(dyn SemanticGroup + 'static),
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> WithStringDiagnostics<TestExpr> {
    let function_code = format!("fn test_func() {{ {function_body} {{\n{expr_code}\n}}; }}");
    let (test_function, diagnostics) =
        setup_test_function(db, &function_code, "test_func", module_code).split();
    let semantic::ExprBlock { statements, .. } = extract_matches!(
        db.expr_semantic(test_function.function_id, test_function.body),
        semantic::Expr::Block
    );
    let statement_expr = extract_matches!(
        db.statement_semantic(test_function.function_id, *statements.last().unwrap()),
        semantic::Statement::Expr
    );
    let semantic::ExprBlock { statements, tail, .. } = extract_matches!(
        db.expr_semantic(test_function.function_id, statement_expr.expr),
        semantic::Expr::Block
    );
    assert!(
        statements.is_empty(),
        "expr_code is not a valid expression. Consider using setup_test_block()."
    );
    WithStringDiagnostics {
        value: TestExpr {
            module_id: test_function.module_id,
            function_id: test_function.function_id,
            signature: test_function.signature,
            body: test_function.body,
            expr_id: tail.unwrap(),
        },
        diagnostics,
    }
}

/// Returns the semantic model of a given block expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
pub fn setup_test_block(
    db: &(dyn SemanticGroup + 'static),
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> WithStringDiagnostics<TestExpr> {
    setup_test_expr(db, &format!("{{ \n{expr_code}\n }}"), module_code, function_body)
}

pub fn test_expr_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> Result<OrderedHashMap<String, String>, String> {
    let db = &SemanticDatabaseForTesting::default();
    Ok(OrderedHashMap::from([(
        "expected_diagnostics".into(),
        setup_test_expr(
            db,
            inputs["expr_code"].as_str(),
            inputs["module_code"].as_str(),
            inputs["function_body"].as_str(),
        )
        .get_diagnostics(),
    )]))
}

pub fn test_function_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> Result<OrderedHashMap<String, String>, String> {
    let db = &SemanticDatabaseForTesting::default();
    Ok(OrderedHashMap::from([(
        "expected_diagnostics".into(),
        setup_test_function(
            db,
            inputs["function"].as_str(),
            inputs["function_name"].as_str(),
            inputs["module_code"].as_str(),
        )
        .get_diagnostics(),
    )]))
}

/// Gets the diagnostics for all the modules (including nested) in the given crate.
pub fn get_crate_semantic_diagnostics(
    db: &dyn SemanticGroup,
    crate_id: CrateId,
) -> Diagnostics<SemanticDiagnostic> {
    let submodules = db.crate_modules(crate_id);
    let mut diagnostics = DiagnosticsBuilder::default();
    for submodule_id in submodules.iter() {
        diagnostics.extend(db.module_semantic_diagnostics(*submodule_id).unwrap());
    }
    diagnostics.build()
}
