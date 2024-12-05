use std::sync::{LazyLock, Mutex};

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, try_ext_as_virtual_impl};
use cairo_lang_defs::ids::{FunctionWithBodyId, ModuleId, SubmoduleId, SubmoduleLongId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder};
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, CrateSettings, Edition, ExperimentalFeaturesConfig, ExternalFiles,
    FilesDatabase, FilesGroup, init_dev_corelib, init_files_group,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, FileKind, FileLongId, VirtualFile};
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, OptionFrom, Upcast, extract_matches};

use crate::db::{SemanticDatabase, SemanticGroup};
use crate::inline_macros::get_default_plugin_suite;
use crate::items::functions::GenericFunctionId;
use crate::{ConcreteFunctionWithBodyId, SemanticDiagnostic, semantic};

#[salsa::database(SemanticDatabase, DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct SemanticDatabaseForTesting {
    storage: salsa::Storage<SemanticDatabaseForTesting>,
}
impl salsa::Database for SemanticDatabaseForTesting {}
impl ExternalFiles for SemanticDatabaseForTesting {
    fn try_ext_as_virtual(&self, external_id: salsa::InternId) -> Option<VirtualFile> {
        try_ext_as_virtual_impl(self.upcast(), external_id)
    }
}
impl salsa::ParallelDatabase for SemanticDatabaseForTesting {
    fn snapshot(&self) -> salsa::Snapshot<SemanticDatabaseForTesting> {
        salsa::Snapshot::new(SemanticDatabaseForTesting { storage: self.storage.snapshot() })
    }
}
impl SemanticDatabaseForTesting {
    pub fn new_empty() -> Self {
        let mut res = SemanticDatabaseForTesting { storage: Default::default() };
        init_files_group(&mut res);
        let suite = get_default_plugin_suite();
        res.set_macro_plugins(suite.plugins);
        res.set_inline_macro_plugins(suite.inline_macro_plugins.into());
        res.set_analyzer_plugins(suite.analyzer_plugins);
        let corelib_path = detect_corelib().expect("Corelib not found in default location.");
        init_dev_corelib(&mut res, corelib_path);
        res
    }
    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> SemanticDatabaseForTesting {
        SemanticDatabaseForTesting { storage: self.storage.snapshot() }
    }
}
pub static SHARED_DB: LazyLock<Mutex<SemanticDatabaseForTesting>> =
    LazyLock::new(|| Mutex::new(SemanticDatabaseForTesting::new_empty()));
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
impl Upcast<dyn ParserGroup> for SemanticDatabaseForTesting {
    fn upcast(&self) -> &(dyn ParserGroup + 'static) {
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

/// A module with [`ModuleId`] being a root of a crate with [`CrateId`].
pub struct TestModule {
    pub crate_id: CrateId,
    pub module_id: ModuleId,
}

impl TestModule {
    /// Setups the build of the [`TestModule`]. Creates a one-module crate with `content` and
    /// `crate_settings`.
    pub fn builder(
        db: &dyn SemanticGroup,
        content: &str,
        crate_settings: Option<&str>,
    ) -> TestModuleBuilder {
        TestModuleBuilder { crate_id: setup_test_crate(db, content, crate_settings) }
    }
}

/// A builder of [`TestModule`] performing additional syntactic and semantic verification
/// on the created module.
pub struct TestModuleBuilder {
    crate_id: CrateId,
}

impl TestModuleBuilder {
    /// Checks the [`CrateId`] **before** verifying the code in the test module in terms of syntax
    /// and semantics.
    /// Use only when lookup of the test crate's ID is required before verifying its contents.
    pub unsafe fn get_crate_id(&self) -> CrateId {
        self.crate_id
    }

    /// Checks the [`ModuleId`] **before** verifying the code in the test module in terms of syntax
    /// and semantics.
    /// Use only when lookup of the test module's ID is required before verifying its contents.
    pub unsafe fn get_module_id(&self) -> ModuleId {
        ModuleId::CrateRoot(self.crate_id)
    }

    /// Constructs the final [`TestModule`] and verifies it by computing syntactic and semantic
    /// diagnostics.
    pub fn build_and_check_for_diagnostics(
        self,
        db: &(dyn SemanticGroup + 'static),
    ) -> WithStringDiagnostics<TestModule> {
        let module_id = ModuleId::CrateRoot(self.crate_id);
        let file_id = db.module_main_file(module_id).unwrap();

        let syntax_diagnostics = db.file_syntax_diagnostics(file_id).format(db.upcast());
        let semantic_diagnostics =
            get_recursive_module_semantic_diagnostics(db, module_id).format(db);

        WithStringDiagnostics {
            value: TestModule { crate_id: module_id.owning_crate(db.upcast()), module_id },
            diagnostics: format!("{syntax_diagnostics}{semantic_diagnostics}"),
        }
    }
}

/// Sets up a crate with given content, and returns its crate id.
pub fn setup_test_crate(
    db: &dyn SemanticGroup,
    content: &str,
    crate_settings: Option<&str>,
) -> CrateId {
    let file_id = FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "lib.cairo".into(),
        content: content.into(),
        code_mappings: [].into(),
        kind: FileKind::Module,
    })
    .intern(db);

    let settings: CrateSettings = if let Some(crate_settings) = crate_settings {
        toml::from_str(crate_settings).expect("Invalid config.")
    } else {
        CrateSettings {
            name: None,
            edition: Edition::default(),
            version: None,
            dependencies: Default::default(),
            experimental_features: ExperimentalFeaturesConfig {
                negative_impls: true,
                associated_item_constraints: true,
                coupons: true,
            },
            cfg_set: Default::default(),
        }
    };

    CrateLongId::Virtual {
        name: "test".into(),
        file_id,
        settings: toml::to_string_pretty(&settings).unwrap(),
    }
    .intern(db)
}

/// A single function with [`FunctionId`] and body with [`semantic::ExprId`],
/// defined inside a module with [`ModuleId`].
pub struct TestFunction {
    pub module_id: ModuleId,
    pub function_id: FunctionWithBodyId,
    pub concrete_function_id: ConcreteFunctionWithBodyId,
    pub signature: semantic::Signature,
    pub body: semantic::ExprId,
}

impl TestFunction {
    /// Setups the build of the [`TestFunction`]. Creates a one-module crate with `crate_settings`,
    /// containing a function with `function_name` and `function_code` body.
    pub fn builder<'name>(
        db: &dyn SemanticGroup,
        function_code: &str,
        function_name: &'name str,
        module_code: &str,
        crate_settings: Option<&str>,
    ) -> TestFunctionBuilder<'name> {
        let content = if module_code.is_empty() {
            function_code.to_string()
        } else {
            format!("{module_code}\n{function_code}")
        };

        let module_builder = TestModule::builder(db, &content, crate_settings);
        TestFunctionBuilder { function_name, module_builder }
    }
}

/// A builder of [`TestFunction`] performing additional syntactic and semantic verification
/// on the created function.
pub struct TestFunctionBuilder<'name> {
    function_name: &'name str,
    module_builder: TestModuleBuilder,
}

impl<'name> TestFunctionBuilder<'name> {
    /// Checks the [`CrateId`] **before** verifying the function code in terms of syntax
    /// and semantics.
    /// Use only when lookup of the test crate's ID is required before verifying its contents.
    pub unsafe fn get_crate_id(&self) -> CrateId {
        self.module_builder.get_crate_id()
    }

    /// Checks the [`ModuleId`] **before** verifying the function code in terms of syntax
    /// and semantics.
    /// Use only when lookup of the test module's ID is required before verifying its contents.
    pub unsafe fn get_module_id(&self) -> ModuleId {
        self.module_builder.get_module_id()
    }

    /// Constructs the final [`TestFunction`] and verifies it by computing syntactic and semantic
    /// diagnostics.
    pub fn build_and_check_for_diagnostics(
        self,
        db: &(dyn SemanticGroup + 'static),
    ) -> WithStringDiagnostics<TestFunction> {
        let Self { function_name, module_builder } = self;

        let (test_module, diagnostics) = module_builder.build_and_check_for_diagnostics(db).split();
        let module_id = test_module.module_id;

        let generic_function_id = db
            .module_item_by_name(module_id, function_name.into())
            .expect("Failed to load module")
            .and_then(GenericFunctionId::option_from)
            .unwrap_or_else(|| panic!("Function '{function_name}' was not found."));

        let free_function_id = extract_matches!(generic_function_id, GenericFunctionId::Free);
        let function_id = FunctionWithBodyId::Free(free_function_id);

        WithStringDiagnostics {
            value: TestFunction {
                module_id,
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
}

/// A single expression placed in the context of a function with [`FunctionWithBodyId`] defined
/// inside a module with [`ModuleId`].
pub struct TestExpr {
    pub module_id: ModuleId,
    pub function_id: FunctionWithBodyId,
    pub signature: semantic::Signature,
    pub body: semantic::ExprId,
    pub expr_id: semantic::ExprId,
}

impl TestExpr {
    /// Setups the build of the [`TestExpr`]. Creates a one-module crate with `module_code` code and
    /// `crate_settings`, containing a function with `function_body` body ending with
    /// `expr_code` being a single expression.
    pub fn builder(
        db: &dyn SemanticGroup,
        expr_code: &str,
        module_code: &str,
        function_body: &str,
        crate_settings: Option<&str>,
    ) -> TestExprBuilder<'static> {
        let function_code = format!("fn test_func() {{ {function_body} {{\n{expr_code}\n}}; }}");
        let function_builder =
            TestFunction::builder(db, &function_code, "test_func", module_code, crate_settings);

        TestExprBuilder { function_builder }
    }

    pub fn build_block(
        db: &dyn SemanticGroup,
        expr_code: &str,
        module_code: &str,
        function_body: &str,
        crate_settings: Option<&str>,
    ) -> TestExprBuilder<'static> {
        let expr_code = &format!("{{ \n{expr_code}\n }}");
        Self::builder(db, expr_code, module_code, function_body, crate_settings)
    }
}

/// A builder of [`TestExpr`] performing additional syntactic and semantic verification
/// on the created module.
pub struct TestExprBuilder<'source> {
    function_builder: TestFunctionBuilder<'source>,
}

impl<'source> TestExprBuilder<'source> {
    /// Checks the [`CrateId`] **before** verifying the function code in terms of syntax
    /// and semantics.
    /// Use only when lookup of the test crate's ID is required before verifying its contents.
    pub unsafe fn get_crate_id(&self) -> CrateId {
        self.function_builder.get_crate_id()
    }

    /// Checks the [`ModuleId`] **before** verifying the function code in terms of syntax
    /// and semantics.
    /// Use only when lookup of the test module's ID is required before verifying its contents.
    pub unsafe fn get_module_id(&self) -> ModuleId {
        self.function_builder.get_module_id()
    }

    /// Constructs the final [`TestExpr`] and verifies it by computing syntactic and semantic
    /// diagnostics.
    pub fn build_and_check_for_diagnostics(
        self,
        db: &(dyn SemanticGroup + 'static),
    ) -> WithStringDiagnostics<TestExpr> {
        let Self { function_builder } = self;

        let (test_function, diagnostics) =
            function_builder.build_and_check_for_diagnostics(db).split();

        let function_id = test_function.function_id;
        let function_body = test_function.body;

        let semantic::ExprBlock { statements, .. } =
            extract_matches!(db.expr_semantic(function_id, function_body), semantic::Expr::Block);

        let statement_expr = extract_matches!(
            db.statement_semantic(function_id, *statements.last().unwrap()),
            semantic::Statement::Expr
        );

        let semantic::ExprBlock { statements, tail, .. } = extract_matches!(
            db.expr_semantic(function_id, statement_expr.expr),
            semantic::Expr::Block
        );

        assert!(
            statements.is_empty(),
            "expr_code is not a valid expression. Consider using setup_test_block()."
        );

        WithStringDiagnostics {
            value: TestExpr {
                module_id: test_function.module_id,
                function_id,
                signature: test_function.signature,
                body: function_body,
                expr_id: tail.unwrap(),
            },
            diagnostics,
        }
    }
}

pub fn test_expr_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SemanticDatabaseForTesting::default();

    let diagnostics = TestExpr::builder(
        db,
        inputs["expr_code"].as_str(),
        inputs["module_code"].as_str(),
        inputs["function_body"].as_str(),
        inputs.get("crate_settings").map(|x| x.as_str()),
    )
    .build_and_check_for_diagnostics(db)
    .get_diagnostics();

    let error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([("expected_diagnostics".into(), diagnostics)]),
        error,
    }
}

pub fn test_function_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SemanticDatabaseForTesting::default();

    let diagnostics = TestFunction::builder(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
        inputs.get("crate_settings").map(|x| x.as_str()),
    )
    .build_and_check_for_diagnostics(db)
    .get_diagnostics();

    let error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([("expected_diagnostics".into(), diagnostics)]),
        error,
    }
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

/// Gets the diagnostics for all the modules (including nested) in the given module.
fn get_recursive_module_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> Diagnostics<SemanticDiagnostic> {
    let mut diagnostics: DiagnosticsBuilder<_> =
        db.module_semantic_diagnostics(module_id).unwrap().into();
    for submodule_id in db.module_submodules_ids(module_id).unwrap().iter() {
        if is_submodule_inline(db, *submodule_id) {
            diagnostics.extend(get_recursive_module_semantic_diagnostics(
                db,
                ModuleId::Submodule(*submodule_id),
            ));
        }
    }
    diagnostics.build()
}

/// Returns true if the given submodule is inline (i.e. has a body), false otherwise.
fn is_submodule_inline(db: &dyn SemanticGroup, submodule: SubmoduleId) -> bool {
    let SubmoduleLongId(_, ptr) = submodule.lookup_intern(db);
    match ptr.lookup(db.upcast()).body(db.upcast()) {
        ast::MaybeModuleBody::Some(_) => true,
        ast::MaybeModuleBody::None(_) => false,
    }
}
