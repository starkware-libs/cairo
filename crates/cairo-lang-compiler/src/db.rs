use std::sync::Arc;

use anyhow::{Result, anyhow, bail};
use cairo_lang_defs::db::{DefsDatabase, DefsGroup, init_defs_group, try_ext_as_virtual_impl};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::{
    CORELIB_VERSION, ExternalFiles, FilesDatabase, FilesGroup, FilesGroupEx, init_dev_corelib,
    init_files_group,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::{CrateId, FlagId, VirtualFile};
use cairo_lang_lowering::db::{
    ExternalCodeSizeEstimator, LoweringDatabase, LoweringGroup, init_lowering_group,
};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_project::ProjectConfig;
use cairo_lang_runnable_utils::builder::RunnableBuilder;
use cairo_lang_semantic::db::{
    PluginSuiteInput, SemanticDatabase, SemanticGroup, init_semantic_group,
};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_sierra_generator::db::{SierraGenDatabase, SierraGenGroup};
use cairo_lang_sierra_generator::program_generator::get_dummy_program_for_size_estimation;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;

use crate::InliningStrategy;
use crate::project::update_crate_roots_from_project_config;

impl ExternalCodeSizeEstimator for RootDatabase {
    /// Estimates the size of a function by compiling it to CASM.
    /// Note that the size is not accurate since we don't use the real costs for the dummy
    /// functions.
    fn estimate_size(&self, function_id: ConcreteFunctionWithBodyId) -> Maybe<isize> {
        let program = get_dummy_program_for_size_estimation(self, function_id)?;

        // All the functions except the first one are dummy functions.
        let n_dummy_functions = program.funcs.len() - 1;

        // TODO(ilya): Consider adding set costs to dummy functions.
        let builder = match RunnableBuilder::new(program, Default::default()) {
            Ok(builder) => builder,
            Err(err) => {
                if err.is_ap_overflow_error() {
                    // If the compilation failed due to an AP overflow, we don't want to panic as it
                    // can happen for valid code. In this case, the function is
                    // probably too large for inline so we can just return the max size.
                    return Ok(isize::MAX);
                }

                panic!("Failed to compile program to casm.");
            }
        };
        let casm = builder.casm_program();
        let total_size = casm.instructions.iter().map(|inst| inst.body.op_size()).sum::<usize>();

        // The size of a dummy function is currently 3 felts. call (2) + ret (1).
        const DUMMY_FUNCTION_SIZE: usize = 3;
        Ok((total_size - (n_dummy_functions * DUMMY_FUNCTION_SIZE)).try_into().unwrap_or(0))
    }
}

#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    SyntaxDatabase
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}
impl salsa::Database for RootDatabase {}
impl ExternalFiles for RootDatabase {
    fn try_ext_as_virtual(&self, external_id: salsa::InternId) -> Option<VirtualFile> {
        try_ext_as_virtual_impl(self, external_id)
    }
}
impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<RootDatabase> {
        salsa::Snapshot::new(RootDatabase { storage: self.storage.snapshot() })
    }
}
impl RootDatabase {
    fn new(default_plugin_suite: PluginSuite, inlining_strategy: InliningStrategy) -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res, inlining_strategy);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        let suite = res.intern_plugin_suite(default_plugin_suite);
        res.set_default_plugins_from_suite(suite);

        res
    }

    pub fn empty() -> Self {
        Self::builder().clear_plugins().build().unwrap()
    }

    pub fn builder() -> RootDatabaseBuilder {
        RootDatabaseBuilder::new()
    }

    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> RootDatabase {
        RootDatabase { storage: self.storage.snapshot() }
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::builder().build().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct RootDatabaseBuilder {
    default_plugin_suite: PluginSuite,
    detect_corelib: bool,
    auto_withdraw_gas: bool,
    panic_backtrace: bool,
    unsafe_panic: bool,
    project_config: Option<Box<ProjectConfig>>,
    cfg_set: Option<CfgSet>,
    inlining_strategy: InliningStrategy,
}

impl RootDatabaseBuilder {
    fn new() -> Self {
        Self {
            default_plugin_suite: get_default_plugin_suite(),
            detect_corelib: false,
            auto_withdraw_gas: true,
            panic_backtrace: false,
            unsafe_panic: false,
            project_config: None,
            cfg_set: None,
            inlining_strategy: InliningStrategy::Default,
        }
    }

    pub fn with_default_plugin_suite(&mut self, suite: PluginSuite) -> &mut Self {
        self.default_plugin_suite.add(suite);
        self
    }

    pub fn clear_plugins(&mut self) -> &mut Self {
        self.default_plugin_suite = get_default_plugin_suite();
        self
    }

    pub fn with_inlining_strategy(&mut self, inlining_strategy: InliningStrategy) -> &mut Self {
        self.inlining_strategy = inlining_strategy;
        self
    }

    pub fn detect_corelib(&mut self) -> &mut Self {
        self.detect_corelib = true;
        self
    }

    pub fn with_project_config(&mut self, config: ProjectConfig) -> &mut Self {
        self.project_config = Some(Box::new(config));
        self
    }

    pub fn with_cfg(&mut self, cfg_set: impl Into<CfgSet>) -> &mut Self {
        self.cfg_set = Some(cfg_set.into());
        self
    }

    pub fn skip_auto_withdraw_gas(&mut self) -> &mut Self {
        self.auto_withdraw_gas = false;
        self
    }

    pub fn with_panic_backtrace(&mut self) -> &mut Self {
        self.panic_backtrace = true;
        self
    }

    pub fn with_unsafe_panic(&mut self) -> &mut Self {
        self.unsafe_panic = true;
        self
    }

    pub fn build(&mut self) -> Result<RootDatabase> {
        // NOTE: Order of operations matters here!
        //   Errors if something is not OK are very subtle, mostly this results in missing
        //   identifier diagnostics, or panics regarding lack of corelib items.

        let mut db = RootDatabase::new(self.default_plugin_suite.clone(), self.inlining_strategy);

        if let Some(cfg_set) = &self.cfg_set {
            db.use_cfg(cfg_set);
        }

        if self.detect_corelib {
            let path =
                detect_corelib().ok_or_else(|| anyhow!("Failed to find development corelib."))?;
            init_dev_corelib(&mut db, path)
        }

        let add_withdraw_gas_flag_id = FlagId::new(&db, "add_withdraw_gas");
        db.set_flag(
            add_withdraw_gas_flag_id,
            Some(Arc::new(Flag::AddWithdrawGas(self.auto_withdraw_gas))),
        );
        let panic_backtrace_flag_id = FlagId::new(&db, "panic_backtrace");
        db.set_flag(
            panic_backtrace_flag_id,
            Some(Arc::new(Flag::PanicBacktrace(self.panic_backtrace))),
        );

        let unsafe_panic_flag_id = FlagId::new(&db, "unsafe_panic");
        db.set_flag(unsafe_panic_flag_id, Some(Arc::new(Flag::UnsafePanic(self.unsafe_panic))));

        if let Some(config) = &self.project_config {
            update_crate_roots_from_project_config(&mut db, config.as_ref());
        }
        validate_corelib(&db)?;

        Ok(db)
    }
}

/// Validates that the corelib version matches the expected one.
pub fn validate_corelib(db: &(dyn FilesGroup + 'static)) -> Result<()> {
    let Some(config) = db.crate_config(CrateId::core(db)) else {
        return Ok(());
    };
    let Some(found) = config.settings.version else {
        return Ok(());
    };
    let Ok(expected) = semver::Version::parse(CORELIB_VERSION) else {
        return Ok(());
    };
    if found == expected {
        return Ok(());
    }
    let path_part = match config.root {
        cairo_lang_filesystem::ids::Directory::Real(path) => {
            format!(" for `{}`", path.to_string_lossy())
        }
        cairo_lang_filesystem::ids::Directory::Virtual { .. } => "".to_string(),
    };
    bail!("Corelib version mismatch: expected `{expected}`, found `{found}`{path_part}.");
}

impl Upcast<dyn FilesGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}
impl Upcast<dyn SierraGenGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SierraGenGroup + 'static) {
        self
    }
}
impl Upcast<dyn ParserGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn ParserGroup + 'static) {
        self
    }
}
