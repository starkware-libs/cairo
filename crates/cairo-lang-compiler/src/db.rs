use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::db::{
    init_dev_corelib, init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx,
    CORELIB_CRATE_NAME,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, Directory};
use cairo_lang_lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::corelib::get_core_ty_by_name;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_sierra_generator::db::SierraGenDatabase;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::project::ProjectError;

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
impl RootDatabase {
    pub fn new(plugins: Vec<Arc<dyn SemanticPlugin>>) -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res);
        res.set_semantic_plugins(plugins);
        res
    }

    pub fn empty() -> Self {
        Self::new(Vec::new())
    }

    pub fn builder() -> RootDatabaseBuilder {
        RootDatabaseBuilder::default()
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        // TODO(spapini): Consider taking from config.
        Self::new(get_default_plugins())
    }
}

#[derive(Default)]
pub struct RootDatabaseBuilder {
    db: RootDatabase,
    main_crate_ids: Option<Vec<CrateId>>,
}

impl RootDatabaseBuilder {
    pub fn new(db: RootDatabase) -> Self {
        Self { db, main_crate_ids: None }
    }

    pub fn empty() -> Self {
        Self::new(RootDatabase::empty())
    }

    pub fn with_plugins(&mut self, plugins: Vec<Arc<dyn SemanticPlugin>>) -> &mut Self {
        self.db.set_semantic_plugins(plugins);
        self
    }

    pub fn with_dev_corelib(&mut self) -> Option<&mut Self> {
        if let Some(path) = detect_corelib() {
            init_dev_corelib(&mut self.db, path);
            Some(self)
        } else {
            None
        }
    }

    pub fn get_main_crate_ids(&self) -> Option<Vec<CrateId>> {
        self.main_crate_ids.clone()
    }

    pub fn with_project_config(&mut self, config: ProjectConfig) -> &mut Self {
        // Updates the crate roots from a ProjectConfig object.
        for (crate_name, directory_path) in config.content.crate_roots {
            let crate_id = self.db.intern_crate(CrateLongId(crate_name));
            let mut path = PathBuf::from(&directory_path);
            if path.is_relative() {
                path = PathBuf::from(&config.base_path).join(path);
            }
            let root = Directory(path);
            self.db.set_crate_root(crate_id, Some(root));
        }

        // Set corelib if defined in project config.
        if let Some(corelib) = config.corelib {
            let core_crate = self.db.intern_crate(CrateLongId(CORELIB_CRATE_NAME.into()));
            self.db.set_crate_root(core_crate, Some(corelib));
        }

        self
    }

    /// Setup to 'db' to compile the file at the given path.
    /// Returns the id of the generated crate.
    fn setup_project_from_single_file(
        &mut self,
        path: &Path,
    ) -> Result<Vec<CrateId>, ProjectError> {
        let bad_path_err = || ProjectError::BadPath { path: path.to_string_lossy().to_string() };
        let file_stem = path.file_stem().and_then(OsStr::to_str).ok_or_else(bad_path_err)?;

        // Create a fake lib file.
        let crate_id = self.db.intern_crate(CrateLongId(file_stem.into()));
        self.db.set_crate_root(crate_id, Some(Directory(path.parent().unwrap().to_path_buf())));

        let module_id = ModuleId::CrateRoot(crate_id);
        let file_id = self.db.module_main_file(module_id).unwrap();
        self.db
            .as_files_group_mut()
            .override_file_content(file_id, Some(Arc::new(format!("mod {};", file_stem))));
        Ok(vec![crate_id])
    }

    fn setup_project_from_directory(&mut self, path: &Path) -> Result<Vec<CrateId>, ProjectError> {
        match ProjectConfig::from_directory(path) {
            Ok(config) => {
                self.with_project_config(config.clone());
                Ok(config
                    .content
                    .crate_roots
                    .keys()
                    .map(|crate_id| self.db.intern_crate(CrateLongId(crate_id.clone())))
                    .collect())
            }
            _ => Err(ProjectError::LoadProjectError),
        }
    }

    fn parse_project_path(path: &Path) -> Result<&Path, ProjectError> {
        if !path.exists() {
            return Err(ProjectError::NoSuchFile { path: path.to_string_lossy().to_string() });
        }
        if path.is_dir() {
            return Ok(path);
        }
        if Some("cairo") != path.extension().and_then(OsStr::to_str) {
            return Err(ProjectError::BadFileExtension);
        }

        let bad_path_err = || ProjectError::BadPath { path: path.to_string_lossy().to_string() };
        let file_stem = path.file_stem().and_then(OsStr::to_str).ok_or_else(bad_path_err)?;
        if file_stem == "lib" { Ok(path.parent().ok_or_else(bad_path_err)?) } else { Ok(path) }
    }

    /// Setup the 'db' to compile the project in the given path.
    /// The path can be either a directory with cairo project file or a .cairo file.
    /// Returns the ids of the project crates.
    pub fn with_project_setup(&mut self, path: &Path) -> Result<&mut Self, ProjectError> {
        let path = Self::parse_project_path(path)?;
        let crate_ids = if path.is_dir() {
            self.setup_project_from_directory(path)?
        } else {
            self.setup_project_from_single_file(path)?
        };
        self.main_crate_ids = Some(crate_ids);
        Ok(self)
    }

    pub fn with_implicit_precedence(&mut self, precedence: Vec<&str>) -> &mut Self {
        self.db.set_implicit_precedence(Arc::new(
            precedence
                .iter()
                .map(|name| get_core_ty_by_name(&self.db, name.into(), vec![]))
                .collect::<Vec<_>>(),
        ));
        self
    }

    pub fn build(self) -> RootDatabase {
        self.db
    }
}

impl AsFilesGroupMut for RootDatabase {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
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
    fn upcast(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn semantic::db::SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn lowering::db::LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for RootDatabase {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
