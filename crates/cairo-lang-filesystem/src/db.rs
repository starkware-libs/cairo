use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;
use serde::{Deserialize, Serialize};

use crate::cfg::CfgSet;
use crate::flag::Flag;
use crate::ids::{
    CrateId, CrateLongId, Directory, FileId, FileLongId, FlagId, FlagLongId, VirtualFile,
};
use crate::span::{FileSummary, TextOffset, TextSpan, TextWidth};

#[cfg(test)]
#[path = "db_test.rs"]
mod test;

pub const CORELIB_CRATE_NAME: &str = "core";

/// A configuration per crate.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CrateConfiguration {
    /// The root directory of the crate.
    pub root: Directory,
    pub settings: CrateSettings,
}
impl CrateConfiguration {
    /// Returns a new configuration.
    pub fn default_for_root(root: Directory) -> Self {
        Self { root, settings: CrateSettings::default() }
    }
}

/// Same as `CrateConfiguration` but without the root directory..
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct CrateSettings {
    /// The crate's Cairo edition.
    pub edition: Edition,

    pub cfg_set: Option<CfgSet>,

    #[serde(default)]
    pub experimental_features: ExperimentalFeaturesConfig,
}

/// The Cairo edition of a crate.
/// Editions are a mechanism to allow breaking changes in the compiler.
/// Compiler minor version updates will always support all editions supported by the previous
/// updates with the same major version. Compiler major version updates may remove support for older
/// editions. Editions may be added to provide features that are not backwards compatible, while
/// allowing user to opt-in to them, and be ready for later compiler updates.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum Edition {
    /// The base edition, dated for the first release of the compiler.
    #[default]
    #[serde(rename = "2023_01")]
    V2023_01,
    #[serde(rename = "2023_10")]
    V2023_10,
    #[serde(rename = "2023_11")]
    V2023_11,
}
impl Edition {
    /// Returns the latest stable edition.
    ///
    /// This Cairo edition is recommended for use in new projects and, in case of existing projects,
    /// to migrate to when possible.
    pub const fn latest() -> Self {
        Self::V2023_11
    }

    /// The name of the prelude submodule of `core::prelude` for this compatibility version.
    pub fn prelude_submodule_name(&self) -> &str {
        match self {
            Self::V2023_01 => "v2023_01",
            Self::V2023_10 | Self::V2023_11 => "v2023_10",
        }
    }

    /// Whether to ignore visibility modifiers.
    pub fn ignore_visibility(&self) -> bool {
        match self {
            Self::V2023_01 | Self::V2023_10 => true,
            Self::V2023_11 => false,
        }
    }
}

/// Configuration per crate.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExperimentalFeaturesConfig {
    pub negative_impls: bool,
}

// Salsa database interface.
#[salsa::query_group(FilesDatabase)]
pub trait FilesGroup {
    #[salsa::interned]
    fn intern_crate(&self, crt: CrateLongId) -> CrateId;
    #[salsa::interned]
    fn intern_file(&self, file: FileLongId) -> FileId;
    #[salsa::interned]
    fn intern_flag(&self, flag: FlagLongId) -> FlagId;

    /// Main input of the project. Lists all the crates configurations.
    #[salsa::input]
    fn crate_configs(&self) -> Arc<OrderedHashMap<CrateId, CrateConfiguration>>;

    /// Overrides for file content. Mostly used by language server and tests.
    /// TODO(spapini): Currently, when this input changes, all the file_content() queries will
    /// be invalidated.
    /// Change this mechanism to hold file_overrides on the db struct outside salsa mechanism,
    /// and invalidate manually.
    #[salsa::input]
    fn file_overrides(&self) -> Arc<OrderedHashMap<FileId, Arc<String>>>;

    // TODO(yuval): consider moving this to a separate crate, or rename this crate.
    /// The compilation flags.
    #[salsa::input]
    fn flags(&self) -> Arc<OrderedHashMap<FlagId, Arc<Flag>>>;
    /// The `#[cfg(...)]` options.
    #[salsa::input]
    fn cfg_set(&self) -> Arc<CfgSet>;

    /// List of crates in the project.
    fn crates(&self) -> Vec<CrateId>;
    /// Configuration of the crate.
    fn crate_config(&self, crate_id: CrateId) -> Option<CrateConfiguration>;

    /// Query for raw file contents. Private.
    fn priv_raw_file_content(&self, file_id: FileId) -> Option<Arc<String>>;
    /// Query for the file contents. This takes overrides into consideration.
    fn file_content(&self, file_id: FileId) -> Option<Arc<String>>;
    fn file_summary(&self, file_id: FileId) -> Option<Arc<FileSummary>>;

    /// Query to get a compilation flag by its ID.
    fn get_flag(&self, id: FlagId) -> Option<Arc<Flag>>;
}

pub fn init_files_group(db: &mut (dyn FilesGroup + 'static)) {
    // Initialize inputs.
    db.set_file_overrides(Arc::new(OrderedHashMap::default()));
    db.set_crate_configs(Arc::new(OrderedHashMap::default()));
    db.set_flags(Arc::new(OrderedHashMap::default()));
    db.set_cfg_set(Arc::new(CfgSet::new()));
}

pub fn init_dev_corelib(db: &mut (dyn FilesGroup + 'static), core_lib_dir: PathBuf) {
    let core_crate = db.intern_crate(CrateLongId::Real(CORELIB_CRATE_NAME.into()));
    db.set_crate_config(
        core_crate,
        Some(CrateConfiguration {
            root: Directory::Real(core_lib_dir),
            settings: CrateSettings {
                edition: Edition::V2023_11,
                cfg_set: Default::default(),
                experimental_features: ExperimentalFeaturesConfig { negative_impls: true },
            },
        }),
    );
}

impl AsFilesGroupMut for dyn FilesGroup {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}

pub trait FilesGroupEx: Upcast<dyn FilesGroup> + AsFilesGroupMut {
    /// Overrides file content. None value removes the override.
    fn override_file_content(&mut self, file: FileId, content: Option<Arc<String>>) {
        let mut overrides = Upcast::upcast(self).file_overrides().as_ref().clone();
        match content {
            Some(content) => overrides.insert(file, content),
            None => overrides.swap_remove(&file),
        };
        self.as_files_group_mut().set_file_overrides(Arc::new(overrides));
    }
    /// Sets the root directory of the crate. None value removes the crate.
    fn set_crate_config(&mut self, crt: CrateId, root: Option<CrateConfiguration>) {
        let mut crate_configs = Upcast::upcast(self).crate_configs().as_ref().clone();
        match root {
            Some(root) => crate_configs.insert(crt, root),
            None => crate_configs.swap_remove(&crt),
        };
        self.as_files_group_mut().set_crate_configs(Arc::new(crate_configs));
    }
    /// Sets the given flag value. None value removes the flag.
    fn set_flag(&mut self, id: FlagId, value: Option<Arc<Flag>>) {
        let mut flags = Upcast::upcast(self).flags().as_ref().clone();
        match value {
            Some(value) => flags.insert(id, value),
            None => flags.swap_remove(&id),
        };
        self.as_files_group_mut().set_flags(Arc::new(flags));
    }
    /// Merges specified [`CfgSet`] into one already stored in this db.
    fn use_cfg(&mut self, cfg_set: &CfgSet) {
        let existing = Upcast::upcast(self).cfg_set();
        let merged = existing.union(cfg_set);
        self.as_files_group_mut().set_cfg_set(Arc::new(merged));
    }
}
impl<T: Upcast<dyn FilesGroup> + AsFilesGroupMut + ?Sized> FilesGroupEx for T {}

pub trait AsFilesGroupMut {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static);
}

fn crates(db: &dyn FilesGroup) -> Vec<CrateId> {
    // TODO(spapini): Sort for stability.
    db.crate_configs().keys().copied().collect()
}
fn crate_config(db: &dyn FilesGroup, crt: CrateId) -> Option<CrateConfiguration> {
    match db.lookup_intern_crate(crt) {
        CrateLongId::Real(_) => db.crate_configs().get(&crt).cloned(),
        CrateLongId::Virtual { name: _, config } => Some(config),
    }
}

fn priv_raw_file_content(db: &dyn FilesGroup, file: FileId) -> Option<Arc<String>> {
    match db.lookup_intern_file(file) {
        FileLongId::OnDisk(path) => match fs::read_to_string(path) {
            Ok(content) => Some(Arc::new(content)),
            Err(_) => None,
        },
        FileLongId::Virtual(virt) => Some(virt.content),
    }
}
fn file_content(db: &dyn FilesGroup, file: FileId) -> Option<Arc<String>> {
    let overrides = db.file_overrides();
    overrides.get(&file).cloned().or_else(|| db.priv_raw_file_content(file))
}
fn file_summary(db: &dyn FilesGroup, file: FileId) -> Option<Arc<FileSummary>> {
    let content = db.file_content(file)?;
    let mut line_offsets = vec![TextOffset::default()];
    let mut offset = TextOffset::default();
    for ch in content.chars() {
        offset = offset.add_width(TextWidth::from_char(ch));
        if ch == '\n' {
            line_offsets.push(offset);
        }
    }
    Some(Arc::new(FileSummary { line_offsets, last_offset: offset }))
}
fn get_flag(db: &dyn FilesGroup, id: FlagId) -> Option<Arc<Flag>> {
    db.flags().get(&id).cloned()
}

/// Returns the location of the originating user code.
pub fn get_originating_location(
    db: &dyn FilesGroup,
    mut file_id: FileId,
    mut span: TextSpan,
) -> (FileId, TextSpan) {
    while let FileLongId::Virtual(VirtualFile { parent: Some(parent), code_mappings, .. }) =
        db.lookup_intern_file(file_id)
    {
        if let Some(origin) = code_mappings.iter().find_map(|mapping| mapping.translate(span)) {
            span = origin;
            file_id = parent;
        } else {
            break;
        }
    }
    (file_id, span)
}
