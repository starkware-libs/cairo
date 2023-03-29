use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::Upcast;

use crate::cfg::CfgSet;
use crate::flag::Flag;
use crate::ids::{CrateId, CrateLongId, Directory, FileId, FileLongId, FlagId, FlagLongId};
use crate::span::{FileSummary, TextOffset, TextWidth};

#[cfg(test)]
#[path = "db_test.rs"]
mod test;

pub const CORELIB_CRATE_NAME: &str = "core";

// Salsa database interface.
#[salsa::query_group(FilesDatabase)]
pub trait FilesGroup {
    #[salsa::interned]
    fn intern_crate(&self, crt: CrateLongId) -> CrateId;
    #[salsa::interned]
    fn intern_file(&self, file: FileLongId) -> FileId;
    #[salsa::interned]
    fn intern_flag(&self, flag: FlagLongId) -> FlagId;

    /// Main input of the project. Lists all the crates.
    #[salsa::input]
    fn crate_roots(&self) -> Arc<HashMap<CrateId, Directory>>;

    /// Overrides for file content. Mostly used by language server and tests.
    /// TODO(spapini): Currently, when this input changes, all the file_content() queries will
    /// be invalidated.
    /// Change this mechanism to hold file_overrides on the db struct outside salsa mechanism,
    /// and invalidate manually.
    #[salsa::input]
    fn file_overrides(&self) -> Arc<HashMap<FileId, Arc<String>>>;

    // TODO(yuval): consider moving this to a separate crate, or rename this crate.
    /// The compilation flags.
    #[salsa::input]
    fn flags(&self) -> Arc<HashMap<FlagId, Arc<Flag>>>;
    /// The `#[cfg(...)]` options.
    #[salsa::input]
    fn cfg_set(&self) -> Arc<CfgSet>;

    /// List of crates in the project.
    fn crates(&self) -> Vec<CrateId>;
    /// Root directory of the crate.
    fn crate_root_dir(&self, crate_id: CrateId) -> Option<Directory>;

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
    db.set_file_overrides(Arc::new(HashMap::new()));
    db.set_crate_roots(Arc::new(HashMap::new()));
    db.set_flags(Arc::new(HashMap::new()));
    db.set_cfg_set(Arc::new(CfgSet::new()));
}

pub fn init_dev_corelib(db: &mut (dyn FilesGroup + 'static), path: PathBuf) {
    let core_crate = db.intern_crate(CrateLongId(CORELIB_CRATE_NAME.into()));
    let core_root_dir = Directory(path);
    db.set_crate_root(core_crate, Some(core_root_dir));
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
            None => overrides.remove(&file),
        };
        self.as_files_group_mut().set_file_overrides(Arc::new(overrides));
    }
    /// Sets the root directory of the crate. None value removes the crate.
    fn set_crate_root(&mut self, crt: CrateId, root: Option<Directory>) {
        let mut crate_roots = Upcast::upcast(self).crate_roots().as_ref().clone();
        match root {
            Some(root) => crate_roots.insert(crt, root),
            None => crate_roots.remove(&crt),
        };
        self.as_files_group_mut().set_crate_roots(Arc::new(crate_roots));
    }
    /// Sets the given flag value. None value removes the flag.
    fn set_flag(&mut self, id: FlagId, value: Option<Arc<Flag>>) {
        let mut flags = Upcast::upcast(self).flags().as_ref().clone();
        match value {
            Some(value) => flags.insert(id, value),
            None => flags.remove(&id),
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
    db.crate_roots().keys().copied().collect()
}
fn crate_root_dir(db: &dyn FilesGroup, crt: CrateId) -> Option<Directory> {
    db.crate_roots().get(&crt).cloned()
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
