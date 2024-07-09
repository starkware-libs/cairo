use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, CrateConfiguration, CrateSettings, FilesGroup, FilesGroupEx,
    CORELIB_CRATE_NAME,
};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, Directory};
use cairo_lang_utils::{Intern, LookupIntern};
use smol_str::SmolStr;

use crate::lang::db::AnalysisDatabase;

/// A complete set of information needed to set up a real crate in the analysis database.
#[derive(Debug, PartialEq, Eq)]
pub struct Crate {
    /// Crate name.
    pub name: SmolStr,

    /// The root directory of the crate.
    ///
    /// This path **must** be absolute,
    /// so it can be safely used as a `FileId` in the analysis database.
    pub root: PathBuf,

    /// Custom stem of crate main file name, if it is not `lib.cairo`.
    ///
    /// This is used to generate a virtual lib file for crates without a root `lib.cairo`.
    pub custom_main_file_stem: Option<SmolStr>,

    /// Crate settings.
    pub settings: CrateSettings,
}

impl Crate {
    /// Applies this crate to the [`AnalysisDatabase`].
    pub fn apply(&self, db: &mut AnalysisDatabase) {
        let crate_id = CrateLongId::Real(self.name.clone()).intern(db);

        let crate_configuration = CrateConfiguration {
            root: Directory::Real(self.root.clone()),
            settings: self.settings.clone(),
        };
        db.set_crate_config(crate_id, Some(crate_configuration));

        if let Some(file_stem) = &self.custom_main_file_stem {
            inject_virtual_wrapper_lib(db, crate_id, file_stem);
        }
    }

    /// Construct a [`Crate`] from data already applied to the [`AnalysisDatabase`].
    ///
    /// Returns `None` if the crate is virtual or the crate configuration is missing.
    pub fn reconstruct(db: &AnalysisDatabase, crate_id: CrateId) -> Option<Self> {
        let CrateLongId::Real(name) = crate_id.lookup_intern(db) else {
            return None;
        };

        let Some(CrateConfiguration { root: Directory::Real(root), settings }) =
            db.crate_config(crate_id)
        else {
            return None;
        };

        let custom_main_file_stem = extract_custom_file_stem(db, crate_id);

        Some(Self { name, root, custom_main_file_stem, settings })
    }

    /// States whether this is the `core` crate.
    pub fn is_core(&self) -> bool {
        self.name == CORELIB_CRATE_NAME
    }

    /// Returns the path to the main file of this crate.
    pub fn source_path(&self) -> PathBuf {
        self.root.join(match &self.custom_main_file_stem {
            Some(stem) => format!("{stem}.cairo"),
            None => "lib.cairo".into(),
        })
    }
}

/// Generate a wrapper lib file for a compilation unit without a root `lib.cairo`.
///
/// This approach allows compiling crates that do not define `lib.cairo` file. For example, single
/// file crates can be created this way. The actual single file module is defined as `mod` item in
/// created lib file.
fn inject_virtual_wrapper_lib(db: &mut AnalysisDatabase, crate_id: CrateId, file_stem: &str) {
    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_main_file(module_id).unwrap();
    // Inject virtual lib file wrapper.
    db.as_files_group_mut()
        .override_file_content(file_id, Some(Arc::new(format!("mod {file_stem};"))));
}

/// The inverse of [`inject_virtual_wrapper_lib`],
/// tries to infer root module name from crate if it does not have real `lib.cairo`.
fn extract_custom_file_stem(db: &AnalysisDatabase, crate_id: CrateId) -> Option<SmolStr> {
    let CrateConfiguration { root: Directory::Real(root), .. } = db.crate_config(crate_id)? else {
        return None;
    };

    if root.join("lib.cairo").exists() {
        return None;
    }

    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_main_file(module_id).ok()?;
    let content = db.file_content(file_id)?;

    let name = content.strip_prefix("mod ")?.strip_suffix(';')?;
    Some(name.into())
}
