use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, CrateConfiguration, CrateSettings, FilesGroupEx, CORELIB_CRATE_NAME,
};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, Directory};
use cairo_lang_utils::Intern;
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

    /// States whether this is the `core` crate.
    pub fn is_core(&self) -> bool {
        self.name == CORELIB_CRATE_NAME
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
