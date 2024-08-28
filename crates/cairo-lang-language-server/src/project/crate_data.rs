use std::path::PathBuf;

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

    /// Custom stems of crate main files, if it is not `lib.cairo`.
    ///
    /// This is used to generate a virtual lib file for crates without a root `lib.cairo`.
    pub custom_main_file_stems: Option<Vec<SmolStr>>,

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

        if let Some(file_stems) = &self.custom_main_file_stems {
            inject_virtual_wrapper_lib(db, crate_id, file_stems);
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

        let custom_main_file_stems = extract_custom_file_stems(db, crate_id);

        Some(Self { name, root, custom_main_file_stems, settings })
    }

    /// States whether this is the `core` crate.
    pub fn is_core(&self) -> bool {
        self.name == CORELIB_CRATE_NAME
    }

    /// Returns paths to the main files of this crate.
    pub fn source_paths(&self) -> Vec<PathBuf> {
        let source_paths = match &self.custom_main_file_stems {
            Some(stems) => stems.iter().map(|stem| format!("{stem}.cairo")).collect(),
            None => vec!["lib.cairo".into()],
        };

        source_paths.into_iter().map(|filename| self.root.join(filename)).collect()
    }
}

/// Generate a wrapper lib file for a compilation unit without a root `lib.cairo`.
///
/// This approach allows compiling crates that do not define `lib.cairo` file. For example, single
/// file crates can be created this way. The actual single file module is defined as `mod` item in
/// created lib file.
fn inject_virtual_wrapper_lib(
    db: &mut AnalysisDatabase,
    crate_id: CrateId,
    file_stems: &[SmolStr],
) {
    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_main_file(module_id).unwrap();

    let file_content =
        file_stems.iter().map(|stem| format!("mod {stem};")).collect::<Vec<_>>().join("\n");

    // Inject virtual lib file wrapper.
    db.as_files_group_mut().override_file_content(file_id, Some(file_content.into()));
}

/// The inverse of [`inject_virtual_wrapper_lib`],
/// tries to infer root module name from crate if it does not have real `lib.cairo`.
fn extract_custom_file_stems(db: &AnalysisDatabase, crate_id: CrateId) -> Option<Vec<SmolStr>> {
    let CrateConfiguration { root: Directory::Real(root), .. } = db.crate_config(crate_id)? else {
        return None;
    };

    if root.join("lib.cairo").exists() {
        return None;
    }

    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_main_file(module_id).ok()?;
    let content = db.file_content(file_id)?;

    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| Some(line.strip_prefix("mod ")?.strip_suffix(';')?.into()))
        .collect::<Option<Vec<_>>>()
}
