use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::{Database, Setter};
use semver::Version;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::cfg::CfgSet;
use crate::flag::Flag;
use crate::ids::{
    ArcStr, BlobId, BlobLongId, CodeMapping, CodeOrigin, CrateId, CrateInput, CrateLongId,
    Directory, DirectoryInput, FileId, FileInput, FileLongId, FlagId, FlagLongId, SmolStrId,
    Tracked, VirtualFile,
};
use crate::span::{FileSummary, TextOffset, TextSpan, TextWidth};

#[cfg(test)]
#[path = "db_test.rs"]
mod test;

pub const CORELIB_CRATE_NAME: &str = "core";
pub const CORELIB_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Unique identifier of a crate.
///
/// This directly translates to [DependencySettings::discriminator] except the discriminator
/// **must** be `None` for the core crate.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct CrateIdentifier(String);

impl<T: ToString> From<T> for CrateIdentifier {
    fn from(value: T) -> Self {
        Self(value.to_string())
    }
}

impl From<CrateIdentifier> for String {
    fn from(value: CrateIdentifier) -> Self {
        value.0
    }
}

/// Same as `CrateConfiguration` but without interning the root directory.
/// This is used to avoid the need to intern the file id inside salsa database inputs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CrateConfigurationInput {
    pub root: DirectoryInput,
    pub settings: CrateSettings,
    pub cache_file: Option<BlobLongId>,
}

impl CrateConfigurationInput {
    /// Converts the input into an [`CrateConfiguration`].
    pub fn into_crate_configuration(self, db: &dyn Database) -> CrateConfiguration<'_> {
        CrateConfiguration {
            root: self.root.into_directory(db),
            settings: self.settings,
            cache_file: self.cache_file.map(|blob_long_id| blob_long_id.intern(db)),
        }
    }
}

/// A configuration per crate.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct CrateConfiguration<'db> {
    /// The root directory of the crate.
    pub root: Directory<'db>,
    pub settings: CrateSettings,
    pub cache_file: Option<BlobId<'db>>,
}
impl<'db> CrateConfiguration<'db> {
    /// Returns a new configuration.
    pub fn default_for_root(root: Directory<'db>) -> Self {
        Self { root, settings: CrateSettings::default(), cache_file: None }
    }

    /// Converts the configuration into an [`CrateConfigurationInput`].
    pub fn into_crate_configuration_input(self, db: &dyn Database) -> CrateConfigurationInput {
        CrateConfigurationInput {
            root: self.root.into_directory_input(db),
            settings: self.settings,
            cache_file: self.cache_file.map(|blob_id| blob_id.long(db).clone()),
        }
    }
}

/// Same as `CrateConfiguration` but without the root directory.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CrateSettings {
    /// The name reflecting how the crate is referred to in the Cairo code e.g. `use crate_name::`.
    /// If set to [`None`] then [`CrateIdentifier`] key will be used as a name.
    pub name: Option<String>,
    /// The crate's Cairo edition.
    pub edition: Edition,
    /// The crate's version.
    ///
    /// ## [CrateSettings::version] vs. [DependencySettings::discriminator]
    ///
    /// Cairo uses semantic versioning for crates.
    /// The version field is an optional piece of metadata that can be attached to a crate
    /// and is used in various lints and can be used as a context in diagnostics.
    ///
    /// On the other hand, the discriminator is a unique identifier that allows including multiple
    /// copies of a crate in a single compilation unit.
    /// It is free-form and never reaches the user.
    pub version: Option<Version>,
    /// The `#[cfg(...)]` configuration.
    pub cfg_set: Option<CfgSet>,
    /// The crate's dependencies.
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencySettings>,

    #[serde(default)]
    pub experimental_features: ExperimentalFeaturesConfig,
}

/// Tracked function to return the default settings for a crate.
/// This is used to initialize the default settings once, and return it by reference.
#[salsa::tracked(returns(ref))]
pub fn default_crate_settings<'db>(_db: &'db dyn Database) -> CrateSettings {
    CrateSettings::default()
}

/// The Cairo edition of a crate.
///
/// Editions are a mechanism to allow breaking changes in the compiler.
/// Compiler minor version updates will always support all editions supported by the previous
/// updates with the same major version. Compiler major version updates may remove support for older
/// editions. Editions may be added to provide features that are not backwards compatible, while
/// allowing user to opt-in to them, and be ready for later compiler updates.
#[derive(
    Clone, Copy, Debug, Default, Hash, PartialEq, Eq, Serialize, Deserialize, salsa::Update,
)]
pub enum Edition {
    /// The base edition, dated for the first release of the compiler.
    #[default]
    #[serde(rename = "2023_01")]
    V2023_01,
    #[serde(rename = "2023_10")]
    V2023_10,
    #[serde(rename = "2023_11")]
    V2023_11,
    #[serde(rename = "2024_07")]
    V2024_07,
}
impl Edition {
    /// Returns the latest stable edition.
    ///
    /// This Cairo edition is recommended for use in new projects and, in case of existing projects,
    /// to migrate to when possible.
    pub const fn latest() -> Self {
        Self::V2024_07
    }

    /// The name of the prelude submodule of `core::prelude` for this compatibility version.
    pub fn prelude_submodule_name<'db>(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        SmolStrId::from(
            db,
            match self {
                Self::V2023_01 => "v2023_01",
                Self::V2023_10 | Self::V2023_11 => "v2023_10",
                Self::V2024_07 => "v2024_07",
            },
        )
    }

    /// Whether to ignore visibility modifiers.
    pub fn ignore_visibility(&self) -> bool {
        match self {
            Self::V2023_01 | Self::V2023_10 => true,
            Self::V2023_11 | Self::V2024_07 => false,
        }
    }
}

/// The settings for a dependency.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DependencySettings {
    /// A unique string allowing identifying different copies of the same dependency
    /// in the compilation unit.
    ///
    /// Usually such copies differ by their versions or sources (or both).
    /// It **must** be [`None`] for the core crate, for other crates it should be directly
    /// translated from their [`CrateIdentifier`].
    pub discriminator: Option<String>,
}

/// Configuration per crate.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExperimentalFeaturesConfig {
    pub negative_impls: bool,
    /// Allows using associated item constraints.
    pub associated_item_constraints: bool,
    /// Allows using coupon types and coupon calls.
    ///
    /// Each function has an associated `Coupon` type, which represents paying the cost of the
    /// function before calling it.
    #[serde(default)]
    pub coupons: bool,
    /// Allows using user defined inline macros.
    #[serde(default)]
    pub user_defined_inline_macros: bool,
}

/// Function to get a virtual file from an external id.
pub type ExtAsVirtual =
    Arc<dyn for<'a> Fn(&'a dyn Database, salsa::Id) -> &'a VirtualFile<'a> + Send + Sync>;

#[salsa::input]
// TODO(eytan-starkware): Change this mechanism to hold input handles on the db struct outside
// salsa mechanism, and invalidate manually.
pub struct FilesGroupInput {
    /// Main input of the project. Lists all the crates configurations.
    #[returns(ref)]
    pub crate_configs: Option<OrderedHashMap<CrateInput, CrateConfigurationInput>>,
    /// Overrides for file content. Mostly used by language server and tests.
    #[returns(ref)]
    pub file_overrides: Option<OrderedHashMap<FileInput, Arc<str>>>,
    // TODO(yuval): consider moving this to a separate crate, or rename this crate.
    /// The compilation flags.
    #[returns(ref)]
    pub flags: Option<OrderedHashMap<FlagLongId, Arc<Flag>>>,
    /// The `#[cfg(...)]` options.
    #[returns(ref)]
    pub cfg_set: Option<CfgSet>,
    #[returns(ref)]
    pub ext_as_virtual_obj: Option<ExtAsVirtual>,
}

#[salsa::tracked]
pub fn files_group_input(db: &dyn Database) -> FilesGroupInput {
    FilesGroupInput::new(db, None, None, None, None, None)
}

/// Queries over the files group.
pub trait FilesGroup: Database {
    /// Interned version of `crate_configs_input`.
    fn crate_configs<'db>(&'db self) -> &'db OrderedHashMap<CrateId<'db>, CrateConfiguration<'db>> {
        crate_configs(self.as_dyn_database())
    }

    /// Interned version of `file_overrides_input`.
    fn file_overrides<'db>(&'db self) -> &'db OrderedHashMap<FileId<'db>, ArcStr> {
        file_overrides(self.as_dyn_database())
    }

    /// Interned version of `flags_input`.
    fn flags<'db>(&'db self) -> &'db OrderedHashMap<FlagId<'db>, Arc<Flag>> {
        flags(self.as_dyn_database())
    }

    /// List of crates in the project.
    fn crates<'db>(&'db self) -> &'db [CrateId<'db>] {
        crates(self.as_dyn_database())
    }

    /// Configuration of the crate.
    fn crate_config<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Option<&'db CrateConfiguration<'db>> {
        crate_config(self.as_dyn_database(), crate_id)
    }

    /// Query for the file contents. This takes overrides into consideration.
    fn file_content<'db>(&'db self, file_id: FileId<'db>) -> Option<&'db str> {
        file_content(self.as_dyn_database(), file_id).as_ref().map(|content| content.as_ref())
    }

    fn file_summary<'db>(&'db self, file_id: FileId<'db>) -> Option<&'db FileSummary> {
        file_summary(self.as_dyn_database(), file_id)
    }

    /// Query for the blob content.
    fn blob_content<'db>(&'db self, blob_id: BlobId<'db>) -> Option<&'db [u8]> {
        blob_content(self.as_dyn_database(), blob_id)
    }
    /// Query to get a compilation flag by its ID.
    fn get_flag<'db>(&'db self, id: FlagId<'db>) -> Option<&'db Flag> {
        get_flag(self.as_dyn_database(), id)
    }

    /// Create an input file from an interned file id.
    fn file_input<'db>(&'db self, file_id: FileId<'db>) -> &'db FileInput {
        file_input(self.as_dyn_database(), file_id)
    }

    /// Create an input crate from an interned crate id.
    fn crate_input<'db>(&'db self, crt: CrateId<'db>) -> &'db CrateInput {
        crate_input(self.as_dyn_database(), crt)
    }

    /// Sets the given flag value. None value removes the flag.
    fn set_flag(&mut self, flag: FlagLongId, value: Option<Arc<Flag>>) {
        let db_ref = self.as_dyn_database();
        let mut flags = files_group_input(db_ref).flags(db_ref).clone().unwrap();
        match value {
            Some(value) => flags.insert(flag, value),
            None => flags.swap_remove(&flag),
        };
        files_group_input(db_ref).set_flags(self).to(Some(flags));
    }

    /// Merges specified [`CfgSet`] into one already stored in this db.
    fn use_cfg(&mut self, cfg_set: &CfgSet) {
        let db_ref = self.as_dyn_database();
        let existing = cfg_set_helper(db_ref);
        let merged = existing.union(cfg_set);
        files_group_input(db_ref).set_cfg_set(self).to(Some(merged));
    }

    /// Returns the cfg set.
    fn cfg_set(&self) -> &CfgSet {
        cfg_set_helper(self.as_dyn_database())
    }
}

impl<T: Database + ?Sized> FilesGroup for T {}

pub fn init_files_group<'db>(db: &mut (dyn Database + 'db)) {
    // Initialize inputs.
    let inp = files_group_input(db);
    inp.set_file_overrides(db).to(Some(Default::default()));
    inp.set_crate_configs(db).to(Some(Default::default()));
    inp.set_flags(db).to(Some(Default::default()));
    inp.set_cfg_set(db).to(Some(Default::default()));
}

pub fn set_crate_configs_input(
    db: &mut dyn Database,
    crate_configs: Option<OrderedHashMap<CrateInput, CrateConfigurationInput>>,
) {
    files_group_input(db).set_crate_configs(db).to(crate_configs);
}

#[salsa::tracked(returns(ref))]
pub fn file_overrides<'db>(db: &'db dyn Database) -> OrderedHashMap<FileId<'db>, ArcStr> {
    let inp = files_group_input(db).file_overrides(db).as_ref().expect("file_overrides is not set");
    inp.iter()
        .map(|(file_id, content)| {
            (file_id.clone().into_file_long_id(db).intern(db), ArcStr::new(content.clone()))
        })
        .collect()
}

#[salsa::tracked(returns(ref))]
pub fn crate_configs<'db>(
    db: &'db dyn Database,
) -> OrderedHashMap<CrateId<'db>, CrateConfiguration<'db>> {
    let inp = files_group_input(db).crate_configs(db).as_ref().expect("crate_configs is not set");
    inp.iter()
        .map(|(crate_input, config)| {
            (
                crate_input.clone().into_crate_long_id(db).intern(db),
                config.clone().into_crate_configuration(db),
            )
        })
        .collect()
}

#[salsa::tracked(returns(ref))]
pub fn flags<'db>(db: &'db dyn Database) -> OrderedHashMap<FlagId<'db>, Arc<Flag>> {
    let inp = files_group_input(db).flags(db).as_ref().expect("flags is not set");
    inp.iter().map(|(flag_id, flag)| (flag_id.clone().intern(db), flag.clone())).collect()
}

#[salsa::tracked(returns(ref))]
fn file_input(db: &dyn Database, file_id: FileId<'_>) -> FileInput {
    file_id.long(db).into_file_input(db)
}

#[salsa::tracked(returns(ref))]
fn crate_input(db: &dyn Database, crt: CrateId<'_>) -> CrateInput {
    crt.long(db).clone().into_crate_input(db)
}

#[salsa::tracked(returns(ref))]
fn crate_configuration_input_helper(
    db: &dyn Database,
    _tracked: Tracked,
    config: CrateConfiguration<'_>,
) -> CrateConfigurationInput {
    config.clone().into_crate_configuration_input(db)
}

fn crate_configuration_input<'db>(
    db: &'db dyn Database,
    config: CrateConfiguration<'db>,
) -> &'db CrateConfigurationInput {
    crate_configuration_input_helper(db, (), config)
}

pub fn init_dev_corelib(db: &mut dyn salsa::Database, core_lib_dir: PathBuf) {
    let core = CrateLongId::core(db).intern(db);
    let root = CrateConfiguration {
        root: Directory::Real(core_lib_dir),
        settings: CrateSettings {
            name: None,
            edition: Edition::V2024_07,
            version: Version::parse(CORELIB_VERSION).ok(),
            cfg_set: Default::default(),
            dependencies: Default::default(),
            experimental_features: ExperimentalFeaturesConfig {
                negative_impls: true,
                associated_item_constraints: true,
                coupons: true,
                user_defined_inline_macros: true,
            },
        },
        cache_file: None,
    };
    let crate_configs = update_crate_configuration_input_helper(db, core, Some(root));
    set_crate_configs_input(db, Some(crate_configs));
}

/// Updates crate configuration input for standalone use.
pub fn update_crate_configuration_input_helper(
    db: &dyn Database,
    crt: CrateId<'_>,
    root: Option<CrateConfiguration<'_>>,
) -> OrderedHashMap<CrateInput, CrateConfigurationInput> {
    let crt = db.crate_input(crt);
    let db_ref: &dyn Database = db;
    let mut crate_configs = files_group_input(db_ref).crate_configs(db_ref).clone().unwrap();
    match root {
        Some(root) => crate_configs.insert(crt.clone(), db.crate_configuration_input(root).clone()),
        None => crate_configs.swap_remove(crt),
    };
    crate_configs
}

/// Sets the root directory of the crate. None value removes the crate.
#[macro_export]
macro_rules! set_crate_config {
    ($self:expr, $crt:expr, $root:expr) => {
        let crate_configs = $crate::db::update_crate_configuration_input_helper($self, $crt, $root);
        $crate::db::set_crate_configs_input($self, Some(crate_configs));
    };
}

/// Updates file overrides input for standalone use.
pub fn update_file_overrides_input_helper(
    db: &dyn Database,
    file: FileInput,
    content: Option<Arc<str>>,
) -> OrderedHashMap<FileInput, Arc<str>> {
    let db_ref: &dyn Database = db;
    let mut overrides = files_group_input(db_ref).file_overrides(db_ref).clone().unwrap();
    match content {
        Some(content) => overrides.insert(file.clone(), content),
        None => overrides.swap_remove(&file),
    };
    overrides
}

/// Overrides file content. None value removes the override.
#[macro_export]
macro_rules! override_file_content {
    ($self:expr, $file:expr, $content:expr) => {
        let file = $self.file_input($file).clone();
        let overrides = $crate::db::update_file_overrides_input_helper($self, file, $content);
        salsa::Setter::to(
            $crate::db::files_group_input($self).set_file_overrides($self),
            Some(overrides),
        );
    };
}

fn cfg_set_helper(db: &dyn Database) -> &CfgSet {
    files_group_input(db).cfg_set(db).as_ref().expect("cfg_set is not set")
}

#[salsa::tracked(returns(ref))]
fn crates<'db>(db: &'db dyn Database) -> Vec<CrateId<'db>> {
    // TODO(spapini): Sort for stability.
    db.crate_configs().keys().copied().collect()
}

/// Tracked function to return the configuration of a crate.
#[salsa::tracked(returns(ref))]
fn crate_config_helper<'db>(
    db: &'db dyn Database,
    crt: CrateId<'db>,
) -> Option<CrateConfiguration<'db>> {
    match crt.long(db) {
        CrateLongId::Real { .. } => db.crate_configs().get(&crt).cloned(),
        CrateLongId::Virtual { name: _, file_id, settings, cache_file } => {
            Some(CrateConfiguration {
                root: Directory::Virtual {
                    files: BTreeMap::from([("lib.cairo".to_string(), *file_id)]),
                    dirs: Default::default(),
                },
                settings: toml::from_str(settings)
                    .expect("Failed to parse virtual crate settings."),
                cache_file: *cache_file,
            })
        }
    }
}

/// Return a reference to the configuration of a crate.
/// This is a wrapper around the tracked function `crate_config_helper` to return a
/// reference to a type unsupported by salsa tracked functions.
fn crate_config<'db>(
    db: &'db dyn Database,
    crt: CrateId<'db>,
) -> Option<&'db CrateConfiguration<'db>> {
    crate_config_helper(db, crt).as_ref()
}

#[salsa::tracked]
fn priv_raw_file_content<'db>(db: &'db dyn Database, file: FileId<'db>) -> Option<SmolStrId<'db>> {
    match file.long(db) {
        FileLongId::OnDisk(path) => {
            // This does not result in performance cost due to OS caching and the fact that salsa
            // will re-execute only this single query if the file content did not change.
            db.report_untracked_read();

            match fs::read_to_string(path) {
                Ok(content) => Some(SmolStrId::new(db, SmolStr::new(content))),
                Err(_) => None,
            }
        }
        FileLongId::Virtual(virt) => Some(virt.content),
        FileLongId::External(external_id) => Some(ext_as_virtual(db, *external_id).content),
    }
}

/// Tracked function to return the content of a file as a string.
#[salsa::tracked(returns(ref))]
fn file_summary_helper<'db>(db: &'db dyn Database, file: FileId<'db>) -> Option<FileSummary> {
    let content = db.file_content(file)?;
    let mut line_offsets = vec![TextOffset::START];
    let mut offset = TextOffset::START;
    for ch in content.chars() {
        offset = offset.add_width(TextWidth::from_char(ch));
        if ch == '\n' {
            line_offsets.push(offset);
        }
    }
    Some(FileSummary { line_offsets, last_offset: offset })
}

/// Query implementation of [FilesGroup::file_content].
#[salsa::tracked(returns(ref))]
fn file_content<'db>(db: &'db dyn Database, file_id: FileId<'db>) -> Option<Arc<str>> {
    let overrides = db.file_overrides();
    overrides.get(&file_id).map(|content| (**content).clone()).or_else(|| {
        priv_raw_file_content(db, file_id).map(|content| content.long(db).clone().into())
    })
}

/// Return a reference to the content of a file as a string.
/// This is a wrapper around the tracked function `file_summary_helper` to return a
/// reference to a type unsupported by salsa tracked functions.
fn file_summary<'db>(db: &'db dyn Database, file: FileId<'db>) -> Option<&'db FileSummary> {
    file_summary_helper(db, file).as_ref()
}

/// Returns a reference to the flag value.
#[salsa::tracked(returns(ref))]
fn get_flag_helper<'db>(db: &'db dyn Database, id: FlagId<'db>) -> Option<Arc<Flag>> {
    db.flags().get(&id).cloned()
}

/// Returns a reference to the flag value.
// TODO(eytan-starkware): Remove helper function and use flags here.
fn get_flag<'db>(db: &'db dyn Database, id: FlagId<'db>) -> Option<&'db Flag> {
    db.flags().get(&id).map(|flag| flag.as_ref())
}

/// Tracked function to return the blob's content.
#[salsa::tracked(returns(ref))]
fn blob_content_helper<'db>(db: &'db dyn Database, blob: BlobId<'db>) -> Option<Vec<u8>> {
    blob.long(db).content()
}

/// Wrapper around the tracked function `blob_content_helper` to return a
/// reference to a type unsupported by salsa tracked functions.
fn blob_content<'db>(db: &'db dyn Database, blob: BlobId<'db>) -> Option<&'db [u8]> {
    blob_content_helper(db, blob).as_ref().map(|content| content.as_slice())
}

/// Returns the location of the originating user code.
pub fn get_originating_location<'db>(
    db: &'db dyn Database,
    mut file_id: FileId<'db>,
    mut span: TextSpan,
    mut parent_files: Option<&mut Vec<FileId<'db>>>,
) -> (FileId<'db>, TextSpan) {
    if let Some(ref mut parent_files) = parent_files {
        parent_files.push(file_id);
    }
    while let Some((parent, code_mappings)) = get_parent_and_mapping(db, file_id) {
        if let Some(origin) = translate_location(code_mappings, span) {
            span = origin;
            file_id = parent;
            if let Some(ref mut parent_files) = parent_files {
                parent_files.push(file_id);
            }
        } else {
            break;
        }
    }
    (file_id, span)
}

/// This function finds a span in original code that corresponds to the provided span in the
/// generated code, using the provided code mappings.
///
/// Code mappings describe a mapping between the original code and the generated one.
/// Each mapping has a resulting span in a generated file and an origin in the original file.
///
/// If any of the provided mappings fully contains the span, origin span of the mapping will be
/// returned. Otherwise, the function will try to find a span that is a result of a concatenation of
/// multiple consecutive mappings.
pub fn translate_location(code_mapping: &[CodeMapping], span: TextSpan) -> Option<TextSpan> {
    // If any of the mappings fully contains the span, return the origin span of the mapping.
    if let Some(containing) = code_mapping.iter().find(|mapping| {
        mapping.span.contains(span) && !matches!(mapping.origin, CodeOrigin::CallSite(_))
    }) {
        // Found a span that fully contains the current one - translates it.
        return containing.translate(span);
    }

    // Find all mappings that have non-empty intersection with the provided span.
    let intersecting_mappings = || {
        code_mapping.iter().filter(|mapping| {
            // Omit mappings to the left or to the right of current span.
            mapping.span.end > span.start && mapping.span.start < span.end
        })
    };

    // Call site can be treated as default origin.
    let call_site = intersecting_mappings()
        .find(|mapping| {
            mapping.span.contains(span) && matches!(mapping.origin, CodeOrigin::CallSite(_))
        })
        .and_then(|containing| containing.translate(span));

    let mut matched = intersecting_mappings()
        .filter(|mapping| matches!(mapping.origin, CodeOrigin::Span(_)))
        .collect::<Vec<_>>();

    // If no mappings intersect with the span, translation is impossible.
    if matched.is_empty() {
        return call_site;
    }

    // Take the first mapping to the left.
    matched.sort_by_key(|mapping| mapping.span);
    let (first, matched) = matched.split_first().expect("non-empty vec always has first element");

    // Find the last mapping which consecutively follows the first one.
    // Note that all spans here intersect with the given one.
    let mut last = first;
    for mapping in matched {
        if mapping.span.start > last.span.end {
            break;
        }

        let mapping_origin =
            mapping.origin.as_span().expect("mappings with start origin should be filtered out");
        let last_origin =
            last.origin.as_span().expect("mappings with start origin should be filtered out");
        // Make sure, the origins are consecutive.
        if mapping_origin.start > last_origin.end {
            break;
        }

        last = mapping;
    }

    // We construct new span from the first and last mappings.
    // If the new span does not contain the original span, there is no translation.
    let constructed_span = TextSpan::new(first.span.start, last.span.end);
    if !constructed_span.contains(span) {
        return call_site;
    }

    // We use the boundaries of the first and last mappings to calculate new span origin.
    let start = match first.origin {
        CodeOrigin::Start(origin_start) => origin_start.add_width(span.start - first.span.start),
        CodeOrigin::Span(span) => span.start,
        CodeOrigin::CallSite(span) => span.start,
    };

    let end = match last.origin {
        CodeOrigin::Start(_) => start.add_width(span.width()),
        CodeOrigin::Span(span) => span.end,
        CodeOrigin::CallSite(span) => span.start,
    };

    Some(TextSpan::new(start, end))
}

/// Returns the parent file and the code mappings of the file.
pub fn get_parent_and_mapping<'db>(
    db: &'db dyn Database,
    file_id: FileId<'db>,
) -> Option<(FileId<'db>, &'db [CodeMapping])> {
    let vf = match file_id.long(db) {
        FileLongId::OnDisk(_) => return None,
        FileLongId::Virtual(vf) => vf,
        FileLongId::External(id) => ext_as_virtual(db, *id),
    };
    Some((vf.parent?, &vf.code_mappings))
}

/// Returns the virtual file matching the external id. Panics if the id is not found.
pub fn ext_as_virtual<'db>(db: &'db dyn Database, id: salsa::Id) -> &'db VirtualFile<'db> {
    files_group_input(db)
        .ext_as_virtual_obj(db)
        .as_ref()
        .expect("`ext_as_virtual` was not set as input.")(db, id)
}

/// Non-pub queries over the files group.
trait PrivFilesGroup: Database {
    /// Create an input crate configuration from a [`CrateConfiguration`].
    fn crate_configuration_input<'db>(
        &'db self,
        config: CrateConfiguration<'db>,
    ) -> &'db CrateConfigurationInput {
        crate_configuration_input(self.as_dyn_database(), config)
    }
}

impl<T: Database + ?Sized> PrivFilesGroup for T {}
