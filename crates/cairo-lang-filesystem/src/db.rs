use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::PathBuf;
use std::ptr::NonNull;
use std::sync::{Arc, RwLock};

use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::plumbing::views;
use salsa::{Database, Durability, Setter};
use semver::Version;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::cfg::CfgSet;
use crate::flag::Flag;
use crate::ids::{
    ArcStr, BlobId, BlobLongId, CodeMapping, CodeOrigin, CrateId, CrateInput, CrateLongId,
    Directory, DirectoryInput, FileId, FileInput, FileLongId, FlagLongId, SmolStrId, SpanInFile,
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
    #[serde(rename = "2025_12")]
    V2025_12,
}
impl Edition {
    /// Returns the latest stable edition.
    ///
    /// This Cairo edition is recommended for use in new projects and, in case of existing projects,
    /// to migrate to when possible.
    pub const fn latest() -> Self {
        Self::V2025_12
    }

    /// The name of the prelude submodule of `core::prelude` for this compatibility version.
    pub fn prelude_submodule_name<'db>(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        SmolStrId::from(
            db,
            match self {
                Self::V2023_01 => "v2023_01",
                Self::V2023_10 | Self::V2023_11 => "v2023_10",
                Self::V2024_07 | Self::V2025_12 => "v2024_07",
            },
        )
    }

    /// Whether to ignore visibility modifiers.
    pub fn ignore_visibility(&self) -> bool {
        match self {
            Self::V2023_01 | Self::V2023_10 => true,
            Self::V2023_11 | Self::V2024_07 | Self::V2025_12 => false,
        }
    }

    /// Whether to member access have the original type of the member.
    pub fn member_access_desnaps(&self) -> bool {
        match self {
            Self::V2023_01 | Self::V2023_10 | Self::V2023_11 | Self::V2024_07 => false,
            Self::V2025_12 => true,
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
    /// Allows using representation pointer types (&T), which desugar to BoxTrait<@T>.
    #[serde(default)]
    pub repr_ptrs: bool,
}

/// Function to get a virtual file from an external id.
pub type ExtAsVirtual =
    Arc<dyn for<'a> Fn(&'a dyn Database, salsa::Id) -> &'a VirtualFile<'a> + Send + Sync>;

#[salsa::input]
// TODO(eytan-starkware): Change this mechanism to hold input handles on the db struct outside
// salsa mechanism, and invalidate manually.
pub struct FilesGroupInput {
    /// Structural revision for databases that source crate configurations from a granular side
    /// table. Queries like `crate_configs()` use this to observe newly inserted handles.
    pub crate_configs_revision: u64,
    /// Structural revision for databases that source file content from granular per-file slots
    /// instead of only the raw-file path.
    pub file_contents_revision: u64,
    // TODO(yuval): consider moving this to a separate crate, or rename this crate.
    /// The compilation flags.
    #[returns(ref)]
    pub flags: Option<OrderedHashMap<FlagLongId, Flag>>,
    /// The `#[cfg(...)]` options.
    #[returns(ref)]
    pub cfg_set: Option<CfgSet>,
    #[returns(ref)]
    pub ext_as_virtual_obj: Option<ExtAsVirtual>,
}

#[salsa::tracked]
pub fn files_group_input(db: &dyn Database) -> FilesGroupInput {
    FilesGroupInput::new(db, 0, 0, None, None, None)
}

/// Per-file content slots used by long-running tools (like CairoLS) that need granular
/// invalidation.
#[salsa::input]
pub struct FileContents {
    #[returns(ref)]
    pub on_disk_content: Option<ArcStr>,
    #[returns(ref)]
    pub generated_content: Option<ArcStr>,
}

/// Per-crate configuration input used by long-running tools that need granular invalidation.
#[salsa::input]
pub struct CrateConfig {
    #[returns(ref)]
    pub config: Option<CrateConfigurationInput>,
}

pub type FileContentStorage = Arc<RwLock<HashMap<FileInput, FileContents>>>;
pub type CrateConfigStorage = Arc<RwLock<HashMap<CrateInput, CrateConfig>>>;

pub fn new_file_content_storage() -> FileContentStorage {
    Default::default()
}

pub fn new_crate_config_storage() -> CrateConfigStorage {
    Default::default()
}

/// Database view used by [`FilesGroup::file_content`] to retrieve per-file editor/generated
/// content.
pub trait FileContentView: Database {
    fn file_content_storage(&self) -> Option<&FileContentStorage> {
        None
    }

    fn file_contents_for_input(&self, file_input: &FileInput) -> Option<FileContents> {
        self.file_content_storage()?.read().unwrap().get(file_input).copied()
    }

    /// Returns a content handle for the given file, if one exists.
    fn file_contents<'db>(&'db self, file_id: FileId<'db>) -> Option<FileContents> {
        let file_input = self.file_input(file_id).clone();
        self.file_contents_for_input(&file_input)
    }

    /// Returns on-disk file content, if one exists.
    fn on_disk_file_content<'db>(&'db self, file_id: FileId<'db>) -> Option<&'db ArcStr> {
        self.file_contents(file_id).and_then(|contents| contents.on_disk_content(self).as_ref())
    }

    /// Returns generated file content, if one exists.
    fn generated_file_content<'db>(&'db self, file_id: FileId<'db>) -> Option<&'db ArcStr> {
        self.file_contents(file_id).and_then(|contents| contents.generated_content(self).as_ref())
    }
}

fn cast_file_content_view<Db: FileContentView + 'static>(
    db: NonNull<Db>,
) -> NonNull<dyn FileContentView> {
    // SAFETY: `db` points to a live `Db` of the registered concrete database type.
    let db_ref = unsafe { db.as_ref() };
    NonNull::from(db_ref as &dyn FileContentView)
}

fn file_content_view(db: &dyn Database) -> &dyn FileContentView {
    let caster =
        catch_unwind(AssertUnwindSafe(|| *views(db).downcaster_for::<dyn FileContentView>())).ok();

    let caster = caster.expect("file content view is not registered");
    // SAFETY: The downcaster was fetched for the concrete type backing `db`.
    unsafe { caster.downcast_unchecked(db.into()) }
}

fn maybe_file_content_view(db: &dyn Database) -> Option<&dyn FileContentView> {
    catch_unwind(AssertUnwindSafe(|| file_content_view(db))).ok()
}

pub fn register_files_group_view<Db>(db: &Db)
where
    Db: Database + FileContentView + 'static,
{
    views(db).add::<Db, dyn FileContentView>(cast_file_content_view::<Db>);
}

/// Database view used by [`FilesGroup::crate_config`] to retrieve crate configuration.
pub trait CrateConfigView: Database {
    fn crate_config_storage(&self) -> Option<&CrateConfigStorage> {
        None
    }

    /// Returns a granular config handle for the given crate input, if one exists.
    fn crate_config_for_input(&self, crate_input: &CrateInput) -> Option<CrateConfig> {
        self.crate_config_storage()?.read().unwrap().get(crate_input).copied()
    }

    /// Returns crate configuration input for the given crate id.
    fn crate_config_input_for<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Option<&'db CrateConfigurationInput> {
        let crate_input = self.crate_input(crate_id).clone();
        self.crate_config_input_for_input(&crate_input)
    }

    /// Returns crate configuration input for the given crate input.
    fn crate_config_input_for_input<'db>(
        &'db self,
        crate_input: &CrateInput,
    ) -> Option<&'db CrateConfigurationInput> {
        self.crate_config_for_input(crate_input).and_then(|config| config.config(self).as_ref())
    }
}

fn cast_crate_config_view<Db: CrateConfigView + 'static>(
    db: NonNull<Db>,
) -> NonNull<dyn CrateConfigView> {
    let db_ref = unsafe { db.as_ref() };
    NonNull::from(db_ref as &dyn CrateConfigView)
}

fn crate_config_view(db: &dyn Database) -> &dyn CrateConfigView {
    let caster =
        catch_unwind(AssertUnwindSafe(|| *views(db).downcaster_for::<dyn CrateConfigView>())).ok();

    let caster = caster.expect("granular crate config view is not registered");
    unsafe { caster.downcast_unchecked(db.into()) }
}

fn crate_config_storage(db: &dyn Database) -> &CrateConfigStorage {
    crate_config_view(db)
        .crate_config_storage()
        .expect("granular crate config storage is not registered")
}

pub fn register_crate_config_view<Db>(db: &Db)
where
    Db: Database + CrateConfigView + 'static,
{
    views(db).add::<Db, dyn CrateConfigView>(cast_crate_config_view::<Db>);
}

/// Queries over the files group.
pub trait FilesGroup: Database {
    /// Interned version of `crate_configs_input`.
    fn crate_configs<'db>(&'db self) -> &'db OrderedHashMap<CrateId<'db>, CrateConfiguration<'db>> {
        crate_configs(self.as_dyn_database())
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

    /// Creates an input file from an interned file id.
    fn file_input<'db>(&'db self, file_id: FileId<'db>) -> &'db FileInput {
        file_input(self.as_dyn_database(), file_id)
    }

    /// Creates an input crate from an interned crate id.
    fn crate_input<'db>(&'db self, crt: CrateId<'db>) -> &'db CrateInput {
        crate_input(self.as_dyn_database(), crt)
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
    inp.set_crate_configs_revision(db).to(0);
    inp.set_file_contents_revision(db).to(0);
    inp.set_flags(db).to(Some(Default::default()));
    inp.set_cfg_set(db).to(Some(Default::default()));
}

fn bump_file_contents_revision(db: &mut dyn Database) {
    let next = files_group_input(db).file_contents_revision(db).saturating_add(1);
    files_group_input(db).set_file_contents_revision(db).to(next);
}

fn bump_crate_configs_revision(db: &mut dyn Database) {
    let next = files_group_input(db).crate_configs_revision(db).saturating_add(1);
    files_group_input(db).set_crate_configs_revision(db).to(next);
}

fn ensure_crate_config_handle_for_input(
    db: &mut dyn Database,
    crate_input: CrateInput,
) -> CrateConfig {
    if let Some(handle) = crate_config_view(db).crate_config_for_input(&crate_input) {
        return handle;
    }

    let storage = crate_config_storage(db);
    let handle = CrateConfig::new(db, None);
    storage.write().unwrap().insert(crate_input, handle);
    bump_crate_configs_revision(db);
    handle
}

pub fn set_crate_config_for_input(
    db: &mut dyn Database,
    crate_input: CrateInput,
    config: Option<CrateConfigurationInput>,
) {
    let handle = ensure_crate_config_handle_for_input(db, crate_input);
    handle.set_config(db).with_durability(Durability::HIGH).to(config);
}

#[salsa::tracked(returns(ref))]
pub fn crate_configs<'db>(
    db: &'db dyn Database,
) -> OrderedHashMap<CrateId<'db>, CrateConfiguration<'db>> {
    let _ = files_group_input(db).crate_configs_revision(db);
    crate_config_storage(db)
        .read()
        .unwrap()
        .iter()
        .map(|(crate_input, config)| {
            (
                crate_input.clone().into_crate_long_id(db).intern(db),
                config
                    .config(db)
                    .as_ref()
                    .expect("granular crate config handle should always contain a config")
                    .clone()
                    .into_crate_configuration(db),
            )
        })
        .collect()
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
    let root = CrateConfiguration {
        root: Directory::Real(core_lib_dir),
        settings: CrateSettings {
            name: None,
            edition: Edition::V2025_12,
            version: Version::parse(CORELIB_VERSION).ok(),
            cfg_set: Default::default(),
            dependencies: Default::default(),
            experimental_features: ExperimentalFeaturesConfig {
                negative_impls: true,
                associated_item_constraints: true,
                coupons: true,
                user_defined_inline_macros: true,
                repr_ptrs: true,
            },
        },
        cache_file: None,
    };
    let core_input = {
        let core = CrateLongId::core(db).intern(db);
        db.crate_input(core).clone()
    };
    let config = db.crate_configuration_input(root).clone();
    set_crate_config_for_input(db, core_input, Some(config));
}

/// Sets the root directory of the crate. None value removes the crate.
pub fn set_crate_config(
    db: &mut dyn Database,
    crt: CrateId<'_>,
    root: Option<CrateConfiguration<'_>>,
) {
    let crate_input = db.crate_input(crt).clone();
    let config = root.map(|root| db.crate_configuration_input(root).clone());
    set_crate_config_for_input(db, crate_input, config);
}

/// Sets the root directory of the crate. None value removes the crate.
#[macro_export]
macro_rules! set_crate_config {
    ($self:expr, $crt:expr, $root:expr) => {{
        let crate_input = {
            let db_ref: &dyn salsa::Database = &*$self;
            $crate::db::FilesGroup::crate_input(db_ref, $crt).clone()
        };
        let config = {
            let db_ref: &dyn salsa::Database = &*$self;
            ($root).map(|root| root.into_crate_configuration_input(db_ref))
        };
        $crate::db::set_crate_config_for_input($self, crate_input, config);
    }};
}

fn file_contents_storage(db: &dyn Database) -> &FileContentStorage {
    file_content_view(db).file_content_storage().expect("file content storage is not registered")
}

fn ensure_file_contents_handle_for_input(
    db: &mut dyn Database,
    file_input: FileInput,
) -> FileContents {
    if let Some(handle) = file_content_view(db).file_contents_for_input(&file_input) {
        return handle;
    }

    let handle = FileContents::new(db, None, None);
    file_contents_storage(db).write().unwrap().insert(file_input, handle);
    bump_file_contents_revision(db);
    handle
}

pub fn set_on_disk_file_content(
    db: &mut dyn Database,
    file_id: FileId<'_>,
    content: Option<Arc<str>>,
) {
    let file_input = db.file_input(file_id).clone();
    set_on_disk_file_content_for_input(db, file_input, content);
}

pub fn set_on_disk_file_content_for_input(
    db: &mut dyn Database,
    file_input: FileInput,
    content: Option<Arc<str>>,
) {
    let handle = ensure_file_contents_handle_for_input(db, file_input);
    handle.set_on_disk_content(db).with_durability(Durability::LOW).to(content.map(ArcStr::new));
}

pub fn set_generated_file_content(
    db: &mut dyn Database,
    file_id: FileId<'_>,
    content: Option<Arc<str>>,
) {
    let file_input = db.file_input(file_id).clone();
    set_generated_file_content_for_input(db, file_input, content);
}

pub fn set_generated_file_content_for_input(
    db: &mut dyn Database,
    file_input: FileInput,
    content: Option<Arc<str>>,
) {
    let handle = ensure_file_contents_handle_for_input(db, file_input);
    handle.set_generated_content(db).with_durability(Durability::HIGH).to(content.map(ArcStr::new));
}

pub type FileContentSnapshot = OrderedHashMap<FileInput, (Option<Arc<str>>, Option<Arc<str>>)>;

pub fn snapshot_file_contents(db: &dyn Database) -> FileContentSnapshot {
    let Some(view) = maybe_file_content_view(db) else {
        return Default::default();
    };
    let Some(storage) = view.file_content_storage() else {
        return Default::default();
    };

    storage
        .read()
        .unwrap()
        .iter()
        .map(|(file_input, handle)| {
            (
                file_input.clone(),
                (
                    handle.on_disk_content(db).as_ref().map(|content| (**content).clone()),
                    handle.generated_content(db).as_ref().map(|content| (**content).clone()),
                ),
            )
        })
        .collect()
}

pub fn snapshot_crate_configs(
    db: &dyn Database,
) -> OrderedHashMap<CrateInput, CrateConfigurationInput> {
    crate_config_storage(db)
        .read()
        .unwrap()
        .iter()
        .filter_map(|(crate_input, handle)| {
            let config = handle.config(db).as_ref()?.clone();
            Some((crate_input.clone(), config))
        })
        .collect()
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
    let _ = files_group_input(db).crate_configs_revision(db);
    match crt.long(db) {
        CrateLongId::Real { .. } => crate_config_view(db)
            .crate_config_input_for(crt)
            .cloned()
            .map(|config| config.into_crate_configuration(db)),
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

/// Returns a reference to the configuration of a crate.
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
    let _ = files_group_input(db).file_contents_revision(db);
    maybe_file_content_view(db)
        .and_then(|view| {
            view.on_disk_file_content(file_id).or_else(|| view.generated_file_content(file_id))
        })
        .map(|content| (**content).clone())
        .or_else(|| {
            priv_raw_file_content(db, file_id).map(|content| content.long(db).clone().into())
        })
}

/// Returns a reference to the content of a file as a string.
/// This is a wrapper around the tracked function `file_summary_helper` to return a
/// reference to a type unsupported by salsa tracked functions.
fn file_summary<'db>(db: &'db dyn Database, file: FileId<'db>) -> Option<&'db FileSummary> {
    file_summary_helper(db, file).as_ref()
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
    mut location: SpanInFile<'db>,
    mut parent_files: Option<&mut Vec<FileId<'db>>>,
) -> SpanInFile<'db> {
    if let Some(ref mut parent_files) = parent_files {
        parent_files.push(location.file_id);
    }
    while let Some((parent, code_mappings)) = get_parent_and_mapping(db, location.file_id) {
        location.file_id = parent.file_id;
        if let Some(ref mut parent_files) = parent_files {
            parent_files.push(location.file_id);
        }
        location.span = translate_location(code_mappings, location.span).unwrap_or(parent.span);
    }
    location
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
) -> Option<(SpanInFile<'db>, &'db [CodeMapping])> {
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
    /// Creates an input crate configuration from a [`CrateConfiguration`].
    fn crate_configuration_input<'db>(
        &'db self,
        config: CrateConfiguration<'db>,
    ) -> &'db CrateConfigurationInput {
        crate_configuration_input(self.as_dyn_database(), config)
    }
}

impl<T: Database + ?Sized> PrivFilesGroup for T {}
