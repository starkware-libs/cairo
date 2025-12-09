//! Module for generating contextualized paths to items.
//!
//! Provides functionality to find the shortest valid path to an item from a module context,
//! considering hierarchical relationships (direct, super::, crate::).
//!
//! # Limitations
//! - Only handles a module context.

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericTypeId, ImplAliasId,
    ImplDefId, ImportableId, LanguageElementId, MacroDeclarationId, ModuleId, ModuleItemId,
    ModuleTypeAliasId, NamedLanguageElementId, StructId, SubmoduleId, TopLevelLanguageElementId,
    TraitFunctionId, TraitId, VariantId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, SmolStrId, Tracked};
use itertools::Itertools;
use salsa::Database;
use tracing::trace;

use crate::expr::inference::InferenceId;
use crate::items::functions::GenericFunctionId;
use crate::items::module::ModuleSemantic;
use crate::resolve::{ResolvedGenericItem, Resolver};

/// Data about how an item can be accessed from a specific context.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct ItemAccessInfo<'db> {
    /// The shortest path segments to access the item from the context.
    pub path_segments: Vec<ImportableId<'db>>,
    /// How the item is accessed.
    pub access_kind: ItemAccessKind<'db>,
    /// Name of the item being accessed.
    pub name: SmolStrId<'db>,
}

impl<'db> ItemAccessInfo<'db> {
    /// Creates a new ItemAccessInfo from path segments.
    pub fn new(
        access_kind: ItemAccessKind<'db>,
        path_segments: Vec<ImportableId<'db>>,
        name: SmolStrId<'db>,
    ) -> Self {
        Self { path_segments, access_kind, name }
    }

    /// Returns the path as a string.
    pub fn path(&self, db: &'db dyn Database) -> String {
        let prefix = match self.access_kind {
            ItemAccessKind::ViaCrate => "crate::".to_string(),
            ItemAccessKind::DirectInModule
            | ItemAccessKind::ViaUse(_)
            | ItemAccessKind::ViaPrelude
            | ItemAccessKind::ViaGlobalUse
            | ItemAccessKind::FullPath(_) => "".to_string(),
        };
        let mut path = self.path_segments.iter().rev().map(|id| id.name(db).long(db)).join("::");
        if path.is_empty() {
            path = self.name.to_string(db);
        } else {
            path = format!("{path}::{}", self.name.long(db));
        }
        format!("{}{}", prefix, path)
    }

    /// Returns the number of path segments.
    pub fn segment_count(&self) -> usize {
        self.path_segments.len()
    }
}

/// How an item is accessed from a context.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub enum ItemAccessKind<'db> {
    /// The item is directly defined in the current module.
    DirectInModule,
    /// The item is accessed via a `use` statement. Contains the alias name used.
    ViaUse(String),
    /// The item is in the prelude.
    ViaPrelude,
    /// The item is accessed via a `use *` (global use).
    ViaGlobalUse,
    /// The item is accessed via `crate::`.
    ViaCrate,
    /// The item requires its full path.
    FullPath(CrateId<'db>),
}

/// Computes the shortest valid path to access an item from a given module context.
///
/// This function tries different access strategies in order of preference, returning the first
/// valid path found.
///
/// Access strategies tried:
/// 1. Direct access (item in current module)
/// 2. Via use (item imported via use statement)
/// 3. Relative access (super:: paths) - returns the shortest super path
/// 4. Absolute access (crate:: paths)
/// 5. Full path (as fallback)
///
/// # Arguments
/// * `db` - The database
/// * `item` - The item to find a path to
/// * `context_module` - The module from which we want to access the item
///
/// # Returns
/// The shortest valid path to the item.
fn get_contextualized_path<'db>(
    db: &'db dyn Database,
    item: ImportableId<'db>,
    context_module: ModuleId<'db>,
) -> Maybe<ItemAccessInfo<'db>> {
    if let Some(access_info) = check_prelude(db, item, context_module)? {
        return Ok(access_info);
    }

    let mut resolver = Resolver::new(db, context_module, InferenceId::NoContext);

    // Try to find the item in the context module (includes use statements, macro expansions,
    // and star uses)
    let item_name = item.name(db);
    if let Some(info) = resolver.resolve_item_in_module(context_module, item_name)
        && let Some(found_item) = Option::<ImportableId<'_>>::from(info.item_id)
        && found_item == item
    {
        return Ok(ItemAccessInfo::new(ItemAccessKind::DirectInModule, vec![], item_name));
    }

    let (owning_crate, path_items) = ancestors(db, item);
    for (i, ancestor) in path_items.iter().enumerate() {
        // Check if the ancestor is accessible from the context (includes use statements, macro
        // expansions, and star uses)
        if let Some(info) = resolver.resolve_item_in_module(context_module, ancestor.name(db))
            && let Some(found_item) = Option::<ImportableId<'_>>::from(info.item_id)
            && found_item == *ancestor
        {
            return Ok(ItemAccessInfo::new(
                ItemAccessKind::DirectInModule,
                path_items[..i + 1].iter().copied().collect_vec(),
                item_name,
            ));
        }

        // Check if the ancestor is in the prelude.
        if let Some(access_info) = check_prelude(db, *ancestor, context_module)? {
            let mut res_path_segments = path_items[..i + 1].iter().copied().collect_vec();
            res_path_segments.extend(access_info.path_segments);
            return Ok(ItemAccessInfo::new(
                ItemAccessKind::ViaPrelude,
                res_path_segments,
                item_name,
            ));
        }
    }

    if owning_crate == context_module.owning_crate(db) {
        return Ok(ItemAccessInfo::new(ItemAccessKind::ViaCrate, path_items, item_name));
    }

    Ok(ItemAccessInfo::new(ItemAccessKind::FullPath(owning_crate), path_items, item_name))
}

fn check_prelude<'db>(
    db: &'db dyn Database,
    item: ImportableId<'db>,
    context_module: ModuleId<'db>,
) -> Result<Option<ItemAccessInfo<'db>>, cairo_lang_diagnostics::DiagnosticAdded> {
    let settings = db.crate_config(context_module.owning_crate(db)).map(|c| c.settings.clone());
    if let Some(prelude_module) = db.get_prelude_submodule(&settings.unwrap_or_default())
        && let Some(module_item_id) = db.module_item_by_name(prelude_module, item.name(db))?
    {
        trace!("Found item {} in prelude module as {module_item_id:?}", item.name(db).long(db));
        // Deal with use statements
        let is_prelude = match (item, ResolvedGenericItem::from_module_item(db, module_item_id)?) {
            (ImportableId::Constant(id), ResolvedGenericItem::GenericConstant(resolved_id)) => {
                id == resolved_id
            }
            (
                ImportableId::Submodule(id),
                ResolvedGenericItem::Module(ModuleId::Submodule(resolved_id)),
            ) => id == resolved_id,
            (
                ImportableId::FreeFunction(id),
                ResolvedGenericItem::GenericFunction(GenericFunctionId::Free(resolved_id)),
            ) => id == resolved_id,
            (
                ImportableId::ExternFunction(id),
                ResolvedGenericItem::GenericFunction(GenericFunctionId::Extern(resolved_id)),
            ) => id == resolved_id,
            (
                ImportableId::Struct(id),
                ResolvedGenericItem::GenericType(GenericTypeId::Struct(resolved_id)),
            ) => id == resolved_id,
            (
                ImportableId::Enum(id),
                ResolvedGenericItem::GenericType(GenericTypeId::Enum(resolved_id)),
            ) => id == resolved_id,
            (
                ImportableId::ExternType(id),
                ResolvedGenericItem::GenericType(GenericTypeId::Extern(resolved_id)),
            ) => id == resolved_id,
            (ImportableId::TypeAlias(id), ResolvedGenericItem::GenericTypeAlias(resolved_id)) => {
                id == resolved_id
            }
            (ImportableId::ImplAlias(id), ResolvedGenericItem::GenericImplAlias(resolved_id)) => {
                id == resolved_id
            }
            (ImportableId::Trait(id), ResolvedGenericItem::Trait(resolved_id)) => id == resolved_id,
            (ImportableId::Impl(id), ResolvedGenericItem::Impl(resolved_id)) => id == resolved_id,
            (ImportableId::MacroDeclaration(id), ResolvedGenericItem::Macro(resolved_id)) => {
                id == resolved_id
            }
            (ImportableId::Variant(id), ResolvedGenericItem::Variant(resolved_id)) => {
                id == resolved_id.id
            }
            _ => false,
        };

        if is_prelude {
            return Ok(Some(ItemAccessInfo::new(
                ItemAccessKind::ViaPrelude,
                vec![],
                item.name(db),
            )));
        }
    }
    Ok(None)
}

/// Returns the objects in the path to the item, from item to crate root (first item is
/// parent if exists).
fn ancestors<'db>(
    db: &'db dyn Database,
    item: ImportableId<'db>,
) -> (CrateId<'db>, Vec<ImportableId<'db>>) {
    let mut current = item;
    let mut path = vec![];
    while let Some(parent) = get_importable_parent(db, current) {
        path.push(parent);
        current = parent;
        if let ImportableId::Crate(crate_id) = current {
            return (crate_id, path);
        }
    }
    unreachable!("Ancestor path must end with a crate root.")
}

/// Returns the importable parent of an item.
fn get_importable_parent<'db>(
    db: &'db dyn Database,
    item: ImportableId<'db>,
) -> Option<ImportableId<'db>> {
    match item {
        ImportableId::Variant(id) => Some(ImportableId::Enum(id.enum_id(db))),
        _ => {
            let mut module_id = item.parent_module(db)?;
            loop {
                match module_id {
                    ModuleId::CrateRoot(id) => return Some(ImportableId::Crate(id)),
                    ModuleId::Submodule(id) => return Some(ImportableId::Submodule(id)),
                    ModuleId::MacroCall { id, .. } => {
                        module_id = id.parent_module(db);
                    }
                }
            }
        }
    }
}

/// Query implementation of [PathSemantic::contextualized_path].
#[allow(clippy::drop_non_drop)] // salsa macro generates this pattern
#[salsa::tracked]
fn contextualized_path_query<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    item: ImportableId<'db>,
    context_module: ModuleId<'db>,
) -> Maybe<ItemAccessInfo<'db>> {
    let info = get_contextualized_path(db, item, context_module);
    trace!(?item, ?context_module, ?info);
    info
}

pub trait ContextualizePath<'db> {
    /// Returns the shortest valid path to access an item from a given module context.
    fn contextualized_path(
        &self,
        db: &'db dyn Database,
        context_module: ModuleId<'db>,
    ) -> Maybe<String>;
}

trait IntoImportableMarker<'db>: Into<ImportableId<'db>> {}

impl<'db, T> ContextualizePath<'db> for T
where
    for<'a> &'a T: IntoImportableMarker<'db>,
{
    fn contextualized_path(
        &self,
        db: &'db dyn Database,
        context_module: ModuleId<'db>,
    ) -> Maybe<String> {
        let item: ImportableId<'db> = self.into();
        contextualized_path_query(db, (), item, context_module).map(|info| info.path(db))
    }
}

impl<'db> IntoImportableMarker<'db> for &CrateId<'db> {}
impl<'db> IntoImportableMarker<'db> for &SubmoduleId<'db> {}
impl<'db> IntoImportableMarker<'db> for &ConstantId<'db> {}
impl<'db> IntoImportableMarker<'db> for &FreeFunctionId<'db> {}
impl<'db> IntoImportableMarker<'db> for &StructId<'db> {}
impl<'db> IntoImportableMarker<'db> for &EnumId<'db> {}
impl<'db> IntoImportableMarker<'db> for &VariantId<'db> {}
impl<'db> IntoImportableMarker<'db> for &ModuleTypeAliasId<'db> {}
impl<'db> IntoImportableMarker<'db> for &ImplAliasId<'db> {}
impl<'db> IntoImportableMarker<'db> for &TraitId<'db> {}
impl<'db> IntoImportableMarker<'db> for &ImplDefId<'db> {}
impl<'db> IntoImportableMarker<'db> for &ExternTypeId<'db> {}
impl<'db> IntoImportableMarker<'db> for &ExternFunctionId<'db> {}
impl<'db> IntoImportableMarker<'db> for &MacroDeclarationId<'db> {}

impl<'db> ContextualizePath<'db> for ModuleId<'db> {
    fn contextualized_path(
        &self,
        db: &'db dyn Database,
        context_module: ModuleId<'db>,
    ) -> Maybe<String> {
        match self {
            ModuleId::CrateRoot(crate_id) => crate_id.contextualized_path(db, context_module),
            ModuleId::Submodule(submodule_id) => {
                submodule_id.contextualized_path(db, context_module)
            }
            ModuleId::MacroCall { .. } => Ok(self.full_path(db)),
        }
    }
}

impl<'db> ContextualizePath<'db> for ModuleItemId<'db> {
    fn contextualized_path(
        &self,
        db: &'db dyn Database,
        context_module: ModuleId<'db>,
    ) -> Maybe<String> {
        if let Some(item) = Option::<ImportableId<'db>>::from(*self) {
            contextualized_path_query(db, (), item, context_module).map(|info| info.path(db))
        } else {
            Ok(self.full_path(db))
        }
    }
}

impl<'db> ContextualizePath<'db> for TraitFunctionId<'db> {
    fn contextualized_path(
        &self,
        db: &'db dyn Database,
        context_module: ModuleId<'db>,
    ) -> Maybe<String> {
        let contextualized_path = self.trait_id(db).contextualized_path(db, context_module)?;
        Ok(if contextualized_path.is_empty() {
            self.name(db).to_string(db)
        } else {
            format!("{contextualized_path}::{}", self.name(db).long(db))
        })
    }
}
