use cairo_lang_defs::ids::{ExternFunctionId, ModuleId, ModuleItemId, TraitId};
use cairo_lang_utils::extract_matches;
use smol_str::SmolStr;

use crate::db::SemanticGroup;
use crate::items::us::SemanticUseEx;
use crate::resolve::ResolvedGenericItem;
use crate::{FunctionId, GenericArgumentId, TypeId, corelib};

/// Helper for getting functions in the corelib.
pub struct ModuleHelper<'a> {
    /// The db.
    db: &'a dyn SemanticGroup,
    /// The current module id.
    id: ModuleId,
}
impl<'a> ModuleHelper<'a> {
    /// Returns a helper for the given module.
    pub fn new(db: &'a dyn SemanticGroup, id: ModuleId) -> Self {
        Self { db, id }
    }
    /// Returns a helper for the core module.
    pub fn core(db: &'a dyn SemanticGroup) -> Self {
        Self::new(db, db.core_module())
    }
    /// Returns a helper for a submodule named `name` of the current module.
    pub fn submodule(&self, name: &str) -> Self {
        let id = corelib::get_submodule(self.db, self.id, name).unwrap_or_else(|| {
            panic!("`{name}` missing in `{}`.", self.id.full_path(self.db.upcast()))
        });
        Self { db: self.db, id }
    }
    /// Returns the id of an extern function named `name` in the current module.
    pub fn extern_function_id(&self, name: impl Into<SmolStr>) -> ExternFunctionId {
        let name = name.into();
        let Ok(Some(ModuleItemId::ExternFunction(id))) =
            self.db.module_item_by_name(self.id, name.clone())
        else {
            panic!("`{}` not found in `{}`.", name, self.id.full_path(self.db.upcast()));
        };
        id
    }
    /// Returns the id of a function named `name` in the current module, with the given
    /// `generic_args`.
    pub fn function_id(
        &self,
        name: impl Into<SmolStr>,
        generic_args: Vec<GenericArgumentId>,
    ) -> FunctionId {
        corelib::get_function_id(self.db, self.id, name.into(), generic_args)
    }

    /// Returns the id of a type named `name` in the current module.
    pub fn ty(&self, name: impl Into<SmolStr>, generic_args: Vec<GenericArgumentId>) -> TypeId {
        corelib::get_ty_by_name(self.db, self.id, name.into(), generic_args)
    }

    /// Returns the id of a trait named `name` in the current module.
    pub fn trait_id(&self, name: impl Into<SmolStr>) -> TraitId {
        // This should not fail if the corelib is present.
        let name = name.into();
        let item_id = self
            .db
            .module_item_by_name(self.id, name.clone())
            .unwrap_or_else(|_| {
                panic!(
                    "Core module `{module}` failed to compile.",
                    module = self.id.full_path(self.db.upcast())
                )
            })
            .unwrap_or_else(|| {
                panic!(
                    "Core module `{module}` is missing an use item for trait `{name}`.",
                    module = self.id.full_path(self.db.upcast()),
                )
            });
        match item_id {
            ModuleItemId::Trait(id) => id,
            ModuleItemId::Use(use_id) => {
                extract_matches!(
                    self.db.use_resolved_item(use_id).unwrap_or_else(|_| panic!(
                        "Could not resolve core trait `{module}::{name}`.",
                        module = self.id.full_path(self.db.upcast()),
                    )),
                    ResolvedGenericItem::Trait
                )
            }
            _ => panic!("Expecting only traits, or uses pointing to traits."),
        }
    }

    pub fn id(&self) -> ModuleId {
        self.id
    }
}
