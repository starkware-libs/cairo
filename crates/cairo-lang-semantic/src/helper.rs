use cairo_lang_defs::ids::{ExternFunctionId, FreeFunctionId, ModuleId, ModuleItemId, TraitId};
use cairo_lang_filesystem::ids::SmolStrId;
use salsa::Database;

use crate::corelib::CorelibSemantic;
use crate::items::functions::GenericFunctionId;
use crate::items::module::ModuleSemantic;
use crate::{FunctionId, GenericArgumentId, TypeId, corelib};

/// Helper for getting functions in the corelib.
pub struct ModuleHelper<'a> {
    /// The db.
    pub db: &'a dyn Database,
    /// The current module id.
    pub id: ModuleId<'a>,
}
impl<'a> ModuleHelper<'a> {
    /// Returns a helper for the core module.
    pub fn core(db: &'a dyn Database) -> Self {
        Self { db, id: db.core_module() }
    }
    /// Returns a helper for a submodule named `name` of the current module.
    pub fn submodule(&self, name: &'a str) -> Self {
        let id = corelib::get_submodule(self.db, self.id, SmolStrId::from(self.db, name))
            .unwrap_or_else(|| panic!("`{name}` missing in `{}`.", self.id.full_path(self.db)));
        Self { db: self.db, id }
    }
    /// Returns the id of an extern function named `name` in the current module.
    pub fn extern_function_id(&self, name: &str) -> ExternFunctionId<'a> {
        let Ok(Some(ModuleItemId::ExternFunction(id))) =
            self.db.module_item_by_name(self.id, SmolStrId::from(self.db, name))
        else {
            panic!("`{}` not found in `{}`.", name, self.id.full_path(self.db));
        };
        id
    }
    /// Returns the id of a trait named `name` in the current module.
    pub fn trait_id(&self, name: &'a str) -> TraitId<'a> {
        let Ok(Some(ModuleItemId::Trait(id))) =
            self.db.module_item_by_name(self.id, SmolStrId::from(self.db, name))
        else {
            panic!("`{name}` not found in `{}`.", self.id.full_path(self.db));
        };
        id
    }
    /// Returns the id of a free function named `name` in the current module.
    pub fn free_function_id(&self, name: &'a str) -> FreeFunctionId<'a> {
        let Ok(Some(ModuleItemId::FreeFunction(id))) =
            self.db.module_item_by_name(self.id, SmolStrId::from(self.db, name))
        else {
            panic!("`{name}` not found in `{}`.", self.id.full_path(self.db));
        };
        id
    }
    /// Returns the id of a function named `name` in the current module, with the given
    /// `generic_args`.
    pub fn function_id(
        &self,
        name: &'a str,
        generic_args: Vec<GenericArgumentId<'a>>,
    ) -> FunctionId<'a> {
        self.generic_function_id(name).concretize(self.db, generic_args)
    }
    /// Returns the id of a generic function named `name` in the current module.
    pub fn generic_function_id(&self, name: &'a str) -> GenericFunctionId<'a> {
        corelib::get_generic_function_id(self.db, self.id, SmolStrId::from(self.db, name))
    }
    /// Returns the id of a type named `name` in the current module, with the given
    /// `generic_args`.
    pub fn ty(&self, name: &'a str, generic_args: Vec<GenericArgumentId<'a>>) -> TypeId<'a> {
        corelib::get_ty_by_name(self.db, self.id, SmolStrId::from(self.db, name), generic_args)
    }
}
