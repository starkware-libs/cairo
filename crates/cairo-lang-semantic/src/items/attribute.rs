use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, FreeFunctionId, FunctionWithBodyId, ImplAliasId, ImplDefId,
    ImplFunctionId, ImplTypeDefId, ModuleId, StructId, SubmoduleId, TraitFunctionId, TraitId,
    TraitTypeId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_syntax::attribute::structured::Attribute;
use salsa::Database;

use crate::items::enm::EnumSemantic;
use crate::items::extern_type::ExternTypeSemantic;
use crate::items::function_with_body::FunctionWithBodySemantic;
use crate::items::imp::ImplSemantic;
use crate::items::impl_alias::ImplAliasSemantic;
use crate::items::module::ModuleSemantic;
use crate::items::structure::StructSemantic;
use crate::items::trt::TraitSemantic;
use crate::{ConcreteEnumId, ConcreteExternTypeId, ConcreteStructId};

pub trait AttributeTrait<'db> {
    fn name(&self, db: &'db dyn Database) -> SmolStrId<'db>;
    fn args(&self, db: &dyn Database) -> String;
    fn full_text(&self, db: &'db dyn Database) -> String {
        if self.args(db).is_empty() {
            self.name(db).to_string(db)
        } else {
            format!("{}({})", self.name(db).long(db), self.args(db))
        }
    }
    fn format(&self, db: &'db dyn Database) -> String {
        format!("#[{}]", self.full_text(db))
    }
}
impl<'db> AttributeTrait<'db> for Attribute<'db> {
    fn name(&self, _db: &'db dyn Database) -> SmolStrId<'db> {
        self.id
    }
    fn args(&self, db: &dyn Database) -> String {
        self.args.iter().map(|arg| arg.text(db)).collect::<Vec<_>>().join(", ")
    }
}

/// Trait for querying attributes of semantic items.
pub trait SemanticQueryAttrs<'db> {
    /// Get the list of attributes attached to this node.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]>;

    /// All attributes attached to this node whose name (without args) is exactly `attr`.
    fn query_attr(
        &self,
        db: &'db dyn Database,
        attr: &str,
    ) -> Maybe<impl Iterator<Item = &'db Attribute<'db>>> {
        Ok(self.attributes_elements(db)?.iter().filter(move |a| a.id.long(db) == attr))
    }

    /// Find first attribute attached to this node whose name (without args) is exactly `attr`.
    fn find_attr(&self, db: &'db dyn Database, attr: &str) -> Maybe<Option<&'db Attribute<'db>>> {
        Ok(self.query_attr(db, attr)?.next())
    }

    /// Check if this node has an attribute whose name (without args) is exactly `attr`.
    fn has_attr(&self, db: &'db dyn Database, attr: &str) -> Maybe<bool> {
        Ok(self.find_attr(db, attr)?.is_some())
    }

    /// Checks if the given object has an attribute with the given name and argument.
    fn has_attr_with_arg(
        &self,
        db: &'db dyn Database,
        attr_name: &str,
        arg_name: &str,
    ) -> Maybe<bool> {
        Ok(self.query_attr(db, attr_name)?.any(|attr| attr.is_single_unnamed_arg(db, arg_name)))
    }
}

impl<'db> SemanticQueryAttrs<'db> for ModuleId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.module_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for StructId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.struct_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ConcreteStructId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        self.struct_id(db).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for TraitId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.trait_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for FunctionWithBodyId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.function_with_body_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ImplDefId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.impl_def_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ImplAliasId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.impl_alias_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for EnumId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.enum_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ConcreteEnumId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        self.enum_id(db).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ExternTypeId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.extern_type_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ConcreteExternTypeId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        self.extern_type_id(db).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for SubmoduleId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        ModuleId::Submodule(*self).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for FreeFunctionId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        FunctionWithBodyId::Free(*self).attributes_elements(db)
    }
}

impl<'db> SemanticQueryAttrs<'db> for TraitTypeId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.trait_type_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for TraitFunctionId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.trait_function_attributes(*self)
    }
}

impl<'db> SemanticQueryAttrs<'db> for ImplTypeDefId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.impl_type_def_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ImplFunctionId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<&'db [Attribute<'db>]> {
        db.impl_function_attributes(*self)
    }
}
