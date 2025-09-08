use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, FreeFunctionId, FunctionWithBodyId, ImplAliasId, ImplDefId,
    ImplFunctionId, ImplTypeDefId, ModuleId, StructId, SubmoduleId, TraitFunctionId, TraitId,
    TraitTypeId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::attribute::structured::Attribute;
use salsa::Database;

use crate::db::SemanticGroup;
use crate::{ConcreteEnumId, ConcreteExternTypeId, ConcreteStructId};

pub trait AttributeTrait {
    fn name(&self, db: &dyn Database) -> String;
    fn args(&self, db: &dyn Database) -> String;
    fn full_text(&self, db: &dyn Database) -> String {
        if self.args(db).is_empty() {
            self.name(db)
        } else {
            format!("{}({})", self.name(db), self.args(db))
        }
    }
    fn format(&self, db: &dyn Database) -> String {
        format!("#[{}]", self.full_text(db))
    }
}
impl<'db> AttributeTrait for Attribute<'db> {
    fn name(&self, _db: &dyn Database) -> String {
        self.id.to_string()
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
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>>;

    /// Collect all attributes attached to this node whose name (without args) is exactly `attr`.
    fn query_attr(&self, db: &'db dyn Database, attr: &str) -> Maybe<Vec<Attribute<'db>>> {
        Ok(self.attributes_elements(db)?.into_iter().filter(|a| a.name(db) == attr).collect())
    }

    /// Find first attribute attached to this node whose name (without args) is exactly `attr`.
    fn find_attr(&self, db: &'db dyn Database, attr: &str) -> Maybe<Option<Attribute<'db>>> {
        Ok(self.query_attr(db, attr)?.into_iter().next())
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
        Ok(self
            .query_attr(db, attr_name)?
            .iter()
            .any(|attr| attr.is_single_unnamed_arg(db, arg_name)))
    }
}

impl<'db> SemanticQueryAttrs<'db> for ModuleId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.module_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for StructId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.struct_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ConcreteStructId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        self.struct_id(db).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for TraitId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.trait_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for FunctionWithBodyId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.function_with_body_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ImplDefId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.impl_def_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ImplAliasId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.impl_alias_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for EnumId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.enum_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ConcreteEnumId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        self.enum_id(db).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ExternTypeId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.extern_type_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ConcreteExternTypeId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        self.extern_type_id(db).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for SubmoduleId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        ModuleId::Submodule(*self).attributes_elements(db)
    }
}
impl<'db> SemanticQueryAttrs<'db> for FreeFunctionId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        FunctionWithBodyId::Free(*self).attributes_elements(db)
    }
}

impl<'db> SemanticQueryAttrs<'db> for TraitTypeId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.trait_type_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for TraitFunctionId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.trait_function_attributes(*self)
    }
}

impl<'db> SemanticQueryAttrs<'db> for ImplTypeDefId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.impl_type_def_attributes(*self)
    }
}
impl<'db> SemanticQueryAttrs<'db> for ImplFunctionId<'db> {
    fn attributes_elements(&self, db: &'db dyn Database) -> Maybe<Vec<Attribute<'db>>> {
        db.impl_function_attributes(*self)
    }
}
