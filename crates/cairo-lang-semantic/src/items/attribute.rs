use cairo_lang_defs::ids::{
    EnumId, FreeFunctionId, FunctionWithBodyId, ImplAliasId, ImplDefId, ImplFunctionId,
    ImplTypeDefId, ModuleId, StructId, SubmoduleId, TraitFunctionId, TraitId, TraitTypeId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::attribute::structured::Attribute;

use crate::db::SemanticGroup;
use crate::{ConcreteEnumId, ConcreteStructId};

pub trait AttributeTrait {
    fn name(&self, db: &dyn SemanticGroup) -> String;
    fn args(&self, db: &dyn SemanticGroup) -> String;
    fn full_text(&self, db: &dyn SemanticGroup) -> String {
        if self.args(db).is_empty() {
            self.name(db)
        } else {
            format!("{}({})", self.name(db), self.args(db))
        }
    }
    fn format(&self, db: &dyn SemanticGroup) -> String {
        format!("#[{}]", self.full_text(db))
    }
}
impl AttributeTrait for Attribute {
    fn name(&self, _db: &dyn SemanticGroup) -> String {
        self.id.to_string()
    }
    fn args(&self, db: &dyn SemanticGroup) -> String {
        self.args.iter().map(|arg| arg.text(db.upcast())).collect::<Vec<_>>().join(", ")
    }
}

/// Trait for querying attributes of semantic items.
pub trait SemanticQueryAttrs {
    /// Get the list of attributes attached to this node.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>>;

    /// Collect all attributes attached to this node whose name (without args) is exactly `attr`.
    fn query_attr(&self, db: &dyn SemanticGroup, attr: &str) -> Maybe<Vec<Attribute>> {
        Ok(self.attributes_elements(db)?.into_iter().filter(|a| a.name(db) == attr).collect())
    }

    /// Find first attribute attached to this node whose name (without args) is exactly `attr`.
    fn find_attr(&self, db: &dyn SemanticGroup, attr: &str) -> Maybe<Option<Attribute>> {
        Ok(self.query_attr(db, attr)?.into_iter().next())
    }

    /// Check if this node has an attribute whose name (without args) is exactly `attr`.
    fn has_attr(&self, db: &dyn SemanticGroup, attr: &str) -> Maybe<bool> {
        Ok(self.find_attr(db, attr)?.is_some())
    }

    /// Checks if the given object has an attribute with the given name and argument.
    fn has_attr_with_arg(
        &self,
        db: &dyn SemanticGroup,
        attr_name: &str,
        arg_name: &str,
    ) -> Maybe<bool> {
        Ok(self
            .query_attr(db, attr_name)?
            .iter()
            .any(|attr| attr.is_single_unnamed_arg(db.upcast(), arg_name)))
    }
}

impl SemanticQueryAttrs for ModuleId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.module_attributes(*self)
    }
}
impl SemanticQueryAttrs for StructId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.struct_attributes(*self)
    }
}
impl SemanticQueryAttrs for ConcreteStructId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        self.struct_id(db).attributes_elements(db)
    }
}
impl SemanticQueryAttrs for TraitId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.trait_attributes(*self)
    }
}
impl SemanticQueryAttrs for FunctionWithBodyId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.function_with_body_attributes(*self)
    }
}
impl SemanticQueryAttrs for ImplDefId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.impl_def_attributes(*self)
    }
}
impl SemanticQueryAttrs for ImplAliasId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.impl_alias_attributes(*self)
    }
}
impl SemanticQueryAttrs for EnumId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.enum_attributes(*self)
    }
}
impl SemanticQueryAttrs for ConcreteEnumId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        self.enum_id(db).attributes_elements(db)
    }
}
impl SemanticQueryAttrs for SubmoduleId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        ModuleId::Submodule(*self).attributes_elements(db)
    }
}
impl SemanticQueryAttrs for FreeFunctionId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        FunctionWithBodyId::Free(*self).attributes_elements(db)
    }
}

impl SemanticQueryAttrs for TraitTypeId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.trait_type_attributes(*self)
    }
}
impl SemanticQueryAttrs for TraitFunctionId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.trait_function_attributes(*self)
    }
}

impl SemanticQueryAttrs for ImplTypeDefId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.impl_type_def_attributes(*self)
    }
}
impl SemanticQueryAttrs for ImplFunctionId {
    fn attributes_elements(&self, db: &dyn SemanticGroup) -> Maybe<Vec<Attribute>> {
        db.impl_function_attributes(*self)
    }
}
