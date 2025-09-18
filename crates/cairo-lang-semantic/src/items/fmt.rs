use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use salsa::Database;

use super::constant::ConstValue;
use crate::{ConcreteVariant, MatchArmSelector};

impl<'db> DebugWithDb<'db> for ConstValue<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        match self {
            ConstValue::Int(value, _ty) => write!(f, "{value}"),
            ConstValue::Struct(inner, _) => {
                write!(f, "{{")?;
                let mut inner = inner.iter().peekable();
                while let Some(value) = inner.next() {
                    write!(f, " ")?;
                    value.fmt(f, db)?;
                    write!(f, ": ")?;
                    value.ty(db).unwrap().fmt(f, db)?;
                    if inner.peek().is_some() {
                        write!(f, ",")?;
                    } else {
                        write!(f, " ")?;
                    }
                }
                write!(f, "}}")
            }
            ConstValue::Enum(variant, inner) => {
                variant.fmt(f, db)?;
                write!(f, "(")?;
                inner.fmt(f, db)?;
                write!(f, ")")
            }
            ConstValue::NonZero(value) => {
                write!(f, "NonZero(")?;
                value.fmt(f, db)?;
                write!(f, ")")
            }
            ConstValue::Generic(param) => write!(f, "{}", param.debug_name(db).long(db)),
            ConstValue::Var(var, _) => write!(f, "?{}", var.id.0),
            ConstValue::Missing(_) => write!(f, "missing"),
            ConstValue::ImplConstant(id) => id.fmt(f, db),
        }
    }
}

impl<'db> DebugWithDb<'db> for ConcreteVariant<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        let enum_name = self.concrete_enum_id.enum_id(db).name(db).long(db);
        let variant_name = self.id.name(db).long(db);
        write!(f, "{enum_name}::{variant_name}")
    }
}

impl<'db> DebugWithDb<'db> for MatchArmSelector<'db> {
    type Db = dyn Database;

    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        semantic_db: &'db dyn Database,
    ) -> std::fmt::Result {
        match self {
            MatchArmSelector::VariantId(variant_id) => {
                write!(f, "{:?}", variant_id.debug(semantic_db))
            }
            MatchArmSelector::Value(s) => {
                write!(f, "{:?}", s.value)
            }
        }
    }
}
