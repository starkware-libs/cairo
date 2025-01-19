use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_utils::Upcast;

use super::constant::ConstValue;
use crate::db::SemanticGroup;
use crate::{ConcreteVariant, MatchArmSelector};

impl<Db: ?Sized + Upcast<dyn SemanticGroup + 'static>> DebugWithDb<Db> for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let db = db.upcast();
        match self {
            ConstValue::Int(value, _ty) => write!(f, "{}", value),
            ConstValue::Struct(inner, _) => {
                write!(f, "{{")?;
                let mut inner = inner.iter().peekable();
                while let Some(value) = inner.next() {
                    write!(f, " ")?;
                    value.fmt(f, db)?;
                    write!(f, ": ")?;
                    value.ty(db).unwrap().fmt(f, db.elongate())?;
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
            ConstValue::Boxed(value) => {
                value.fmt(f, db)?;
                write!(f, ".into_box()")
            }
            ConstValue::Generic(param) => write!(f, "{}", param.debug_name(db.upcast())),
            ConstValue::Var(var, _) => write!(f, "?{}", var.id.0),
            ConstValue::Missing(_) => write!(f, "missing"),
            ConstValue::ImplConstant(id) => id.fmt(f, db),
        }
    }
}

impl<Db: ?Sized + Upcast<dyn SemanticGroup + 'static>> DebugWithDb<Db> for ConcreteVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let db = db.upcast();
        let enum_name = self.concrete_enum_id.enum_id(db.upcast()).name(db.upcast());
        let variant_name = self.id.name(db.upcast());
        write!(f, "{enum_name}::{variant_name}")
    }
}

impl<Db: ?Sized + Upcast<dyn SemanticGroup + 'static>> DebugWithDb<Db> for MatchArmSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let db = db.upcast();
        match self {
            MatchArmSelector::VariantId(variant_id) => {
                write!(f, "{:?}", variant_id.debug(db))
            }
            MatchArmSelector::Value(s) => {
                write!(f, "{:?}", s.value)
            }
        }
    }
}
