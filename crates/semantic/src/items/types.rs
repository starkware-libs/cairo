use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::diagnostic_utils::StableLocation;
use defs::ids::{GenericParamId, GenericTypeId, ModuleId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use itertools::Itertools;
use syntax::node::ast;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::resolve_item::resolve_item;
use crate::{semantic, SemanticDiagnostic};

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum TypeLongId {
    Concrete(ConcreteType),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    Missing,
    // TODO(spapini): tuple, generic type parameters.
}
define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type);
impl TypeId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_type(TypeLongId::Missing)
    }
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match db.lookup_intern_type(*self) {
            TypeLongId::Concrete(ConcreteType { generic_type, generic_args: _ }) => {
                // TODO(spapini): Format generics.
                generic_type.format(db.upcast())
            }
            TypeLongId::Tuple(inner_types) => {
                format!("({})", inner_types.into_iter().map(|ty| ty.format(db)).join(", "))
            }
            TypeLongId::Missing => "<missing>".to_string(),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteType {
    pub generic_type: GenericTypeId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteType {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        self.generic_type.fmt(f, db)?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for arg in self.generic_args.iter() {
                write!(f, "{:?},", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

// TODO(yuval): move to a separate module "type".
// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path.
pub fn resolve_type(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(diagnostics, db, module_id, ty_syntax).unwrap_or_else(|kind| {
        diagnostics.add(SemanticDiagnostic {
            stable_location: StableLocation::from_ast(module_id, ty_syntax),
            kind,
        });
        TypeId::missing(db)
    })
}

pub fn maybe_resolve_type(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    ty_syntax: &ast::Expr,
) -> Result<TypeId, SemanticDiagnosticKind> {
    let syntax_db = db.upcast();
    Ok(match ty_syntax {
        ast::Expr::Path(path) => TypeId::try_from(
            resolve_item(db, diagnostics, module_id, path)
                .ok_or(SemanticDiagnosticKind::UnknownType)?,
        )?,
        ast::Expr::Parenthesized(expr_syntax) => {
            resolve_type(diagnostics, db, module_id, &expr_syntax.expr(syntax_db))
        }
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|subexpr_syntax| resolve_type(diagnostics, db, module_id, &subexpr_syntax))
                .collect();
            db.intern_type(TypeLongId::Tuple(sub_tys))
        }
        _ => return Err(SemanticDiagnosticKind::UnknownType),
    })
}

/// Query implementation of [crate::db::SemanticGroup::generic_type_generic_params].
pub fn generic_type_generic_params(
    db: &dyn SemanticGroup,
    generic_type: GenericTypeId,
) -> Option<Vec<GenericParamId>> {
    // TODO(spapini): other types.
    match generic_type {
        GenericTypeId::Struct(_) => Some(vec![]),
        GenericTypeId::Enum(_) => Some(vec![]),
        GenericTypeId::Extern(id) => db.extern_type_declaration_generic_params(id),
    }
}
