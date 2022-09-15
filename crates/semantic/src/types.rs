use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::diagnostic_utils::StableLocation;
use defs::ids::{GenericTypeId, ModuleId};
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
            TypeLongId::Concrete(ConcreteType { generic_type, generic_args }) => {
                assert!(generic_args.is_empty(), "Generic are not supported yet.");
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
        self.generic_type.fmt(f, db.upcast())?;
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
    ty_syntax: ast::Expr,
) -> TypeId {
    let syntax_db = db.upcast();
    match ty_syntax {
        ast::Expr::Path(path) => resolve_item(db, module_id, &path)
            .and_then(GenericTypeId::from)
            .and_then(|generic_type| specialize_type(diagnostics, db, generic_type))
            .unwrap_or_else(|| {
                diagnostics.add(SemanticDiagnostic {
                    stable_location: StableLocation::from_ast(module_id, &path),
                    kind: SemanticDiagnosticKind::UnknownType,
                });
                TypeId::missing(db)
            }),
        ast::Expr::Parenthesized(expr_syntax) => {
            resolve_type(diagnostics, db, module_id, expr_syntax.expr(syntax_db))
        }
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|subexpr_syntax| resolve_type(diagnostics, db, module_id, subexpr_syntax))
                .collect();
            db.intern_type(TypeLongId::Tuple(sub_tys))
        }
        _ => {
            diagnostics.add(SemanticDiagnostic {
                stable_location: StableLocation::from_ast(module_id, &ty_syntax),
                kind: SemanticDiagnosticKind::UnknownType,
            });
            TypeId::missing(db)
        }
    }
}

/// Tries to specializes a generic type.
fn specialize_type(
    _diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    generic_type: GenericTypeId,
) -> Option<TypeId> {
    Some(db.intern_type(TypeLongId::Concrete(ConcreteType { generic_type, generic_args: vec![] })))
}
