use defs::ids::{ModuleId, ModuleItemId, Symbol};
use filesystem::ids::CrateLongId;
use syntax::node::ast::{self};
use syntax::node::helpers::GetIdentifier;
use syntax::node::Terminal;
use utils::OptionHelper;

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;

pub fn resolve_item(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    module_id: ModuleId,
    path: &ast::ExprPath,
) -> Option<Symbol> {
    let syntax_db = db.upcast();
    let elements_vec = path.elements(syntax_db);
    let mut elements = elements_vec.iter().peekable();

    let ident = elements
        .next()
        .map(|segment| segment.identifier(syntax_db))
        .on_none(|| diagnostics.report(path, InvalidPath))?;
    let mut symbol = db
        .module_item_by_name(module_id, ident.clone())
        .map(Symbol::ModuleItem)
        .or_else(|| {
            // TODO(spapini): Use a hashset.
            // TODO(spapini): Better diagnostics.
            let crate_id = db.intern_crate(CrateLongId(ident.clone()));
            if db.crates().iter().any(|c| *c == crate_id) {
                Some(Symbol::Crate(crate_id))
            } else {
                None
            }
        })
        .or_else(|| db.module_item_by_name(core_module(db), ident.clone()).map(Symbol::ModuleItem))
        .on_none(|| diagnostics.report(path, PathNotFound))?;

    // Follow modules.
    for segment in elements {
        match segment {
            ast::PathSegment::Simple(ident) => {
                let ident = ident.ident(syntax_db).text(syntax_db);
                symbol = match symbol {
                    Symbol::ModuleItem(ModuleItemId::Submodule(submodule)) => Symbol::ModuleItem(
                        db.module_item_by_name(ModuleId::Submodule(submodule), ident)
                            .on_none(|| diagnostics.report(segment, PathNotFound))?,
                    ),
                    Symbol::Crate(crate_id) => Symbol::ModuleItem(
                        db.module_item_by_name(ModuleId::CrateRoot(crate_id), ident)
                            .on_none(|| diagnostics.report(segment, PathNotFound))?,
                    ),
                    _ => {
                        diagnostics.report(segment, InvalidPath);
                        return None;
                    }
                }
            }
            ast::PathSegment::WithGenericArgs(generic_args) => {
                diagnostics.report(generic_args, Unsupported);
                return None;
            }
        }
    }

    Some(symbol)
}
