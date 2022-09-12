use defs::ids::{ModuleId, ModuleItemId, Symbol};
use filesystem::ids::CrateLongId;
use syntax::node::ast::{self};
use syntax::node::helpers::PathSegmentEx;
use syntax::node::Terminal;

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;

pub fn resolve_item(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    path: &ast::ExprPath,
) -> Result<Symbol, SemanticDiagnosticKind> {
    let syntax_db = db.upcast();
    let elements = path.elements(syntax_db);
    if elements.len() != 1 {
        // TODO(spapini): Qualified paths are not supported yet.
        return None;
    }

    // Follow modules.
    for segment in elements {
        match segment {
            ast::PathSegment::Ident(ident) => {
                let ident = ident.ident(syntax_db).text(syntax_db);
                symbol = match symbol {
                    Symbol::ModuleItem(ModuleItemId::Submodule(submodule)) => Symbol::ModuleItem(
                        db.module_item_by_name(ModuleId::Submodule(submodule), ident)
                            .ok_or(SemanticDiagnosticKind::PathNotFound)?,
                    ),
                    Symbol::Crate(crate_id) => Symbol::ModuleItem(
                        db.module_item_by_name(ModuleId::CrateRoot(crate_id), ident)
                            .ok_or(SemanticDiagnosticKind::PathNotFound)?,
                    ),
                    _ => {
                        // TODO(spapini): Support associated items.
                        return Err(SemanticDiagnosticKind::InvalidPath);
                    }
                }
            }
            ast::PathSegment::GenericArgs(_generic_args) => {
                return Err(SemanticDiagnosticKind::Unsupported);
            }
        }
    }

    Ok(symbol)
}
