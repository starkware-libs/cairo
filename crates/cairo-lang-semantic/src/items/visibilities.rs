use std::collections::HashSet;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast;
use crate::db::SemanticGroup;
use crate::Visibility;

pub fn semantic_visibility(
    module_id: &ModuleId,
    visibility: &ast::Visibility,
) -> Visibility {
    match visibility {
        ast::Visibility::Public(_) => Visibility::Public,
        ast::Visibility::Default(_) => Visibility::SubModule(module_id.clone()),
    }
}

pub fn is_visible_in(db: &dyn SemanticGroup, visibility: Visibility, module_id: ModuleId) -> Maybe<bool> {
    match visibility {
        Visibility::Public => Ok(true),
        Visibility::SubModule(node) => {
            let mut visited = HashSet::new();
            let mut nodes = vec![node];
            while let Some(current) = nodes.pop() {
                if module_id == current {
                    return Ok(true)
                }
                if !visited.contains(&current) {
                    for sub_id in  db.module_submodules_ids(current.clone())? {
                        nodes.push(ModuleId::Submodule(sub_id));
                    }
                    visited.insert(current);
                }
            }
            Ok(false)
        }
    }
}