use std::collections::HashSet;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast;
use crate::db::SemanticGroup;
use crate::Visibility;

pub fn compute_visibility(
    db: &dyn SemanticGroup,
    module_id: &ModuleId,
    visibility: &ast::Visibility,
) -> Maybe<Visibility> {
    match visibility {
        ast::Visibility::Default(_) => {
            let mut module_ids = HashSet::new();
            let mut nodes = vec![module_id.clone()];
            while !nodes.is_empty() {
                if let Some(module_id) = nodes.pop() {
                    if !module_ids.contains(&module_id) {
                        for sub in  db.module_submodules_ids(module_id.clone())? {
                            nodes.push(ModuleId::Submodule(sub));
                        }
                    }
                    module_ids.insert(module_id);
                } else {
                    break;
                }
            }
            Ok(Visibility { is_public: false, module_ids: module_ids.into_iter().collect() })
        }
        ast::Visibility::Public(_) => Ok(Visibility { is_public: true, module_ids: vec![] })
    }
}