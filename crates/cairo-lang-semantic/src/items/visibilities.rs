use cairo_lang_syntax::node::ast;
use crate::db::SemanticGroup;

/// The visibility attribute of a item or member.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub enum Visibility {
    Default,
    Public,
}

pub fn priv_visibility(db: &dyn SemanticGroup, vis: ast::OptionVisibility) -> Visibility {
    match vis {
        ast::OptionVisibility::Visibility(vis) => {
            match vis.item(db.upcast()) {
                ast::VisibilityItem::Pub(_) => Visibility::Public
            }
        }
        ast::OptionVisibility::Empty(_) => Visibility::Default
    }
}