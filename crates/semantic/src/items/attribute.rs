use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, Terminal};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    id: SmolStr,
}

pub fn process_modifiers(
    syntax_db: &dyn SyntaxGroup,
    attributes: ast::AttributeList,
) -> Vec<Attribute> {
    // TODO(ilya): Consider checking for attribute repetitions.
    attributes
        .elements(syntax_db)
        .into_iter()
        .map(|attribute| Attribute { id: attribute.attr(syntax_db).text(syntax_db) })
        .collect()
}
