use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;

use crate::db::SemanticGroup;

/// Semantic representation of an [ast::Attribute].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub stable_ptr: ast::AttributePtr,
    pub id: SmolStr,
    pub id_stable_ptr: ast::TerminalIdentifierPtr,
    pub args: Vec<ast::Expr>,
    pub args_stable_ptr: ast::OptionAttributeArgsPtr,
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        write!(f, r#"Attribute {{ id: "{}""#, self.id)?;
        if !self.args.is_empty() {
            write!(f, ", args: [")?;
            for arg in self.args.iter() {
                write!(f, "{:?}, ", arg.as_syntax_node().get_text(db.upcast()))?;
            }
            write!(f, "]")?;
        }
        write!(f, " }}")
    }
}

/// Returns the semantic attributes for the given [ast::AttributeList].
pub fn ast_attributes_to_semantic(
    syntax_db: &dyn SyntaxGroup,
    attributes: ast::AttributeList,
) -> Vec<Attribute> {
    // TODO(ilya): Consider checking for attribute repetitions.
    attributes
        .elements(syntax_db)
        .into_iter()
        .map(|a| ast_attribute_to_semantic(syntax_db, a))
        .collect()
}

/// Returns the semantic attribute for the given [ast::Attribute].
pub fn ast_attribute_to_semantic(
    syntax_db: &dyn SyntaxGroup,
    attribute: ast::Attribute,
) -> Attribute {
    let attr_id = attribute.attr(syntax_db);
    let attr_args = attribute.args(syntax_db);

    Attribute {
        stable_ptr: attribute.stable_ptr(),
        id: attr_id.text(syntax_db),
        id_stable_ptr: attr_id.stable_ptr(),
        args: match attr_args {
            ast::OptionAttributeArgs::AttributeArgs(ref attribute_args) => {
                attribute_args.arg_list(syntax_db).elements(syntax_db)
            }
            ast::OptionAttributeArgs::Empty(_) => vec![],
        },
        args_stable_ptr: attr_args.stable_ptr(),
    }
}

/// Query implementation of [SemanticGroup::module_attributes].
pub fn module_attributes(db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<Vec<Attribute>> {
    Ok(match module_id {
        ModuleId::CrateRoot(_) => vec![],
        ModuleId::Submodule(submodule_id) => {
            let module_ast =
                &db.module_submodules(submodule_id.parent_module(db.upcast()))?[submodule_id];
            let syntax_db = db.upcast();

            ast_attributes_to_semantic(syntax_db, module_ast.attributes(syntax_db))
        }
    })
}
