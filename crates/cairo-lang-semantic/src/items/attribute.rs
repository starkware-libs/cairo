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
    pub args: Vec<AttributeArg>,
    pub args_stable_ptr: ast::OptionArgListParenthesizedPtr,
}

/// Semantic representation of a single attribute value.
///
/// 1. If `name` is `Some` and `value` is `Some`, then this represents `name: value` argument.
/// 2. If `name` is `None` and `value` is `Some`, then this represents nameless `value` argument.
/// 3. If `name` is `Some` and `value` is `None`, then this represents named variable shorthand
///    `:name`.
/// 4. It is not possible that both `name` and `value` will be `None`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeArg {
    pub name: Option<SmolStr>,
    pub name_stable_ptr: Option<ast::TerminalIdentifierPtr>,
    pub value: Option<ast::Expr>,
    pub value_stable_ptr: Option<ast::ExprPtr>,
    pub arg: ast::Arg,
    pub arg_stable_ptr: ast::ArgPtr,
}

impl Attribute {
    pub fn query_arg_by_key(&self, attr: &str) -> Vec<AttributeArg> {
        self.args
            .iter()
            .filter(|arg| arg.name.as_ref().map(SmolStr::as_str) == Some(attr))
            .cloned()
            .collect()
    }
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        write!(f, r#"Attribute {{ id: "{}""#, self.id)?;
        if !self.args.is_empty() {
            write!(f, ", args: [")?;
            for arg in self.args.iter() {
                write!(f, "{:?}, ", arg.arg.as_syntax_node().get_text(db.upcast()))?;
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
    let attr_args = attribute.arguments(syntax_db);

    Attribute {
        stable_ptr: attribute.stable_ptr(),
        id: attr_id.text(syntax_db),
        id_stable_ptr: attr_id.stable_ptr(),
        args: match attr_args {
            ast::OptionArgListParenthesized::ArgListParenthesized(ref attribute_args) => {
                attribute_args
                    .args(syntax_db)
                    .elements(syntax_db)
                    .into_iter()
                    .map(|arg| ast_arg_to_semantic(syntax_db, arg))
                    .collect()
            }
            ast::OptionArgListParenthesized::Empty(_) => vec![],
        },
        args_stable_ptr: attr_args.stable_ptr(),
    }
}

fn ast_arg_to_semantic(_syntax_db: &dyn SyntaxGroup, _arg: ast::Arg) -> AttributeArg {
    todo!()
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
