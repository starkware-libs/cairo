use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast::ArgClause;
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

/// Returns the semantic attribute argument for the given [ast::Arg].
fn ast_arg_to_semantic(syntax_db: &dyn SyntaxGroup, syntax_arg: ast::Arg) -> AttributeArg {
    // TODO(mkaput): We deliberately ignore modifiers here, because no existing attribute uses them.
    //   Perhaps in the future we might ban them on syntactic level?
    let mut name = None;
    let mut name_stable_ptr = None;
    let mut value = None;
    let mut value_stable_ptr = None;
    match syntax_arg.arg_clause(syntax_db) {
        ArgClause::Unnamed(clause) => {
            let expr = clause.value(syntax_db);
            value_stable_ptr = Some(expr.stable_ptr());
            value = Some(expr);
        }
        ArgClause::Named(clause) => {
            let identifier = clause.name(syntax_db);
            name_stable_ptr = Some(identifier.stable_ptr());
            name = Some(identifier.text(syntax_db));

            let expr = clause.value(syntax_db);
            value_stable_ptr = Some(expr.stable_ptr());
            value = Some(expr);
        }
        ArgClause::FieldInitShorthand(clause) => {
            let identifier = clause.name(syntax_db).name(syntax_db);
            name_stable_ptr = Some(identifier.stable_ptr());
            name = Some(identifier.text(syntax_db));
        }
    }

    assert!(name.is_some() || value.is_some());
    assert_eq!(name.is_some(), name_stable_ptr.is_some());
    assert_eq!(value.is_some(), value_stable_ptr.is_some());

    let arg_stable_ptr = syntax_arg.stable_ptr();
    AttributeArg { name, name_stable_ptr, value, value_stable_ptr, arg: syntax_arg, arg_stable_ptr }
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
