use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;

use crate::db::SemanticGroup;

/// Semantic representation of an attribute.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub stable_ptr: ast::AttributePtr,
    pub id: SmolStr,
    pub id_stable_ptr: ast::TerminalIdentifierPtr,
    pub args: Vec<AttributeArg>,
    pub args_stable_ptr: ast::OptionAttributeArgsPtr,
}

/// Semantic representation of a single attribute value.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum AttributeArg {
    Key(AttributeArgKey),
    KeyValue(AttributeArgKeyValue),
    FnCall(AttributeArgFnCall),
    Number(ast::TerminalLiteralNumber),
    ShortString(ast::TerminalShortString),
    False(ast::TerminalFalse),
    True(ast::TerminalTrue),
}

/// Semantic representation of an attribute argument of form `name`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeArgKey {
    pub stable_ptr: ast::AttributeArgKeyPtr,
    pub path: ast::ExprPath,
    pub path_stable_ptr: ast::ExprPathPtr,
}

/// Semantic representation of an attribute argument of form `name = value`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeArgKeyValue {
    pub stable_ptr: ast::AttributeArgKeyValuePtr,
    pub path: ast::ExprPath,
    pub path_stable_ptr: ast::ExprPathPtr,
    pub value: ast::Expr,
    pub value_stable_ptr: ast::ExprPtr,
}

/// Semantic representation of an attribute argument of form `name(arg1, arg2, ...)`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeArgFnCall {
    pub stable_ptr: ast::AttributeArgFnCallPtr,
    pub path: ast::ExprPath,
    pub path_stable_ptr: ast::ExprPathPtr,
    pub args: Vec<AttributeArg>,
    pub args_stable_ptr: ast::AttributeArgsPtr,
}

impl AttributeArg {
    pub fn stable_ptr(&self) -> SyntaxStablePtrId {
        match self {
            AttributeArg::Key(v) => v.stable_ptr.untyped(),
            AttributeArg::KeyValue(v) => v.stable_ptr.untyped(),
            AttributeArg::FnCall(v) => v.stable_ptr.untyped(),
            AttributeArg::Number(v) => v.stable_ptr().untyped(),
            AttributeArg::ShortString(v) => v.stable_ptr().untyped(),
            AttributeArg::False(v) => v.stable_ptr().untyped(),
            AttributeArg::True(v) => v.stable_ptr().untyped(),
        }
    }
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        write!(f, r#"Attribute {{ id: "{}""#, self.id)?;
        if !self.args.is_empty() {
            write!(f, ", args: [")?;
            for arg in &self.args {
                arg.fmt(f, db)?;
                write!(f, ", ")?;
            }
            write!(f, "]")?;
        }
        write!(f, " }}")
    }
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for AttributeArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        match self {
            AttributeArg::Key(v) => v.fmt(f, db),
            AttributeArg::KeyValue(v) => v.fmt(f, db),
            AttributeArg::FnCall(v) => v.fmt(f, db),
            AttributeArg::Number(v) => write!(f, "{}", v.text(db.upcast())),
            AttributeArg::ShortString(v) => write!(f, "{}", v.text(db.upcast())),
            AttributeArg::False(v) => write!(f, "{}", v.text(db.upcast())),
            AttributeArg::True(v) => write!(f, "{}", v.text(db.upcast())),
        }
    }
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for AttributeArgKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        write!(f, "{}", self.path.as_syntax_node().get_text(db.upcast()))
    }
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for AttributeArgKeyValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        write!(
            f,
            "{} = {:?}",
            self.path.as_syntax_node().get_text(db.upcast()),
            self.value.as_syntax_node().get_text(db.upcast())
        )
    }
}

impl<'a> DebugWithDb<dyn SemanticGroup + 'a> for AttributeArgFnCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SemanticGroup + 'a)) -> fmt::Result {
        write!(f, "{}(", self.path.as_syntax_node().get_text(db.upcast()))?;
        for arg in &self.args {
            arg.fmt(f, db)?;
            write!(f, ", ")?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

/// Returns the semantic attributes for the given AST attribute list.
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

/// Returns the semantic attribute for the given AST attribute.
pub fn ast_attribute_to_semantic(
    syntax_db: &dyn SyntaxGroup,
    attribute: ast::Attribute,
) -> Attribute {
    let attr_id = attribute.ident(syntax_db);
    let attr_args = attribute.args(syntax_db);

    Attribute {
        stable_ptr: attribute.stable_ptr(),
        id: attr_id.text(syntax_db),
        id_stable_ptr: attr_id.stable_ptr(),
        args: match attr_args {
            ast::OptionAttributeArgs::AttributeArgs(ref attribute_args) => attribute_args
                .arg_list(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|arg| ast_attribute_arg_to_semantic(syntax_db, arg))
                .collect(),
            ast::OptionAttributeArgs::Empty(_) => vec![],
        },
        args_stable_ptr: attr_args.stable_ptr(),
    }
}

fn ast_attribute_arg_to_semantic(
    syntax_db: &dyn SyntaxGroup,
    arg: ast::AttributeArg,
) -> AttributeArg {
    match arg {
        ast::AttributeArg::Key(variant) => {
            let stable_ptr = variant.stable_ptr();
            let path = variant.path(syntax_db);
            let path_stable_ptr = variant.path(syntax_db).stable_ptr();
            AttributeArg::Key(AttributeArgKey { stable_ptr, path, path_stable_ptr })
        }
        ast::AttributeArg::KeyValue(variant) => {
            let stable_ptr = variant.stable_ptr();
            let path = variant.path(syntax_db);
            let path_stable_ptr = variant.path(syntax_db).stable_ptr();
            let value = variant.expr(syntax_db);
            let value_stable_ptr = value.stable_ptr();
            AttributeArg::KeyValue(AttributeArgKeyValue {
                stable_ptr,
                path,
                path_stable_ptr,
                value,
                value_stable_ptr,
            })
        }
        ast::AttributeArg::FnCall(variant) => {
            let stable_ptr = variant.stable_ptr();
            let path = variant.path(syntax_db);
            let path_stable_ptr = variant.path(syntax_db).stable_ptr();
            let args = variant
                .args(syntax_db)
                .arg_list(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|arg| ast_attribute_arg_to_semantic(syntax_db, arg))
                .collect();
            let args_stable_ptr = variant.args(syntax_db).stable_ptr();
            AttributeArg::FnCall(AttributeArgFnCall {
                stable_ptr,
                path,
                path_stable_ptr,
                args,
                args_stable_ptr,
            })
        }
        ast::AttributeArg::Number(terminal) => AttributeArg::Number(terminal),
        ast::AttributeArg::ShortString(terminal) => AttributeArg::ShortString(terminal),
        ast::AttributeArg::False(terminal) => AttributeArg::False(terminal),
        ast::AttributeArg::True(terminal) => AttributeArg::True(terminal),
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
