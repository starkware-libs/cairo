use std::fmt;

use cairo_lang_debug::DebugWithDb;
use smol_str::SmolStr;

use crate::node::ast::ArgClause;
use crate::node::db::SyntaxGroup;
use crate::node::{ast, Terminal, TypedSyntaxNode};

/// Easier to digest representation of an [ast::Attribute].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub stable_ptr: ast::AttributePtr,
    pub id: SmolStr,
    pub id_stable_ptr: ast::TerminalIdentifierPtr,
    pub args: Vec<AttributeArg>,
    pub args_stable_ptr: ast::OptionArgListParenthesizedPtr,
}

/// Easier to digest representation of a single attribute value.
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

impl<'a> DebugWithDb<dyn SyntaxGroup + 'a> for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SyntaxGroup + 'a)) -> fmt::Result {
        write!(f, r#"Attribute {{ id: "{}""#, self.id)?;
        if !self.args.is_empty() {
            write!(f, ", args: [")?;
            for arg in self.args.iter() {
                write!(f, "{:?}, ", arg.arg.as_syntax_node().get_text(db))?;
            }
            write!(f, "]")?;
        }
        write!(f, " }}")
    }
}

pub trait AttributeStructurize {
    /// Return the structured attribute for the given [ast::Attribute].
    fn structurize(self, db: &dyn SyntaxGroup) -> Attribute;
}

impl AttributeStructurize for ast::Attribute {
    fn structurize(self, db: &dyn SyntaxGroup) -> Attribute {
        let attr_id = self.attr(db);
        let attr_args = self.arguments(db);

        Attribute {
            stable_ptr: self.stable_ptr(),
            id: attr_id.text(db),
            id_stable_ptr: attr_id.stable_ptr(),
            args: match attr_args {
                ast::OptionArgListParenthesized::ArgListParenthesized(ref attribute_args) => {
                    attribute_args
                        .args(db)
                        .elements(db)
                        .into_iter()
                        .map(|arg| AttributeArg::from(arg, db))
                        .collect()
                }
                ast::OptionArgListParenthesized::Empty(_) => vec![],
            },
            args_stable_ptr: attr_args.stable_ptr(),
        }
    }
}

pub trait AttributeListStructurize {
    /// Return structured attributes for the given [ast::AttributeList].
    fn structurize(self, db: &dyn SyntaxGroup) -> Vec<Attribute>;
}

impl AttributeListStructurize for ast::AttributeList {
    fn structurize(self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        // TODO(ilya): Consider checking for attribute repetitions.
        self.elements(db).into_iter().map(|attr| attr.structurize(db)).collect()
    }
}

impl AttributeArg {
    /// Build [AttributeArg] from [ast::Arg].
    fn from(syntax_arg: ast::Arg, db: &dyn SyntaxGroup) -> AttributeArg {
        // TODO(mkaput): We deliberately ignore modifiers here, because no existing attribute uses
        //   them. Perhaps in the future we might ban them on syntactic level?
        let mut name = None;
        let mut name_stable_ptr = None;
        let mut value = None;
        let mut value_stable_ptr = None;
        match syntax_arg.arg_clause(db) {
            ArgClause::Unnamed(clause) => {
                let expr = clause.value(db);
                value_stable_ptr = Some(expr.stable_ptr());
                value = Some(expr);
            }
            ArgClause::Named(clause) => {
                let identifier = clause.name(db);
                name_stable_ptr = Some(identifier.stable_ptr());
                name = Some(identifier.text(db));

                let expr = clause.value(db);
                value_stable_ptr = Some(expr.stable_ptr());
                value = Some(expr);
            }
            ArgClause::FieldInitShorthand(clause) => {
                let identifier = clause.name(db).name(db);
                name_stable_ptr = Some(identifier.stable_ptr());
                name = Some(identifier.text(db));
            }
        }

        assert!(name.is_some() || value.is_some());
        assert_eq!(name.is_some(), name_stable_ptr.is_some());
        assert_eq!(value.is_some(), value_stable_ptr.is_some());

        let arg_stable_ptr = syntax_arg.stable_ptr();
        AttributeArg {
            name,
            name_stable_ptr,
            value,
            value_stable_ptr,
            arg: syntax_arg,
            arg_stable_ptr,
        }
    }
}
