use std::fmt;

use cairo_lang_debug::DebugWithDb;
use smol_str::SmolStr;

use crate::node::db::SyntaxGroup;
use crate::node::{ast, Terminal, TypedSyntaxNode};

/// Easier to digest representation of an [ast::Attribute].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub stable_ptr: ast::AttributePtr,
    pub id: SmolStr,
    pub id_stable_ptr: ast::ExprPathPtr,
    pub args: Vec<AttributeArg>,
    pub args_stable_ptr: ast::OptionArgListParenthesizedPtr,
}
impl Attribute {
    /// Checks if the given attribute has a single argument with the given name.
    pub fn is_single_unnamed_arg(&self, db: &dyn SyntaxGroup, arg_name: &str) -> bool {
        match &self.args[..] {
            [arg] => match &arg.variant {
                AttributeArgVariant::Unnamed { value, .. } => {
                    value.as_syntax_node().get_text_without_trivia(db) == arg_name
                }
                _ => false,
            },
            _ => false,
        }
    }
}

/// Easier to digest representation of a single attribute value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeArg {
    pub variant: AttributeArgVariant,
    pub arg: ast::Arg,
    pub arg_stable_ptr: ast::ArgPtr,
    pub modifiers: Vec<Modifier>,
}

/// Variant of [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeArgVariant {
    /// Just `value`.
    Unnamed { value: ast::Expr, value_stable_ptr: ast::ExprPtr },
    /// `name: value`.
    Named {
        value: ast::Expr,
        value_stable_ptr: ast::ExprPtr,
        name: SmolStr,
        name_stable_ptr: ast::TerminalIdentifierPtr,
    },
    /// `:name`
    FieldInitShorthand { name: SmolStr, name_stable_ptr: ast::TerminalIdentifierPtr },
}

/// Easier to digest representation of a [`ast::Modifier`] attached to [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Modifier {
    pub text: SmolStr,
    pub stable_ptr: ast::ModifierPtr,
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
            id: attr_id.as_syntax_node().get_text_without_trivia(db).into(),
            id_stable_ptr: attr_id.stable_ptr(),
            args: match attr_args {
                ast::OptionArgListParenthesized::ArgListParenthesized(ref attribute_args) => {
                    attribute_args
                        .arguments(db)
                        .elements(db)
                        .into_iter()
                        .map(|arg| AttributeArg::from_ast(arg, db))
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
    /// Build [`AttributeArg`] from [`ast::Arg`].
    pub fn from_ast(arg: ast::Arg, db: &dyn SyntaxGroup) -> AttributeArg {
        let variant = match arg.arg_clause(db) {
            ast::ArgClause::Unnamed(clause) => {
                let value = clause.value(db);
                AttributeArgVariant::Unnamed { value_stable_ptr: value.stable_ptr(), value }
            }
            ast::ArgClause::Named(clause) => {
                let identifier = clause.name(db);
                let value = clause.value(db);
                AttributeArgVariant::Named {
                    value_stable_ptr: value.stable_ptr(),
                    value,
                    name_stable_ptr: identifier.stable_ptr(),
                    name: identifier.text(db),
                }
            }
            ast::ArgClause::FieldInitShorthand(clause) => {
                let identifier = clause.name(db).name(db);
                AttributeArgVariant::FieldInitShorthand {
                    name_stable_ptr: identifier.stable_ptr(),
                    name: identifier.text(db),
                }
            }
        };

        let modifiers = arg
            .modifiers(db)
            .elements(db)
            .into_iter()
            .map(|modifier| Modifier::from(modifier, db))
            .collect();

        let arg_stable_ptr = arg.stable_ptr();
        AttributeArg { variant, arg, arg_stable_ptr, modifiers }
    }

    pub fn text(&self, db: &dyn SyntaxGroup) -> String {
        match &self.variant {
            AttributeArgVariant::Unnamed { value, .. } => {
                value.as_syntax_node().get_text_without_trivia(db)
            }
            AttributeArgVariant::Named { value, name, .. } => {
                format!("{}: {}", name, value.as_syntax_node().get_text_without_trivia(db))
            }
            AttributeArgVariant::FieldInitShorthand { name, .. } => {
                format!(":{}", name)
            }
        }
    }
}

impl Modifier {
    /// Build [`Modifier`] from [`ast::Modifier`].
    fn from(modifier: ast::Modifier, db: &dyn SyntaxGroup) -> Modifier {
        Modifier {
            stable_ptr: modifier.stable_ptr(),
            text: modifier.as_syntax_node().get_text(db).into(),
        }
    }
}
