use std::fmt;

use cairo_lang_debug::DebugWithDb;
use smol_str::SmolStr;

use crate::node::db::SyntaxGroup;
use crate::node::{Terminal, TypedSyntaxNode, ast};

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
                AttributeArgVariant::Unnamed(value) => {
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
    pub modifiers: Vec<Modifier>,
}

/// Variant of [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeArgVariant {
    /// Just `value`.
    Unnamed(ast::Expr),
    /// `name: value`.
    Named { value: ast::Expr, name: NameInfo },
    /// `:name`
    FieldInitShorthand(NameInfo),
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// The data on a name part of an argument.
pub struct NameInfo {
    /// The name of the argument.
    pub text: SmolStr,
    /// The stable pointer to the name.
    pub stable_ptr: ast::TerminalIdentifierPtr,
}
impl NameInfo {
    fn from_ast(name: ast::TerminalIdentifier, db: &dyn SyntaxGroup) -> Self {
        NameInfo { text: name.text(db), stable_ptr: name.stable_ptr(db) }
    }
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
            for arg in &self.args {
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
            stable_ptr: self.stable_ptr(db),
            id: attr_id.as_syntax_node().get_text_without_trivia(db).into(),
            id_stable_ptr: attr_id.stable_ptr(db),
            args: match attr_args {
                ast::OptionArgListParenthesized::ArgListParenthesized(ref attribute_args) => {
                    attribute_args
                        .arguments(db)
                        .elements(db)
                        .map(|arg| AttributeArg::from_ast(arg, db))
                        .collect()
                }
                ast::OptionArgListParenthesized::Empty(_) => vec![],
            },
            args_stable_ptr: attr_args.stable_ptr(db),
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
        self.elements(db).map(|attr| attr.structurize(db)).collect()
    }
}

impl AttributeArg {
    /// Build [`AttributeArg`] from [`ast::Arg`].
    pub fn from_ast(arg: ast::Arg, db: &dyn SyntaxGroup) -> AttributeArg {
        let variant = match arg.arg_clause(db) {
            ast::ArgClause::Unnamed(clause) => AttributeArgVariant::Unnamed(clause.value(db)),
            ast::ArgClause::Named(clause) => AttributeArgVariant::Named {
                value: clause.value(db),
                name: NameInfo::from_ast(clause.name(db), db),
            },
            ast::ArgClause::FieldInitShorthand(clause) => AttributeArgVariant::FieldInitShorthand(
                NameInfo::from_ast(clause.name(db).name(db), db),
            ),
        };

        let modifiers =
            arg.modifiers(db).elements(db).map(|modifier| Modifier::from(modifier, db)).collect();

        AttributeArg { variant, arg, modifiers }
    }

    pub fn text(&self, db: &dyn SyntaxGroup) -> String {
        match &self.variant {
            AttributeArgVariant::Unnamed(value) => {
                value.as_syntax_node().get_text_without_trivia(db)
            }
            AttributeArgVariant::Named { value, name } => {
                format!("{}: {}", name.text, value.as_syntax_node().get_text_without_trivia(db))
            }
            AttributeArgVariant::FieldInitShorthand(name) => {
                format!(":{}", name.text)
            }
        }
    }
}

impl Modifier {
    /// Build [`Modifier`] from [`ast::Modifier`].
    fn from(modifier: ast::Modifier, db: &dyn SyntaxGroup) -> Modifier {
        Modifier {
            stable_ptr: modifier.stable_ptr(db),
            text: modifier.as_syntax_node().get_text(db).into(),
        }
    }
}
