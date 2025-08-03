use std::fmt;

use cairo_lang_debug::DebugWithDb;
use smol_str::SmolStr;

use crate::node::db::SyntaxGroup;
use crate::node::{Terminal, TypedSyntaxNode, ast};

/// Easier to digest representation of an [ast::Attribute].
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct Attribute<'a> {
    pub stable_ptr: ast::AttributePtr<'a>,
    pub id: SmolStr,
    pub id_stable_ptr: ast::ExprPathPtr<'a>,
    pub args: Vec<AttributeArg<'a>>,
    pub args_stable_ptr: ast::OptionArgListParenthesizedPtr<'a>,
}
impl<'a> Attribute<'a> {
    /// Checks if the given attribute has a single argument with the given name.
    pub fn is_single_unnamed_arg(&self, db: &'a dyn SyntaxGroup, arg_name: &str) -> bool {
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
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct AttributeArg<'a> {
    pub variant: AttributeArgVariant<'a>,
    pub arg: ast::Arg<'a>,
    pub modifiers: Vec<Modifier<'a>>,
}

/// Variant of [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub enum AttributeArgVariant<'a> {
    /// Just `value`.
    Unnamed(ast::Expr<'a>),
    /// `name: value`.
    Named { value: ast::Expr<'a>, name: NameInfo<'a> },
    /// `:name`
    FieldInitShorthand(NameInfo<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
/// The data on a name part of an argument.
pub struct NameInfo<'a> {
    /// The name of the argument.
    pub text: SmolStr,
    /// The stable pointer to the name.
    pub stable_ptr: ast::TerminalIdentifierPtr<'a>,
}
impl<'a> NameInfo<'a> {
    fn from_ast(name: &ast::TerminalIdentifier<'a>, db: &'a dyn SyntaxGroup) -> Self {
        NameInfo { text: name.text(db).into(), stable_ptr: name.stable_ptr(db) }
    }
}

/// Easier to digest representation of a [`ast::Modifier`] attached to [`AttributeArg`].
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct Modifier<'a> {
    pub text: SmolStr,
    pub stable_ptr: ast::ModifierPtr<'a>,
}

impl<'a> DebugWithDb<'a> for Attribute<'a> {
    type Db = dyn SyntaxGroup;
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &'a dyn SyntaxGroup) -> fmt::Result {
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

pub trait AttributeStructurize<'a> {
    /// Return the structured attribute for the given [ast::Attribute].
    fn structurize(self, db: &'a dyn SyntaxGroup) -> Attribute<'a>;
}

impl<'a> AttributeStructurize<'a> for ast::Attribute<'a> {
    fn structurize(self, db: &'a dyn SyntaxGroup) -> Attribute<'a> {
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

pub trait AttributeListStructurize<'a> {
    /// Return structured attributes for the given [ast::AttributeList].
    fn structurize(self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>>;
}

impl<'a> AttributeListStructurize<'a> for ast::AttributeList<'a> {
    fn structurize(self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        // TODO(ilya): Consider checking for attribute repetitions.
        self.elements(db).map(|attr| attr.structurize(db)).collect()
    }
}

impl<'a> AttributeArg<'a> {
    /// Build [`AttributeArg`] from [`ast::Arg`].
    pub fn from_ast(arg: ast::Arg<'a>, db: &'a dyn SyntaxGroup) -> AttributeArg<'a> {
        let variant = match arg.arg_clause(db) {
            ast::ArgClause::Unnamed(clause) => AttributeArgVariant::Unnamed(clause.value(db)),
            ast::ArgClause::Named(clause) => AttributeArgVariant::Named {
                value: clause.value(db),
                name: NameInfo::from_ast(&clause.name(db), db),
            },
            ast::ArgClause::FieldInitShorthand(clause) => AttributeArgVariant::FieldInitShorthand(
                NameInfo::from_ast(&clause.name(db).name(db), db),
            ),
        };

        let modifiers =
            arg.modifiers(db).elements(db).map(|modifier| Modifier::from(modifier, db)).collect();

        AttributeArg { variant, arg, modifiers }
    }

    pub fn text(&self, db: &dyn SyntaxGroup) -> String {
        match &self.variant {
            AttributeArgVariant::Unnamed(value) => {
                value.as_syntax_node().get_text_without_trivia(db).to_string()
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

impl<'a> Modifier<'a> {
    /// Build [`Modifier`] from [`ast::Modifier`].
    fn from(modifier: ast::Modifier<'a>, db: &'a dyn SyntaxGroup) -> Modifier<'a> {
        Modifier {
            stable_ptr: modifier.stable_ptr(db),
            text: modifier.as_syntax_node().get_text(db).into(),
        }
    }
}
