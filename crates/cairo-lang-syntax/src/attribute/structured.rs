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
    pub id_stable_ptr: ast::TerminalIdentifierPtr,
    pub args: Vec<ast::Expr>,
    pub args_stable_ptr: ast::OptionAttributeArgsPtr,
}

impl<'a> DebugWithDb<dyn SyntaxGroup + 'a> for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn SyntaxGroup + 'a)) -> fmt::Result {
        write!(f, r#"Attribute {{ id: "{}""#, self.id)?;
        if !self.args.is_empty() {
            write!(f, ", args: [")?;
            for arg in self.args.iter() {
                write!(f, "{:?}, ", arg.as_syntax_node().get_text(db))?;
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
        let attr_args = self.args(db);

        Attribute {
            stable_ptr: self.stable_ptr(),
            id: attr_id.text(db),
            id_stable_ptr: attr_id.stable_ptr(),
            args: match attr_args {
                ast::OptionAttributeArgs::AttributeArgs(ref attribute_args) => {
                    attribute_args.arg_list(db).elements(db)
                }
                ast::OptionAttributeArgs::Empty(_) => vec![],
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
