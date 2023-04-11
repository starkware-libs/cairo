use smol_str::SmolStr;

use super::ast::{
    self, FunctionDeclaration, FunctionDeclarationGreen, FunctionWithBody, FunctionWithBodyPtr,
    Item, ItemConstant, ItemEnum, ItemExternFunction, ItemExternFunctionPtr, ItemExternType,
    ItemImpl, ItemModule, ItemStruct, ItemTrait, ItemTypeAlias, ItemUse, Modifier,
    TerminalIdentifierGreen, TokenIdentifierGreen, TraitItemFunction, TraitItemFunctionPtr,
};
use super::db::SyntaxGroup;
use super::Terminal;
use crate::node::ast::{Attribute, AttributeList};
use crate::node::green::GreenNodeDetails;

#[cfg(test)]
#[path = "helpers_test.rs"]
mod test;

pub trait GetIdentifier {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl GetIdentifier for ast::ExprPathGreen {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let children = match db.lookup_intern_green(self.0).details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        assert_eq!(children.len() & 1, 1, "Expected an odd number of elements in the path.");
        let segment_green = ast::PathSegmentGreen(*children.last().unwrap());
        let children = match db.lookup_intern_green(segment_green.0).details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        let identifier = ast::TerminalIdentifierGreen(children[0]);
        identifier.identifier(db)
    }
}
impl GetIdentifier for ast::TerminalIdentifierGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match db.lookup_intern_green(self.0).details {
            GreenNodeDetails::Token(_) => "Unexpected token".into(),
            GreenNodeDetails::Node { children, width: _ } => {
                TokenIdentifierGreen(children[1]).text(db)
            }
        }
    }
}
impl GetIdentifier for ast::ExprPath {
    /// Retrieves the identifier of the last segment of the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.elements(db).last().cloned().unwrap().identifier(db)
    }
}

/// Helper trait for ast::PathSegment.
pub trait PathSegmentEx {
    fn identifier_ast(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier;
    fn generic_args(&self, db: &dyn SyntaxGroup) -> Option<Vec<ast::GenericArg>>;
}
impl PathSegmentEx for ast::PathSegment {
    /// Retrieves the identifier ast of a path segment.
    fn identifier_ast(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier {
        match self {
            ast::PathSegment::Simple(segment) => segment.ident(db),
            ast::PathSegment::WithGenericArgs(segment) => segment.ident(db),
        }
    }
    fn generic_args(&self, db: &dyn SyntaxGroup) -> Option<Vec<ast::GenericArg>> {
        match self {
            ast::PathSegment::Simple(_) => None,
            ast::PathSegment::WithGenericArgs(segment) => {
                Some(segment.generic_args(db).generic_args(db).elements(db))
            }
        }
    }
}
impl GetIdentifier for ast::PathSegment {
    /// Retrieves the text of the segment (without the generic args).
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.identifier_ast(db).text(db)
    }
}
impl GetIdentifier for ast::Modifier {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match self {
            Modifier::Ref(r) => r.text(db),
            Modifier::Mut(m) => m.text(db),
        }
    }
}

/// Trait for ast object with a name terminal.
pub trait NameGreen {
    /// Returns the TerminalIdentifierGreen of the `name` node.
    fn name_green(self, db: &dyn SyntaxGroup) -> TerminalIdentifierGreen;
}

impl NameGreen for FunctionDeclarationGreen {
    fn name_green(self, db: &dyn SyntaxGroup) -> TerminalIdentifierGreen {
        TerminalIdentifierGreen(
            db.lookup_intern_green(self.0).children()[FunctionDeclaration::INDEX_NAME],
        )
    }
}

impl NameGreen for FunctionWithBodyPtr {
    fn name_green(self, db: &dyn SyntaxGroup) -> TerminalIdentifierGreen {
        self.declaration_green(db).name_green(db)
    }
}

impl NameGreen for ItemExternFunctionPtr {
    fn name_green(self, db: &dyn SyntaxGroup) -> TerminalIdentifierGreen {
        self.declaration_green(db).name_green(db)
    }
}

impl NameGreen for TraitItemFunctionPtr {
    fn name_green(self, db: &dyn SyntaxGroup) -> TerminalIdentifierGreen {
        self.declaration_green(db).name_green(db)
    }
}

/// Trait for querying attributes of AST items.
pub trait QueryAttrs {
    /// Generic call `self.attributes(db).elements(db)`.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute>;

    /// Collect all attributes named exactly `attr` attached to this node.
    fn query_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> Vec<Attribute> {
        self.attributes_elements(db).into_iter().filter(|a| a.attr(db).text(db) == attr).collect()
    }

    /// Find first attribute named exactly `attr` attached do this node.
    fn find_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> Option<Attribute> {
        self.query_attr(db, attr).into_iter().next()
    }

    /// Check if this node has an attribute named exactly `attr`.
    fn has_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.find_attr(db, attr).is_some()
    }
}
impl QueryAttrs for ItemConstant {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemModule {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for FunctionWithBody {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemUse {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemExternFunction {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemExternType {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemTrait {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemImpl {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemStruct {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemEnum {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for ItemTypeAlias {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for TraitItemFunction {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for Item {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        match self {
            Item::Constant(item) => item.attributes_elements(db),
            Item::Module(item) => item.attributes_elements(db),
            Item::FreeFunction(item) => item.attributes_elements(db),
            Item::Use(item) => item.attributes_elements(db),
            Item::ExternFunction(item) => item.attributes_elements(db),
            Item::ExternType(item) => item.attributes_elements(db),
            Item::Trait(item) => item.attributes_elements(db),
            Item::Impl(item) => item.attributes_elements(db),
            Item::Struct(item) => item.attributes_elements(db),
            Item::Enum(item) => item.attributes_elements(db),
            Item::TypeAlias(item) => item.attributes_elements(db),
        }
    }
}

impl QueryAttrs for AttributeList {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.elements(db)
    }
}
