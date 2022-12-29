use smol_str::SmolStr;

use super::ast::{
    self, FunctionDeclaration, FunctionDeclarationGreen, Item, ItemEnum, ItemExternFunction,
    ItemExternFunctionPtr, ItemExternType, ItemFreeFunction, ItemFreeFunctionPtr, ItemImpl,
    ItemModule, ItemStruct, ItemTrait, ItemTypeAlias, ItemUse, Modifier, TerminalIdentifierGreen,
    TokenIdentifierGreen, TraitItemFunctionPtr,
};
use super::db::SyntaxGroup;
use super::kind::SyntaxKind;
use super::Terminal;
use crate::node::green::GreenNodeDetails;

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
        ast::TerminalIdentifierGreen(*children.last().unwrap()).identifier(db)
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
impl GetIdentifier for ast::ParamNameGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let green_node = db.lookup_intern_green(self.0);

        match green_node.details {
            GreenNodeDetails::Token(_) => "Unexpected token".into(),
            GreenNodeDetails::Node { .. } => match green_node.kind {
                SyntaxKind::TerminalIdentifier => TerminalIdentifierGreen(self.0).identifier(db),
                // All '_' params will be named with the same name...
                SyntaxKind::TerminalUnderscore => "_".into(),
                _ => "Unexpected identifier for param name".into(),
            },
        }
    }
}

/// Helper trait for ast::PathSegment.
pub trait PathSegmentEx {
    fn identifier_ast(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier;
}
impl PathSegmentEx for ast::PathSegment {
    /// Retrieves the identifier ast of a path segment.
    fn identifier_ast(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier {
        match self {
            ast::PathSegment::Simple(segment) => segment.ident(db),
            ast::PathSegment::WithGenericArgs(segment) => segment.ident(db),
        }
    }
}
impl GetIdentifier for ast::PathSegment {
    /// Retrieves the text of the segment (without the generic args).
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.identifier_ast(db).text(db)
    }
}
impl GetIdentifier for ast::ParamName {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match self {
            ast::ParamName::Underscore(_) => "_".into(),
            ast::ParamName::Name(name) => name.text(db),
        }
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

impl NameGreen for ItemFreeFunctionPtr {
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
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool;
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool;
}
impl QueryAttrs for ItemModule {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemFreeFunction {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemUse {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemExternFunction {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemExternType {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemTrait {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemImpl {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemStruct {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemEnum {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}
impl QueryAttrs for ItemTypeAlias {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.attributes(db).elements(db).iter().any(|a| a.attr(db).text(db) == attr)
    }
    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self.attributes(db).elements(db).last() {
            None => false,
            Some(last_attr) => last_attr.attr(db).text(db) == attr,
        }
    }
}

impl QueryAttrs for Item {
    fn any_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self {
            ast::Item::Module(item) => item.any_attr(db, attr),
            ast::Item::FreeFunction(item) => item.any_attr(db, attr),
            ast::Item::Use(item) => item.any_attr(db, attr),
            ast::Item::ExternFunction(item) => item.any_attr(db, attr),
            ast::Item::ExternType(item) => item.any_attr(db, attr),
            ast::Item::Trait(item) => item.any_attr(db, attr),
            ast::Item::Impl(item) => item.any_attr(db, attr),
            ast::Item::Struct(item) => item.any_attr(db, attr),
            ast::Item::Enum(item) => item.any_attr(db, attr),
            ast::Item::TypeAlias(item) => item.any_attr(db, attr),
        }
    }

    fn last_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        match self {
            ast::Item::Module(item) => item.last_attr(db, attr),
            ast::Item::FreeFunction(item) => item.last_attr(db, attr),
            ast::Item::Use(item) => item.last_attr(db, attr),
            ast::Item::ExternFunction(item) => item.last_attr(db, attr),
            ast::Item::ExternType(item) => item.last_attr(db, attr),
            ast::Item::Trait(item) => item.last_attr(db, attr),
            ast::Item::Impl(item) => item.last_attr(db, attr),
            ast::Item::Struct(item) => item.last_attr(db, attr),
            ast::Item::Enum(item) => item.last_attr(db, attr),
            ast::Item::TypeAlias(item) => item.last_attr(db, attr),
        }
    }
}
