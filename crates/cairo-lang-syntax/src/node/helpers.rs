use cairo_lang_filesystem::ids::SmolStrId;
use salsa::Database;

use super::ast::{
    self, FunctionDeclaration, FunctionDeclarationGreen, FunctionWithBody, FunctionWithBodyPtr,
    ImplItem, ItemConstant, ItemEnum, ItemExternFunction, ItemExternFunctionPtr, ItemExternType,
    ItemImpl, ItemImplAlias, ItemInlineMacro, ItemMacroDeclaration, ItemModule, ItemStruct,
    ItemTrait, ItemTypeAlias, ItemUse, Member, Modifier, ModuleItem, OptionArgListParenthesized,
    Statement, StatementBreak, StatementContinue, StatementExpr, StatementLet, StatementReturn,
    TerminalIdentifier, TerminalIdentifierGreen, TokenIdentifierGreen, TraitItem,
    TraitItemConstant, TraitItemFunction, TraitItemFunctionPtr, TraitItemImpl, TraitItemType,
    UsePathLeaf, Variant, WrappedArgList,
};
use super::ids::SyntaxStablePtrId;
use super::kind::SyntaxKind;
use super::{SyntaxNode, Terminal, TypedStablePtr, TypedSyntaxNode};
use crate::node::ast::{Attribute, AttributeList};
use crate::node::green::GreenNodeDetails;

#[cfg(test)]
#[path = "helpers_test.rs"]
mod test;

pub trait GetIdentifier<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a>;
}
impl<'a> ast::UsePathLeafPtr<'a> {
    pub fn name_green(&self, _syntax_db: &dyn Database) -> Self {
        *self
    }
}
impl<'a> GetIdentifier<'a> for ast::UsePathLeafPtr<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let alias_clause_green = self.alias_clause_green(db).0;
        let green_node = alias_clause_green.long(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        if !children.is_empty() {
            return ast::TerminalIdentifierGreen(children[ast::AliasClause::INDEX_ALIAS])
                .identifier(db);
        }
        let ident_green = self.ident_green(db);
        let ident = ident_green.identifier(db);
        if ident.long(db) != "self" {
            return ident;
        }
        let mut node = self.0.lookup(db);
        loop {
            node = if let Some(parent) = node.parent(db) {
                parent
            } else {
                return ident;
            };
            if matches!(node.kind(db), SyntaxKind::UsePathSingle) {
                return ast::UsePathSingle::from_syntax_node(db, node).ident(db).identifier(db);
            }
        }
    }
}
impl<'a> GetIdentifier<'a> for ast::PathSegmentGreen<'a> {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let green_node = self.0.long(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        let identifier = ast::TerminalIdentifierGreen(children[0]);
        identifier.identifier(db)
    }
}
impl<'a> GetIdentifier<'a> for ast::ExprPathGreen<'a> {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let green_node = self.0.long(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        let segment_green = ast::ExprPathInnerGreen(*children.last().unwrap());
        segment_green.identifier(db)
    }
}

impl<'a> GetIdentifier<'a> for ast::ExprPathInnerGreen<'a> {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let green_node = self.0.long(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        assert_eq!(children.len() & 1, 1, "Expected an odd number of elements in the path.");
        let segment_green = ast::PathSegmentGreen(*children.last().unwrap());
        segment_green.identifier(db)
    }
}
impl<'a> GetIdentifier<'a> for ast::TerminalIdentifierGreen<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        match &self.0.long(db).details {
            GreenNodeDetails::Token(_) => panic!("Unexpected token"),
            GreenNodeDetails::Node { children, width: _ } => {
                TokenIdentifierGreen(children[1]).text(db)
            }
        }
    }
}
impl<'a> GetIdentifier<'a> for ast::ExprPath<'a> {
    /// Retrieves the identifier of the last segment of the path.
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        self.segments(db).elements(db).next_back().unwrap().identifier(db)
    }
}

/// Helper trait for ast::PathSegment.
pub trait PathSegmentEx<'a> {
    fn identifier_ast(&self, db: &'a dyn Database) -> ast::TerminalIdentifier<'a>;
    fn generic_args(&self, db: &'a dyn Database) -> Option<Vec<ast::GenericArg<'a>>>;
}
impl<'a> PathSegmentEx<'a> for ast::PathSegment<'a> {
    /// Retrieves the identifier ast of a path segment.
    fn identifier_ast(&self, db: &'a dyn Database) -> ast::TerminalIdentifier<'a> {
        match self {
            ast::PathSegment::Simple(segment) => segment.ident(db),
            ast::PathSegment::WithGenericArgs(segment) => segment.ident(db),
            ast::PathSegment::Missing(missing_segment) => missing_segment.ident(db),
        }
    }
    fn generic_args(&self, db: &'a dyn Database) -> Option<Vec<ast::GenericArg<'a>>> {
        match self {
            ast::PathSegment::Simple(_) | ast::PathSegment::Missing(_) => None,
            ast::PathSegment::WithGenericArgs(segment) => {
                Some(segment.generic_args(db).generic_args(db).elements_vec(db))
            }
        }
    }
}
impl<'a> GetIdentifier<'a> for ast::PathSegment<'a> {
    /// Retrieves the text of the segment (without the generic args).
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        match self {
            ast::PathSegment::Simple(segment) => segment.identifier(db),
            ast::PathSegment::WithGenericArgs(segment) => segment.identifier(db),
            ast::PathSegment::Missing(missing_segment) => missing_segment.identifier(db),
        }
    }
}
impl<'a> GetIdentifier<'a> for ast::PathSegmentSimple<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let green_node = self.as_syntax_node().green_node(db);
        let GreenNodeDetails::Node { children, .. } = &green_node.details else {
            panic!("Unexpected token");
        };
        TerminalIdentifierGreen(children[0]).identifier(db)
    }
}
impl<'a> GetIdentifier<'a> for ast::PathSegmentWithGenericArgs<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let green_node = self.as_syntax_node().green_node(db);
        let GreenNodeDetails::Node { children, .. } = &green_node.details else {
            panic!("Unexpected token");
        };
        TerminalIdentifierGreen(children[0]).identifier(db)
    }
}

impl<'a> GetIdentifier<'a> for ast::PathSegmentMissing<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        let green_node = self.as_syntax_node().green_node(db);
        let GreenNodeDetails::Node { children, .. } = &green_node.details else {
            panic!("Unexpected token");
        };
        TerminalIdentifierGreen(children[0]).identifier(db)
    }
}

impl<'a> GetIdentifier<'a> for ast::Modifier<'a> {
    fn identifier(&self, db: &'a dyn Database) -> SmolStrId<'a> {
        match self {
            Modifier::Ref(r) => r.text(db),
            Modifier::Mut(m) => m.text(db),
        }
    }
}

/// Trait for ast object with a name terminal.
pub trait NameGreen<'a> {
    /// Returns the TerminalIdentifierGreen of the `name` node.
    fn name_green(self, db: &'a dyn Database) -> TerminalIdentifierGreen<'a>;
}

impl<'a> NameGreen<'a> for FunctionDeclarationGreen<'a> {
    fn name_green(self, db: &'a dyn Database) -> TerminalIdentifierGreen<'a> {
        TerminalIdentifierGreen(self.0.long(db).children()[FunctionDeclaration::INDEX_NAME])
    }
}

impl<'a> NameGreen<'a> for FunctionWithBodyPtr<'a> {
    fn name_green(self, db: &'a dyn Database) -> TerminalIdentifierGreen<'a> {
        self.declaration_green(db).name_green(db)
    }
}

impl<'a> NameGreen<'a> for ItemExternFunctionPtr<'a> {
    fn name_green(self, db: &'a dyn Database) -> TerminalIdentifierGreen<'a> {
        self.declaration_green(db).name_green(db)
    }
}

impl<'a> NameGreen<'a> for TraitItemFunctionPtr<'a> {
    fn name_green(self, db: &'a dyn Database) -> TerminalIdentifierGreen<'a> {
        self.declaration_green(db).name_green(db)
    }
}

/// Provides methods to extract a _name_ of AST objects.
pub trait HasName<'a> {
    /// Gets a [`TerminalIdentifier`] that represents a _name_ of this AST object.
    fn name(&self, db: &'a dyn Database) -> ast::TerminalIdentifier<'a>;
}

impl<'a> HasName<'a> for FunctionWithBody<'a> {
    fn name(&self, db: &'a dyn Database) -> TerminalIdentifier<'a> {
        self.declaration(db).name(db)
    }
}

impl<'a> HasName<'a> for ItemExternFunction<'a> {
    fn name(&self, db: &'a dyn Database) -> TerminalIdentifier<'a> {
        self.declaration(db).name(db)
    }
}

impl<'a> HasName<'a> for TraitItemFunction<'a> {
    fn name(&self, db: &'a dyn Database) -> TerminalIdentifier<'a> {
        self.declaration(db).name(db)
    }
}

impl<'a> HasName<'a> for UsePathLeaf<'a> {
    fn name(&self, db: &'a dyn Database) -> TerminalIdentifier<'a> {
        match self.alias_clause(db) {
            ast::OptionAliasClause::Empty(_) => self.ident(db).identifier_ast(db),
            ast::OptionAliasClause::AliasClause(alias) => alias.alias(db),
        }
    }
}

pub trait GenericParamEx<'a> {
    /// Returns the name of a generic param if one exists.
    fn name(&self, db: &'a dyn Database) -> Option<ast::TerminalIdentifier<'a>>;
}
impl<'a> GenericParamEx<'a> for ast::GenericParam<'a> {
    fn name(&self, db: &'a dyn Database) -> Option<ast::TerminalIdentifier<'a>> {
        match self {
            ast::GenericParam::Type(t) => Some(t.name(db)),
            ast::GenericParam::Const(c) => Some(c.name(db)),
            ast::GenericParam::ImplNamed(i) => Some(i.name(db)),
            ast::GenericParam::ImplAnonymous(_) => None,
            ast::GenericParam::NegativeImpl(_) => None,
        }
    }
}

/// Checks if the given attribute has a single argument with the given name.
pub fn is_single_arg_attr(db: &dyn Database, attr: &Attribute<'_>, arg_name: &str) -> bool {
    match attr.arguments(db) {
        OptionArgListParenthesized::ArgListParenthesized(args) => {
            matches!(&args.arguments(db).elements_vec(db)[..],
                    [arg] if arg.as_syntax_node().get_text_without_trivia(db).long(db) == arg_name)
        }
        OptionArgListParenthesized::Empty(_) => false,
    }
}

/// Trait for querying attributes of AST items.
pub trait QueryAttrs<'a> {
    /// Generic call `self.attributes(db).elements(db)`, wrapped with `Option` for cases where the
    /// type does not support attributes.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>>;

    /// Generic call to `self.attributes(db).elements(db)`.
    fn attributes_elements(&self, db: &'a dyn Database) -> impl Iterator<Item = Attribute<'a>> {
        self.try_attributes(db).into_iter().flat_map(move |attrs| attrs.elements(db))
    }

    /// Collect all attributes named exactly `attr` attached to this node.
    fn query_attr(
        &self,
        db: &'a dyn Database,
        attr: &'a str,
    ) -> impl Iterator<Item = Attribute<'a>> {
        self.attributes_elements(db).filter(move |a| {
            a.attr(db).as_syntax_node().get_text_without_trivia(db).long(db) == attr
        })
    }

    /// Find first attribute named exactly `attr` attached to this node.
    fn find_attr(&self, db: &'a dyn Database, attr: &'a str) -> Option<Attribute<'a>> {
        self.query_attr(db, attr).next()
    }

    /// Check if this node has an attribute named exactly `attr`.
    fn has_attr(&self, db: &'a dyn Database, attr: &'a str) -> bool {
        self.find_attr(db, attr).is_some()
    }

    /// Checks if the given object has an attribute with the given name and argument.
    fn has_attr_with_arg(&self, db: &'a dyn Database, attr_name: &'a str, arg_name: &str) -> bool {
        self.query_attr(db, attr_name).any(|attr| is_single_arg_attr(db, &attr, arg_name))
    }
}

impl<'a> QueryAttrs<'a> for ItemConstant<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemModule<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for FunctionWithBody<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemUse<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemExternFunction<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemExternType<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemTrait<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemImpl<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemImplAlias<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemStruct<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemEnum<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemTypeAlias<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for ItemMacroDeclaration<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for TraitItemFunction<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for TraitItemType<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for TraitItemConstant<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for TraitItemImpl<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}
impl<'a> QueryAttrs<'a> for TraitItem<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        match self {
            TraitItem::Function(item) => Some(item.attributes(db)),
            TraitItem::Type(item) => Some(item.attributes(db)),
            TraitItem::Constant(item) => Some(item.attributes(db)),
            TraitItem::Impl(item) => Some(item.attributes(db)),
            TraitItem::Missing(_) => None,
        }
    }
}

impl<'a> QueryAttrs<'a> for ItemInlineMacro<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for ModuleItem<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        match self {
            ModuleItem::Constant(item) => Some(item.attributes(db)),
            ModuleItem::Module(item) => Some(item.attributes(db)),
            ModuleItem::FreeFunction(item) => Some(item.attributes(db)),
            ModuleItem::Use(item) => Some(item.attributes(db)),
            ModuleItem::ExternFunction(item) => Some(item.attributes(db)),
            ModuleItem::ExternType(item) => Some(item.attributes(db)),
            ModuleItem::Trait(item) => Some(item.attributes(db)),
            ModuleItem::Impl(item) => Some(item.attributes(db)),
            ModuleItem::ImplAlias(item) => Some(item.attributes(db)),
            ModuleItem::Struct(item) => Some(item.attributes(db)),
            ModuleItem::Enum(item) => Some(item.attributes(db)),
            ModuleItem::TypeAlias(item) => Some(item.attributes(db)),
            ModuleItem::InlineMacro(item) => Some(item.attributes(db)),
            ModuleItem::MacroDeclaration(macro_declaration) => {
                Some(macro_declaration.attributes(db))
            }
            ModuleItem::Missing(_) => None,
            ModuleItem::HeaderDoc(_) => None,
        }
    }
}

impl<'a> QueryAttrs<'a> for ImplItem<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        match self {
            ImplItem::Function(item) => Some(item.attributes(db)),
            ImplItem::Type(item) => Some(item.attributes(db)),
            ImplItem::Constant(item) => Some(item.attributes(db)),
            ImplItem::Impl(item) => Some(item.attributes(db)),
            ImplItem::Module(item) => Some(item.attributes(db)),
            ImplItem::Use(item) => Some(item.attributes(db)),
            ImplItem::ExternFunction(item) => Some(item.attributes(db)),
            ImplItem::ExternType(item) => Some(item.attributes(db)),
            ImplItem::Trait(item) => Some(item.attributes(db)),
            ImplItem::Struct(item) => Some(item.attributes(db)),
            ImplItem::Enum(item) => Some(item.attributes(db)),
            ImplItem::Missing(_) => None,
        }
    }
}

impl<'a> QueryAttrs<'a> for AttributeList<'a> {
    fn try_attributes(&self, _db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.clone())
    }
}
impl<'a> QueryAttrs<'a> for Member<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for Variant<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for StatementBreak<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for StatementContinue<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for StatementReturn<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for StatementLet<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

impl<'a> QueryAttrs<'a> for StatementExpr<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        Some(self.attributes(db))
    }
}

/// Allows querying attributes of a syntax node, any typed node which QueryAttrs is implemented for
/// should be added here.
impl<'a> QueryAttrs<'a> for SyntaxNode<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        match self.kind(db) {
            SyntaxKind::ItemConstant => {
                Some(ast::ItemConstant::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemModule => {
                Some(ast::ItemModule::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::FunctionWithBody => {
                Some(ast::FunctionWithBody::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemUse => Some(ast::ItemUse::from_syntax_node(db, *self).attributes(db)),
            SyntaxKind::ItemExternFunction => {
                Some(ast::ItemExternFunction::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemExternType => {
                Some(ast::ItemExternType::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemTrait => {
                Some(ast::ItemTrait::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemImpl => Some(ast::ItemImpl::from_syntax_node(db, *self).attributes(db)),
            SyntaxKind::ItemImplAlias => {
                Some(ast::ItemImplAlias::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemStruct => {
                Some(ast::ItemStruct::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemEnum => Some(ast::ItemEnum::from_syntax_node(db, *self).attributes(db)),
            SyntaxKind::ItemTypeAlias => {
                Some(ast::ItemTypeAlias::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::TraitItemFunction => {
                Some(ast::TraitItemFunction::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::ItemInlineMacro => {
                Some(ast::ItemInlineMacro::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::AttributeList => Some(ast::AttributeList::from_syntax_node(db, *self)),
            SyntaxKind::Member => Some(ast::Member::from_syntax_node(db, *self).attributes(db)),
            SyntaxKind::Variant => Some(ast::Variant::from_syntax_node(db, *self).attributes(db)),
            SyntaxKind::StatementBreak => {
                Some(ast::StatementBreak::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::StatementContinue => {
                Some(ast::StatementContinue::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::StatementReturn => {
                Some(ast::StatementReturn::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::StatementLet => {
                Some(ast::StatementLet::from_syntax_node(db, *self).attributes(db))
            }
            SyntaxKind::StatementExpr => {
                Some(ast::StatementExpr::from_syntax_node(db, *self).attributes(db))
            }
            _ => None,
        }
    }
}

impl<'a> QueryAttrs<'a> for Statement<'a> {
    fn try_attributes(&self, db: &'a dyn Database) -> Option<AttributeList<'a>> {
        match self {
            Statement::Break(statement) => Some(statement.attributes(db)),
            Statement::Continue(statement) => Some(statement.attributes(db)),
            Statement::Return(statement) => Some(statement.attributes(db)),
            Statement::Let(statement) => Some(statement.attributes(db)),
            Statement::Expr(statement) => Some(statement.attributes(db)),
            Statement::Item(statement) => statement.item(db).try_attributes(db),
            Statement::Missing(_) => None,
        }
    }
}
pub trait WrappedArgListHelper<'a> {
    /// Pills the wrapping brackets to get the argument list. Returns None if `self` is `Missing`.
    fn arg_list(&self, db: &'a dyn Database) -> Option<ast::ArgList<'a>>;
    /// Gets the syntax node of the right wrapping bracket.
    fn right_bracket_syntax_node(&self, db: &'a dyn Database) -> SyntaxNode<'a>;
    /// Gets the syntax node of the left wrapping bracket.
    fn left_bracket_syntax_node(&self, db: &'a dyn Database) -> SyntaxNode<'a>;
    /// Gets a stable pointer to the left wrapping bracket.
    fn left_bracket_stable_ptr(&self, db: &'a dyn Database) -> SyntaxStablePtrId<'a>;
}
impl<'a> WrappedArgListHelper<'a> for WrappedArgList<'a> {
    fn arg_list(&self, db: &'a dyn Database) -> Option<ast::ArgList<'a>> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::BracketedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::BracedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::Missing(_) => None,
        }
    }

    fn right_bracket_syntax_node(&self, db: &'a dyn Database) -> SyntaxNode<'a> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.rparen(db).as_syntax_node(),
            WrappedArgList::BracketedArgList(args) => args.rbrack(db).as_syntax_node(),
            WrappedArgList::BracedArgList(args) => args.rbrace(db).as_syntax_node(),
            WrappedArgList::Missing(_) => self.as_syntax_node(),
        }
    }

    fn left_bracket_syntax_node(&self, db: &'a dyn Database) -> SyntaxNode<'a> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.lparen(db).as_syntax_node(),
            WrappedArgList::BracketedArgList(args) => args.lbrack(db).as_syntax_node(),
            WrappedArgList::BracedArgList(args) => args.lbrace(db).as_syntax_node(),
            WrappedArgList::Missing(_) => self.as_syntax_node(),
        }
    }

    fn left_bracket_stable_ptr(&self, db: &'a dyn Database) -> SyntaxStablePtrId<'a> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.lparen(db).stable_ptr(db).untyped(),
            WrappedArgList::BracketedArgList(args) => args.lbrack(db).stable_ptr(db).untyped(),
            WrappedArgList::BracedArgList(args) => args.lbrace(db).stable_ptr(db).untyped(),
            WrappedArgList::Missing(_) => self.stable_ptr(db).untyped(),
        }
    }
}

pub trait WrappedGenericParamListHelper<'a> {
    /// Checks whether there are 0 generic parameters
    fn is_empty(&'a self, db: &'a dyn Database) -> bool;
}
impl<'a> WrappedGenericParamListHelper<'a> for ast::WrappedGenericParamList<'a> {
    fn is_empty(&'a self, db: &'a dyn Database) -> bool {
        self.generic_params(db).elements(db).len() == 0
    }
}

pub trait OptionWrappedGenericParamListHelper<'a> {
    /// Checks whether there are 0 generic parameters. True either when the generic params clause
    /// doesn't exist or when it's empty
    fn is_empty(&'a self, db: &'a dyn Database) -> bool;
}
impl<'a> OptionWrappedGenericParamListHelper<'a> for ast::OptionWrappedGenericParamList<'a> {
    fn is_empty(&'a self, db: &'a dyn Database) -> bool {
        match self {
            ast::OptionWrappedGenericParamList::Empty(_) => true,
            ast::OptionWrappedGenericParamList::WrappedGenericParamList(
                wrapped_generic_param_list,
            ) => wrapped_generic_param_list.is_empty(db),
        }
    }
}

/// Trait for getting the items of a body-item (an item that contains items), as an iterator.
pub trait BodyItems<'a> {
    /// The type of an Item.
    type Item: 'a;
    /// Returns the items of the body-item as an iterator.
    /// Use with caution, as this includes items that may be filtered out by plugins.
    /// Do note that plugins that directly run on this body-item without going/checking up on the
    /// syntax tree may assume that e.g. out-of-config items were already filtered out.
    /// Don't use on an item that is not the original plugin's context, unless you are sure that
    /// while traversing the AST to get to it from the original plugin's context, you did not go
    /// through another submodule.
    fn iter_items(&self, db: &'a dyn Database) -> impl Iterator<Item = Self::Item> + 'a;
}

impl<'a> BodyItems<'a> for ast::ModuleBody<'a> {
    type Item = ModuleItem<'a>;
    fn iter_items(&self, db: &'a dyn Database) -> impl Iterator<Item = Self::Item> + 'a {
        self.items(db).elements(db)
    }
}

impl<'a> BodyItems<'a> for ast::TraitBody<'a> {
    type Item = TraitItem<'a>;
    fn iter_items(&self, db: &'a dyn Database) -> impl Iterator<Item = Self::Item> + 'a {
        self.items(db).elements(db)
    }
}

impl<'a> BodyItems<'a> for ast::ImplBody<'a> {
    type Item = ImplItem<'a>;
    fn iter_items(&self, db: &'a dyn Database) -> impl Iterator<Item = Self::Item> + 'a {
        self.items(db).elements(db)
    }
}

/// Helper trait for ast::UsePath.
pub trait UsePathEx<'a> {
    /// Retrieves the item of a use path.
    fn get_item(&self, db: &'a dyn Database) -> ast::ItemUse<'a>;
}
impl<'a> UsePathEx<'a> for ast::UsePath<'a> {
    fn get_item(&self, db: &'a dyn Database) -> ast::ItemUse<'a> {
        let mut node = self.as_syntax_node();
        loop {
            let Some(parent) = node.parent(db) else {
                unreachable!("UsePath is not under an ItemUse.");
            };
            match parent.kind(db) {
                SyntaxKind::ItemUse => {
                    break ast::ItemUse::from_syntax_node(db, parent);
                }
                _ => node = parent,
            }
        }
    }
}

impl<'a> UsePathLeaf<'a> {
    /// Retrieves the stable pointer of the name of the leaf.
    pub fn name_stable_ptr(&self, db: &'a dyn Database) -> SyntaxStablePtrId<'a> {
        self.name(db).stable_ptr(db).untyped()
    }
}

/// Helper trait for check syntactically if a type is dependent on a given identifier.
pub trait IsDependentType<'db> {
    /// Returns true if `self` is dependent on `identifier` in an internal type.
    /// For example given identifier `T` will return true for:
    /// `T`, `Array<T>`, `Array<Array<T>>`, `(T, felt252)`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_dependent_type(&self, db: &'db dyn Database, identifiers: &[SmolStrId<'db>]) -> bool;
}

impl<'a> IsDependentType<'a> for ast::ExprPath<'a> {
    fn is_dependent_type(&self, db: &'a dyn Database, identifiers: &[SmolStrId<'a>]) -> bool {
        let segments = self.segments(db).elements_vec(db);
        if let [ast::PathSegment::Simple(arg_segment)] = &segments[..] {
            identifiers.contains(&arg_segment.identifier(db))
        } else {
            segments.into_iter().any(|segment| {
                let ast::PathSegment::WithGenericArgs(with_generics) = segment else {
                    return false;
                };
                with_generics.generic_args(db).generic_args(db).elements(db).any(|arg| {
                    let generic_arg_value = match arg {
                        ast::GenericArg::Named(named) => named.value(db),
                        ast::GenericArg::Unnamed(unnamed) => unnamed.value(db),
                    };
                    match generic_arg_value {
                        ast::GenericArgValue::Expr(arg_expr) => {
                            arg_expr.expr(db).is_dependent_type(db, identifiers)
                        }
                        ast::GenericArgValue::Underscore(_) => false,
                    }
                })
            })
        }
    }
}

impl<'a> IsDependentType<'a> for ast::Expr<'a> {
    fn is_dependent_type(&self, db: &dyn Database, identifiers: &[SmolStrId<'a>]) -> bool {
        match self {
            ast::Expr::Path(type_path) => type_path.is_dependent_type(db, identifiers),
            ast::Expr::Unary(unary) => unary.expr(db).is_dependent_type(db, identifiers),
            ast::Expr::Binary(binary) => {
                binary.lhs(db).is_dependent_type(db, identifiers)
                    || binary.rhs(db).is_dependent_type(db, identifiers)
            }
            ast::Expr::Tuple(tuple) => tuple
                .expressions(db)
                .elements(db)
                .any(|expr| expr.is_dependent_type(db, identifiers)),
            ast::Expr::FixedSizeArray(arr) => {
                arr.exprs(db).elements(db).any(|expr| expr.is_dependent_type(db, identifiers))
                    || match arr.size(db) {
                        ast::OptionFixedSizeArraySize::Empty(_) => false,
                        ast::OptionFixedSizeArraySize::FixedSizeArraySize(size) => {
                            size.size(db).is_dependent_type(db, identifiers)
                        }
                    }
            }
            ast::Expr::Literal(_)
            | ast::Expr::ShortString(_)
            | ast::Expr::String(_)
            | ast::Expr::False(_)
            | ast::Expr::True(_)
            | ast::Expr::Parenthesized(_)
            | ast::Expr::FunctionCall(_)
            | ast::Expr::StructCtorCall(_)
            | ast::Expr::Block(_)
            | ast::Expr::Match(_)
            | ast::Expr::If(_)
            | ast::Expr::Loop(_)
            | ast::Expr::While(_)
            | ast::Expr::For(_)
            | ast::Expr::Closure(_)
            | ast::Expr::ErrorPropagate(_)
            | ast::Expr::FieldInitShorthand(_)
            | ast::Expr::Indexed(_)
            | ast::Expr::InlineMacro(_)
            | ast::Expr::Missing(_) => false,
        }
    }
}
