use cairo_lang_utils::LookupIntern;
use smol_str::SmolStr;

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
use super::db::SyntaxGroup;
use super::ids::SyntaxStablePtrId;
use super::kind::SyntaxKind;
use super::{SyntaxNode, Terminal, TypedStablePtr, TypedSyntaxNode};
use crate::node::ast::{Attribute, AttributeList};
use crate::node::green::GreenNodeDetails;

#[cfg(test)]
#[path = "helpers_test.rs"]
mod test;

pub trait GetIdentifier {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl<'a> ast::UsePathLeafPtr<'a> {
    pub fn name_green(&self, _syntax_db: &dyn SyntaxGroup) -> Self {
        *self
    }
}
impl<'a> GetIdentifier for ast::UsePathLeafPtr<'a> {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let alias_clause_green = self.alias_clause_green(db).0;
        let green_node = alias_clause_green.lookup_intern(db);
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
        if ident != "self" {
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
impl<'a> GetIdentifier for ast::PathSegmentGreen<'a> {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let green_node = self.0.lookup_intern(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        let identifier = ast::TerminalIdentifierGreen(children[0]);
        identifier.identifier(db)
    }
}
impl<'a> GetIdentifier for ast::ExprPathGreen<'a> {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let green_node = self.0.lookup_intern(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        let segment_green = ast::ExprPathInnerGreen(*children.last().unwrap());
        segment_green.identifier(db)
    }
}

impl<'a> GetIdentifier for ast::ExprPathInnerGreen<'a> {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let green_node = self.0.lookup_intern(db);
        let children = match &green_node.details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        assert_eq!(children.len() & 1, 1, "Expected an odd number of elements in the path.");
        let segment_green = ast::PathSegmentGreen(*children.last().unwrap());
        segment_green.identifier(db)
    }
}
impl<'a> GetIdentifier for ast::TerminalIdentifierGreen<'a> {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match &self.0.lookup_intern(db).details {
            GreenNodeDetails::Token(_) => "Unexpected token".into(),
            GreenNodeDetails::Node { children, width: _ } => {
                TokenIdentifierGreen(children[1]).text(db)
            }
        }
    }
}
impl<'a> GetIdentifier for ast::ExprPath<'a> {
    /// Retrieves the identifier of the last segment of the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.segments(db).elements(db).last().cloned().unwrap().identifier(db)
    }
}

/// Helper trait for ast::PathSegment.
pub trait PathSegmentEx<'a> {
    fn identifier_ast(&self, db: &'a dyn SyntaxGroup) -> ast::TerminalIdentifier<'a>;
    fn generic_args(&self, db: &'a dyn SyntaxGroup) -> Option<Vec<ast::GenericArg<'a>>>;
}
impl<'a> PathSegmentEx<'a> for ast::PathSegment<'a> {
    /// Retrieves the identifier ast of a path segment.
    fn identifier_ast(&self, db: &'a dyn SyntaxGroup) -> ast::TerminalIdentifier<'a> {
        match self {
            ast::PathSegment::Simple(segment) => segment.ident(db),
            ast::PathSegment::WithGenericArgs(segment) => segment.ident(db),
            ast::PathSegment::Missing(missing_segment) => missing_segment.ident(db),
        }
    }
    fn generic_args(&self, db: &'a dyn SyntaxGroup) -> Option<Vec<ast::GenericArg<'a>>> {
        match self {
            ast::PathSegment::Simple(_) | ast::PathSegment::Missing(_) => None,
            ast::PathSegment::WithGenericArgs(segment) => {
                Some(segment.generic_args(db).generic_args(db).elements(db))
            }
        }
    }
}
impl<'a> GetIdentifier for ast::PathSegment<'a> {
    /// Retrieves the text of the segment (without the generic args).
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.identifier_ast(db).text(db)
    }
}
impl<'a> GetIdentifier for ast::Modifier<'a> {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match self {
            Modifier::Ref(r) => r.text(db),
            Modifier::Mut(m) => m.text(db),
        }
    }
}

/// Trait for ast object with a name terminal.
pub trait NameGreen<'a> {
    /// Returns the TerminalIdentifierGreen of the `name` node.
    fn name_green(self, db: &'a dyn SyntaxGroup) -> TerminalIdentifierGreen<'a>;
}

impl<'a> NameGreen<'a> for FunctionDeclarationGreen<'a> {
    fn name_green(self, db: &'a dyn SyntaxGroup) -> TerminalIdentifierGreen<'a> {
        TerminalIdentifierGreen(
            self.0.lookup_intern(db).children()[FunctionDeclaration::INDEX_NAME],
        )
    }
}

impl<'a> NameGreen<'a> for FunctionWithBodyPtr<'a> {
    fn name_green(self, db: &'a dyn SyntaxGroup) -> TerminalIdentifierGreen<'a> {
        self.declaration_green(db).name_green(db)
    }
}

impl<'a> NameGreen<'a> for ItemExternFunctionPtr<'a> {
    fn name_green(self, db: &'a dyn SyntaxGroup) -> TerminalIdentifierGreen<'a> {
        self.declaration_green(db).name_green(db)
    }
}

impl<'a> NameGreen<'a> for TraitItemFunctionPtr<'a> {
    fn name_green(self, db: &'a dyn SyntaxGroup) -> TerminalIdentifierGreen<'a> {
        self.declaration_green(db).name_green(db)
    }
}

/// Provides methods to extract a _name_ of AST objects.
pub trait HasName<'a> {
    /// Gets a [`TerminalIdentifier`] that represents a _name_ of this AST object.
    fn name(&self, db: &'a dyn SyntaxGroup) -> ast::TerminalIdentifier<'a>;
}

impl<'a> HasName<'a> for FunctionWithBody<'a> {
    fn name(&self, db: &'a dyn SyntaxGroup) -> TerminalIdentifier<'a> {
        self.declaration(db).name(db)
    }
}

impl<'a> HasName<'a> for ItemExternFunction<'a> {
    fn name(&self, db: &'a dyn SyntaxGroup) -> TerminalIdentifier<'a> {
        self.declaration(db).name(db)
    }
}

impl<'a> HasName<'a> for TraitItemFunction<'a> {
    fn name(&self, db: &'a dyn SyntaxGroup) -> TerminalIdentifier<'a> {
        self.declaration(db).name(db)
    }
}

impl<'a> HasName<'a> for UsePathLeaf<'a> {
    fn name(&self, db: &'a dyn SyntaxGroup) -> TerminalIdentifier<'a> {
        match self.alias_clause(db) {
            ast::OptionAliasClause::Empty(_) => self.ident(db).identifier_ast(db),
            ast::OptionAliasClause::AliasClause(alias) => alias.alias(db),
        }
    }
}

pub trait GenericParamEx<'a> {
    /// Returns the name of a generic param if one exists.
    fn name(&self, db: &'a dyn SyntaxGroup) -> Option<ast::TerminalIdentifier<'a>>;
}
impl<'a> GenericParamEx<'a> for ast::GenericParam<'a> {
    fn name(&self, db: &'a dyn SyntaxGroup) -> Option<ast::TerminalIdentifier<'a>> {
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
pub fn is_single_arg_attr(db: &dyn SyntaxGroup, attr: &Attribute, arg_name: &str) -> bool {
    match attr.arguments(db) {
        OptionArgListParenthesized::ArgListParenthesized(args) => {
            matches!(&args.arguments(db).elements(db)[..],
                    [arg] if arg.as_syntax_node().get_text_without_trivia(db) == arg_name)
        }
        OptionArgListParenthesized::Empty(_) => false,
    }
}

/// Trait for querying attributes of AST items.
pub trait QueryAttrs<'a> {
    /// Generic call `self.attributes(db).elements(db)`.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>>;

    /// Collect all attributes named exactly `attr` attached to this node.
    fn query_attr(&self, db: &'a dyn SyntaxGroup, attr: &str) -> Vec<Attribute<'a>> {
        self.attributes_elements(db)
            .into_iter()
            .filter(|a| a.attr(db).as_syntax_node().get_text_without_trivia(db) == attr)
            .collect()
    }

    /// Find first attribute named exactly `attr` attached do this node.
    fn find_attr(&self, db: &'a dyn SyntaxGroup, attr: &str) -> Option<Attribute<'a>> {
        self.query_attr(db, attr).into_iter().next()
    }

    /// Check if this node has an attribute named exactly `attr`.
    fn has_attr(&self, db: &'a dyn SyntaxGroup, attr: &str) -> bool {
        self.find_attr(db, attr).is_some()
    }

    /// Checks if the given object has an attribute with the given name and argument.
    fn has_attr_with_arg(&self, db: &'a dyn SyntaxGroup, attr_name: &str, arg_name: &str) -> bool {
        self.query_attr(db, attr_name).iter().any(|attr| is_single_arg_attr(db, attr, arg_name))
    }
}

impl<'a> QueryAttrs<'a> for ItemConstant<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemModule<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for FunctionWithBody<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemUse<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemExternFunction<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemExternType<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemTrait<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemImpl<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemImplAlias<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemStruct<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemEnum<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemTypeAlias<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for ItemMacroDeclaration<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for TraitItemFunction<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for TraitItemType<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for TraitItemConstant<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for TraitItemImpl<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}
impl<'a> QueryAttrs<'a> for TraitItem<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        match self {
            TraitItem::Function(item) => item.attributes_elements(db),
            TraitItem::Type(item) => item.attributes_elements(db),
            TraitItem::Constant(item) => item.attributes_elements(db),
            TraitItem::Impl(item) => item.attributes_elements(db),
            TraitItem::Missing(_) => vec![],
        }
    }
}

impl<'a> QueryAttrs<'a> for ItemInlineMacro<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for ModuleItem<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        match self {
            ModuleItem::Constant(item) => item.attributes_elements(db),
            ModuleItem::Module(item) => item.attributes_elements(db),
            ModuleItem::FreeFunction(item) => item.attributes_elements(db),
            ModuleItem::Use(item) => item.attributes_elements(db),
            ModuleItem::ExternFunction(item) => item.attributes_elements(db),
            ModuleItem::ExternType(item) => item.attributes_elements(db),
            ModuleItem::Trait(item) => item.attributes_elements(db),
            ModuleItem::Impl(item) => item.attributes_elements(db),
            ModuleItem::ImplAlias(item) => item.attributes_elements(db),
            ModuleItem::Struct(item) => item.attributes_elements(db),
            ModuleItem::Enum(item) => item.attributes_elements(db),
            ModuleItem::TypeAlias(item) => item.attributes_elements(db),
            ModuleItem::InlineMacro(item) => item.attributes_elements(db),
            ModuleItem::Missing(_) => vec![],
            ModuleItem::HeaderDoc(_) => vec![],
            ModuleItem::MacroDeclaration(macro_declaration) => {
                macro_declaration.attributes_elements(db)
            }
        }
    }
}

impl<'a> QueryAttrs<'a> for ImplItem<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        match self {
            ImplItem::Function(item) => item.attributes_elements(db),
            ImplItem::Type(item) => item.attributes_elements(db),
            ImplItem::Constant(item) => item.attributes_elements(db),
            ImplItem::Impl(item) => item.attributes_elements(db),
            ImplItem::Module(item) => item.attributes_elements(db),
            ImplItem::Use(item) => item.attributes_elements(db),
            ImplItem::ExternFunction(item) => item.attributes_elements(db),
            ImplItem::ExternType(item) => item.attributes_elements(db),
            ImplItem::Trait(item) => item.attributes_elements(db),
            ImplItem::Struct(item) => item.attributes_elements(db),
            ImplItem::Enum(item) => item.attributes_elements(db),
            ImplItem::Missing(_) => vec![],
        }
    }
}

impl<'a> QueryAttrs<'a> for AttributeList<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.elements(db)
    }
}
impl<'a> QueryAttrs<'a> for Member<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for Variant<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for StatementBreak<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for StatementContinue<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for StatementReturn<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for StatementLet<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

impl<'a> QueryAttrs<'a> for StatementExpr<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        self.attributes(db).elements(db)
    }
}

/// Allows querying attributes of a syntax node, any typed node which QueryAttrs is implemented for
/// should be added here.
impl<'a> QueryAttrs<'a> for SyntaxNode<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        match self.kind(db) {
            SyntaxKind::ItemConstant => {
                ast::ItemConstant::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemModule => {
                ast::ItemModule::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::FunctionWithBody => {
                ast::FunctionWithBody::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemUse => {
                ast::ItemUse::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemExternFunction => {
                ast::ItemExternFunction::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemExternType => {
                ast::ItemExternType::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemTrait => {
                ast::ItemTrait::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemImpl => {
                ast::ItemImpl::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemImplAlias => {
                ast::ItemImplAlias::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemStruct => {
                ast::ItemStruct::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemEnum => {
                ast::ItemEnum::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemTypeAlias => {
                ast::ItemTypeAlias::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::TraitItemFunction => {
                ast::TraitItemFunction::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::ItemInlineMacro => {
                ast::ItemInlineMacro::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::AttributeList => {
                ast::AttributeList::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::Member => ast::Member::from_syntax_node(db, *self).attributes_elements(db),
            SyntaxKind::Variant => {
                ast::Variant::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::StatementBreak => {
                ast::StatementBreak::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::StatementContinue => {
                ast::StatementContinue::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::StatementReturn => {
                ast::StatementReturn::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::StatementLet => {
                ast::StatementLet::from_syntax_node(db, *self).attributes_elements(db)
            }
            SyntaxKind::StatementExpr => {
                ast::StatementExpr::from_syntax_node(db, *self).attributes_elements(db)
            }
            _ => vec![],
        }
    }
}

impl<'a> QueryAttrs<'a> for Statement<'a> {
    fn attributes_elements(&self, db: &'a dyn SyntaxGroup) -> Vec<Attribute<'a>> {
        match self {
            Statement::Break(statement) => statement.attributes_elements(db),
            Statement::Continue(statement) => statement.attributes_elements(db),
            Statement::Return(statement) => statement.attributes_elements(db),
            Statement::Let(statement) => statement.attributes_elements(db),
            Statement::Expr(statement) => statement.attributes_elements(db),
            Statement::Item(statement) => statement.item(db).attributes_elements(db),
            Statement::Missing(_) => vec![],
        }
    }
}
pub trait WrappedArgListHelper<'a> {
    /// Pills the wrapping brackets to get the argument list. Returns None if `self` is `Missing`.
    fn arg_list(&'a self, db: &'a dyn SyntaxGroup) -> Option<ast::ArgList<'a>>;
    /// Gets the syntax node of the right wrapping bracket.
    fn right_bracket_syntax_node(&'a self, db: &'a dyn SyntaxGroup) -> SyntaxNode<'a>;
    /// Gets the syntax node of the left wrapping bracket.
    fn left_bracket_syntax_node(&'a self, db: &'a dyn SyntaxGroup) -> SyntaxNode<'a>;
    /// Gets a stable pointer to the left wrapping bracket.
    fn left_bracket_stable_ptr(&'a self, db: &'a dyn SyntaxGroup) -> SyntaxStablePtrId<'a>;
}
impl<'a> WrappedArgListHelper<'a> for WrappedArgList<'a> {
    fn arg_list(&'a self, db: &'a dyn SyntaxGroup) -> Option<ast::ArgList<'a>> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::BracketedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::BracedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::Missing(_) => None,
        }
    }

    fn right_bracket_syntax_node(&'a self, db: &'a dyn SyntaxGroup) -> SyntaxNode<'a> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.rparen(db).as_syntax_node(),
            WrappedArgList::BracketedArgList(args) => args.rbrack(db).as_syntax_node(),
            WrappedArgList::BracedArgList(args) => args.rbrace(db).as_syntax_node(),
            WrappedArgList::Missing(_) => self.as_syntax_node(),
        }
    }

    fn left_bracket_syntax_node(&'a self, db: &'a dyn SyntaxGroup) -> SyntaxNode<'a> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.lparen(db).as_syntax_node(),
            WrappedArgList::BracketedArgList(args) => args.lbrack(db).as_syntax_node(),
            WrappedArgList::BracedArgList(args) => args.lbrace(db).as_syntax_node(),
            WrappedArgList::Missing(_) => self.as_syntax_node(),
        }
    }

    fn left_bracket_stable_ptr(&'a self, db: &'a dyn SyntaxGroup) -> SyntaxStablePtrId<'a> {
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
    fn is_empty(&'a self, db: &'a dyn SyntaxGroup) -> bool;
}
impl<'a> WrappedGenericParamListHelper<'a> for ast::WrappedGenericParamList<'a> {
    fn is_empty(&'a self, db: &'a dyn SyntaxGroup) -> bool {
        self.generic_params(db).elements(db).is_empty()
    }
}

pub trait OptionWrappedGenericParamListHelper<'a> {
    /// Checks whether there are 0 generic parameters. True either when the generic params clause
    /// doesn't exist or when it's empty
    fn is_empty(&'a self, db: &'a dyn SyntaxGroup) -> bool;
}
impl<'a> OptionWrappedGenericParamListHelper<'a> for ast::OptionWrappedGenericParamList<'a> {
    fn is_empty(&'a self, db: &'a dyn SyntaxGroup) -> bool {
        match self {
            ast::OptionWrappedGenericParamList::Empty(_) => true,
            ast::OptionWrappedGenericParamList::WrappedGenericParamList(
                wrapped_generic_param_list,
            ) => wrapped_generic_param_list.is_empty(db),
        }
    }
}

/// Trait for getting the items of a body-item (an item that contains items), as a vector.
pub trait BodyItems<'a> {
    /// The type of an Item.
    type Item: 'a;
    /// Returns the items of the body-item as a vector.
    /// Use with caution, as this includes items that may be filtered out by plugins.
    /// Do note that plugins that directly run on this body-item without going/checking up on the
    /// syntax tree may assume that e.g. out-of-config items were already filtered out.
    /// Don't use on an item that is not the original plugin's context, unless you are sure that
    /// while traversing the AST to get to it from the original plugin's context, you did not go
    /// through another submodule.
    fn items_vec(&self, db: &'a dyn SyntaxGroup) -> Vec<Self::Item>;
}

impl<'a> BodyItems<'a> for ast::ModuleBody<'a> {
    type Item = ModuleItem<'a>;
    fn items_vec(&self, db: &'a dyn SyntaxGroup) -> Vec<ModuleItem<'a>> {
        self.items(db).elements(db)
    }
}

impl<'a> BodyItems<'a> for ast::TraitBody<'a> {
    type Item = TraitItem<'a>;
    fn items_vec(&self, db: &'a dyn SyntaxGroup) -> Vec<TraitItem<'a>> {
        self.items(db).elements(db)
    }
}

impl<'a> BodyItems<'a> for ast::ImplBody<'a> {
    type Item = ImplItem<'a>;
    fn items_vec(&self, db: &'a dyn SyntaxGroup) -> Vec<ImplItem<'a>> {
        self.items(db).elements(db)
    }
}

/// Helper trait for ast::UsePath.
pub trait UsePathEx<'a> {
    /// Retrieves the item of a use path.
    fn get_item(&self, db: &'a dyn SyntaxGroup) -> ast::ItemUse<'a>;
}
impl<'a> UsePathEx<'a> for ast::UsePath<'a> {
    fn get_item(&self, db: &'a dyn SyntaxGroup) -> ast::ItemUse<'a> {
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
    pub fn name_stable_ptr(&self, db: &'a dyn SyntaxGroup) -> SyntaxStablePtrId<'a> {
        self.name(db).stable_ptr(db).untyped()
    }
}

/// Helper trait for check syntactically if a type is dependent on a given identifier.
pub trait IsDependentType {
    /// Returns true if `self` is dependent on `identifier` in an internal type.
    /// For example given identifier `T` will return true for:
    /// `T`, `Array<T>`, `Array<Array<T>>`, `(T, felt252)`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_dependent_type(&self, db: &dyn SyntaxGroup, identifiers: &[&str]) -> bool;
}

impl<'a> IsDependentType for ast::ExprPath<'a> {
    fn is_dependent_type(&self, db: &dyn SyntaxGroup, identifiers: &[&str]) -> bool {
        let segments = self.segments(db).elements(db);
        if let [ast::PathSegment::Simple(arg_segment)] = &segments[..] {
            identifiers.contains(&arg_segment.ident(db).text(db).as_str())
        } else {
            segments.into_iter().any(|segment| {
                let ast::PathSegment::WithGenericArgs(with_generics) = segment else {
                    return false;
                };
                with_generics.generic_args(db).generic_args(db).elements(db).iter().any(|arg| {
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

impl<'a> IsDependentType for ast::Expr<'a> {
    fn is_dependent_type(&self, db: &dyn SyntaxGroup, identifiers: &[&str]) -> bool {
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
                .iter()
                .any(|expr| expr.is_dependent_type(db, identifiers)),
            ast::Expr::FixedSizeArray(arr) => {
                arr.exprs(db)
                    .elements(db)
                    .iter()
                    .any(|expr| expr.is_dependent_type(db, identifiers))
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
            | ast::Expr::Placeholder(_)
            | ast::Expr::Missing(_) => false,
        }
    }
}
