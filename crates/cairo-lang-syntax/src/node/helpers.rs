use cairo_lang_utils::LookupIntern;
use smol_str::SmolStr;

use super::ast::{
    self, FunctionDeclaration, FunctionDeclarationGreen, FunctionWithBody, FunctionWithBodyPtr,
    ImplItem, ItemConstant, ItemEnum, ItemExternFunction, ItemExternFunctionPtr, ItemExternType,
    ItemImpl, ItemImplAlias, ItemInlineMacro, ItemModule, ItemStruct, ItemTrait, ItemTypeAlias,
    ItemUse, Member, Modifier, ModuleItem, OptionArgListParenthesized, Statement, StatementBreak,
    StatementContinue, StatementExpr, StatementLet, StatementReturn, TerminalIdentifier,
    TerminalIdentifierGreen, TokenIdentifierGreen, TraitItem, TraitItemConstant, TraitItemFunction,
    TraitItemFunctionPtr, TraitItemImpl, TraitItemType, UsePathLeaf, Variant, WrappedArgList,
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
impl ast::UsePathLeafPtr {
    pub fn name_green(&self, _syntax_db: &dyn SyntaxGroup) -> Self {
        *self
    }
}
impl GetIdentifier for ast::UsePathLeafPtr {
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
        ident_green.identifier(db)
    }
}
impl GetIdentifier for ast::PathSegmentGreen {
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
impl GetIdentifier for ast::ExprPathGreen {
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
impl GetIdentifier for ast::TerminalIdentifierGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match &self.0.lookup_intern(db).details {
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
            self.0.lookup_intern(db).children()[FunctionDeclaration::INDEX_NAME],
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

/// Provides methods to extract a _name_ of AST objects.
pub trait HasName {
    /// Gets a [`TerminalIdentifier`] that represents a _name_ of this AST object.
    fn name(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier;
}

impl HasName for FunctionWithBody {
    fn name(&self, db: &dyn SyntaxGroup) -> TerminalIdentifier {
        self.declaration(db).name(db)
    }
}

impl HasName for ItemExternFunction {
    fn name(&self, db: &dyn SyntaxGroup) -> TerminalIdentifier {
        self.declaration(db).name(db)
    }
}

impl HasName for TraitItemFunction {
    fn name(&self, db: &dyn SyntaxGroup) -> TerminalIdentifier {
        self.declaration(db).name(db)
    }
}

impl HasName for UsePathLeaf {
    fn name(&self, db: &dyn SyntaxGroup) -> TerminalIdentifier {
        match self.alias_clause(db) {
            ast::OptionAliasClause::Empty(_) => self.ident(db).identifier_ast(db),
            ast::OptionAliasClause::AliasClause(alias) => alias.alias(db),
        }
    }
}

pub trait GenericParamEx {
    /// Returns the name of a generic param if one exists.
    fn name(&self, db: &dyn SyntaxGroup) -> Option<ast::TerminalIdentifier>;
}
impl GenericParamEx for ast::GenericParam {
    fn name(&self, db: &dyn SyntaxGroup) -> Option<ast::TerminalIdentifier> {
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
pub trait QueryAttrs {
    /// Generic call `self.attributes(db).elements(db)`.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute>;

    /// Collect all attributes named exactly `attr` attached to this node.
    fn query_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> Vec<Attribute> {
        self.attributes_elements(db)
            .into_iter()
            .filter(|a| a.attr(db).as_syntax_node().get_text_without_trivia(db) == attr)
            .collect()
    }

    /// Find first attribute named exactly `attr` attached do this node.
    fn find_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> Option<Attribute> {
        self.query_attr(db, attr).into_iter().next()
    }

    /// Check if this node has an attribute named exactly `attr`.
    fn has_attr(&self, db: &dyn SyntaxGroup, attr: &str) -> bool {
        self.find_attr(db, attr).is_some()
    }

    /// Checks if the given object has an attribute with the given name and argument.
    fn has_attr_with_arg(&self, db: &dyn SyntaxGroup, attr_name: &str, arg_name: &str) -> bool {
        self.query_attr(db, attr_name).iter().any(|attr| is_single_arg_attr(db, attr, arg_name))
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
impl QueryAttrs for ItemImplAlias {
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
impl QueryAttrs for TraitItemType {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for TraitItemConstant {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for TraitItemImpl {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}
impl QueryAttrs for TraitItem {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        match self {
            TraitItem::Function(item) => item.attributes_elements(db),
            TraitItem::Type(item) => item.attributes_elements(db),
            TraitItem::Constant(item) => item.attributes_elements(db),
            TraitItem::Impl(item) => item.attributes_elements(db),
            TraitItem::Missing(_) => vec![],
        }
    }
}

impl QueryAttrs for ItemInlineMacro {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for ModuleItem {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
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
        }
    }
}

impl QueryAttrs for ImplItem {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
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

impl QueryAttrs for AttributeList {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.elements(db)
    }
}
impl QueryAttrs for Member {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for Variant {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for StatementBreak {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for StatementContinue {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for StatementReturn {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for StatementLet {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

impl QueryAttrs for StatementExpr {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        self.attributes(db).elements(db)
    }
}

/// Allows querying attributes of a syntax node, any typed node which QueryAttrs is implemented for
/// should be added here.
impl QueryAttrs for SyntaxNode {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
        match self.kind(db) {
            SyntaxKind::ItemConstant => {
                ast::ItemConstant::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemModule => {
                ast::ItemModule::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::FunctionWithBody => {
                ast::FunctionWithBody::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemUse => {
                ast::ItemUse::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemExternFunction => {
                ast::ItemExternFunction::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemExternType => {
                ast::ItemExternType::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemTrait => {
                ast::ItemTrait::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemImpl => {
                ast::ItemImpl::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemImplAlias => {
                ast::ItemImplAlias::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemStruct => {
                ast::ItemStruct::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemEnum => {
                ast::ItemEnum::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemTypeAlias => {
                ast::ItemTypeAlias::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::TraitItemFunction => {
                ast::TraitItemFunction::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::ItemInlineMacro => {
                ast::ItemInlineMacro::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::AttributeList => {
                ast::AttributeList::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::Member => {
                ast::Member::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::Variant => {
                ast::Variant::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::StatementBreak => {
                ast::StatementBreak::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::StatementContinue => {
                ast::StatementContinue::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::StatementReturn => {
                ast::StatementReturn::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::StatementLet => {
                ast::StatementLet::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            SyntaxKind::StatementExpr => {
                ast::StatementExpr::from_syntax_node(db, self.clone()).attributes_elements(db)
            }
            _ => vec![],
        }
    }
}

impl QueryAttrs for Statement {
    fn attributes_elements(&self, db: &dyn SyntaxGroup) -> Vec<Attribute> {
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
pub trait WrappedArgListHelper {
    /// Pills the wrapping brackets to get the argument list. Returns None if `self` is `Missing`.
    fn arg_list(&self, db: &dyn SyntaxGroup) -> Option<ast::ArgList>;
    /// Gets the syntax node of the right wrapping bracket.
    fn right_bracket_syntax_node(&self, db: &dyn SyntaxGroup) -> SyntaxNode;
    /// Gets the syntax node of the left wrapping bracket.
    fn left_bracket_syntax_node(&self, db: &dyn SyntaxGroup) -> SyntaxNode;
    /// Gets a stable pointer to the left wrapping bracket.
    fn left_bracket_stable_ptr(&self, db: &dyn SyntaxGroup) -> SyntaxStablePtrId;
}
impl WrappedArgListHelper for WrappedArgList {
    fn arg_list(&self, db: &dyn SyntaxGroup) -> Option<ast::ArgList> {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::BracketedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::BracedArgList(args) => Some(args.arguments(db)),
            WrappedArgList::Missing(_) => None,
        }
    }

    fn right_bracket_syntax_node(&self, db: &dyn SyntaxGroup) -> SyntaxNode {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.rparen(db).as_syntax_node(),
            WrappedArgList::BracketedArgList(args) => args.rbrack(db).as_syntax_node(),
            WrappedArgList::BracedArgList(args) => args.rbrace(db).as_syntax_node(),
            WrappedArgList::Missing(_) => self.as_syntax_node(),
        }
    }

    fn left_bracket_syntax_node(&self, db: &dyn SyntaxGroup) -> SyntaxNode {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => args.lparen(db).as_syntax_node(),
            WrappedArgList::BracketedArgList(args) => args.lbrack(db).as_syntax_node(),
            WrappedArgList::BracedArgList(args) => args.lbrace(db).as_syntax_node(),
            WrappedArgList::Missing(_) => self.as_syntax_node(),
        }
    }

    fn left_bracket_stable_ptr(&self, db: &dyn SyntaxGroup) -> SyntaxStablePtrId {
        match self {
            WrappedArgList::ParenthesizedArgList(args) => (&args.lparen(db)).into(),
            WrappedArgList::BracketedArgList(args) => (&args.lbrack(db)).into(),
            WrappedArgList::BracedArgList(args) => (&args.lbrace(db)).into(),
            WrappedArgList::Missing(_) => self.into(),
        }
    }
}

pub trait WrappedGenericParamListHelper {
    /// Checks whether there are 0 generic parameters
    fn is_empty(&self, db: &dyn SyntaxGroup) -> bool;
}
impl WrappedGenericParamListHelper for ast::WrappedGenericParamList {
    fn is_empty(&self, db: &dyn SyntaxGroup) -> bool {
        self.generic_params(db).elements(db).is_empty()
    }
}

pub trait OptionWrappedGenericParamListHelper {
    /// Checks whether there are 0 generic parameters. True either when the generic params clause
    /// doesn't exist or when it's empty
    fn is_empty(&self, db: &dyn SyntaxGroup) -> bool;
}
impl OptionWrappedGenericParamListHelper for ast::OptionWrappedGenericParamList {
    fn is_empty(&self, db: &dyn SyntaxGroup) -> bool {
        match self {
            ast::OptionWrappedGenericParamList::Empty(_) => true,
            ast::OptionWrappedGenericParamList::WrappedGenericParamList(
                wrapped_generic_param_list,
            ) => wrapped_generic_param_list.is_empty(db),
        }
    }
}

/// Trait for getting the items of a body-item (an item that contains items), as a vector.
pub trait BodyItems {
    /// The type of an Item.
    type Item;
    /// Returns the items of the body-item as a vector.
    /// Use with caution, as this includes items that may be filtered out by plugins.
    /// Do note that plugins that directly run on this body-item without going/checking up on the
    /// syntax tree may assume that e.g. out-of-config items were already filtered out.
    /// Don't use on an item that is not the original plugin's context, unless you are sure that
    /// while traversing the AST to get to it from the original plugin's context, you did not go
    /// through another submodule.
    fn items_vec(&self, db: &dyn SyntaxGroup) -> Vec<Self::Item>;
}

impl BodyItems for ast::ModuleBody {
    type Item = ModuleItem;
    fn items_vec(&self, db: &dyn SyntaxGroup) -> Vec<ModuleItem> {
        self.items(db).elements(db)
    }
}

impl BodyItems for ast::TraitBody {
    type Item = TraitItem;
    fn items_vec(&self, db: &dyn SyntaxGroup) -> Vec<TraitItem> {
        self.items(db).elements(db)
    }
}

impl BodyItems for ast::ImplBody {
    type Item = ImplItem;
    fn items_vec(&self, db: &dyn SyntaxGroup) -> Vec<ImplItem> {
        self.items(db).elements(db)
    }
}

/// Helper trait for ast::UsePath.
pub trait UsePathEx {
    /// Retrieves the item of a use path.
    fn get_item(&self, db: &dyn SyntaxGroup) -> ast::ItemUse;
}
impl UsePathEx for ast::UsePath {
    fn get_item(&self, db: &dyn SyntaxGroup) -> ast::ItemUse {
        let mut node = self.as_syntax_node();
        loop {
            let Some(parent) = node.parent() else {
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

impl UsePathLeaf {
    /// Retrieves the stable pointer of the name of the leaf.
    pub fn name_stable_ptr(&self, db: &dyn SyntaxGroup) -> SyntaxStablePtrId {
        match self.alias_clause(db) {
            ast::OptionAliasClause::Empty(_) => self.ident(db).stable_ptr().untyped(),
            ast::OptionAliasClause::AliasClause(alias) => alias.alias(db).stable_ptr().untyped(),
        }
    }
}
