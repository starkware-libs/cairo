use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_semantic::resolve::{
    CRATE_KW, ResolvedConcreteItem, ResolvedGenericItem, SELF_TYPE_KW, SUPER_KW,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::grandparent_kind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::Upcast;
use lsp_types::SemanticTokenType;

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup};

#[allow(dead_code)]
pub enum SemanticTokenKind {
    Namespace,
    Class,
    Enum,
    Interface,
    Struct,
    TypeParameter,
    Type,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Function,
    Comment,
    Keyword,
    Operator,
    Number,
    String,
    Field,
    Annotation,
    InlineMacro,
    GenericParamImpl,
}
impl SemanticTokenKind {
    pub fn from_syntax_node(db: &AnalysisDatabase, mut node: SyntaxNode) -> Option<Self> {
        let syntax_db = db.upcast();
        let mut expr_path_ptr = None;
        let kind = node.kind(syntax_db);
        match kind {
            SyntaxKind::TokenIdentifier => {}
            _ if kind.is_keyword_token() => return Some(SemanticTokenKind::Keyword),
            SyntaxKind::TokenLiteralNumber => return Some(SemanticTokenKind::Number),
            SyntaxKind::TokenNot
                if matches!(
                    grandparent_kind(syntax_db, &node),
                    Some(SyntaxKind::ExprInlineMacro | SyntaxKind::ItemInlineMacro)
                ) =>
            {
                return Some(SemanticTokenKind::InlineMacro);
            }
            SyntaxKind::TokenPlus
                if matches!(
                    grandparent_kind(syntax_db, &node),
                    Some(SyntaxKind::GenericParamImplAnonymous)
                ) =>
            {
                return Some(SemanticTokenKind::GenericParamImpl);
            }
            SyntaxKind::TokenAnd
            | SyntaxKind::TokenAndAnd
            | SyntaxKind::TokenOr
            | SyntaxKind::TokenOrOr
            | SyntaxKind::TokenEqEq
            | SyntaxKind::TokenNeq
            | SyntaxKind::TokenGE
            | SyntaxKind::TokenGT
            | SyntaxKind::TokenLE
            | SyntaxKind::TokenLT
            | SyntaxKind::TokenNot
            | SyntaxKind::TokenPlus
            | SyntaxKind::TokenMinus
            | SyntaxKind::TokenMul
            | SyntaxKind::TokenDiv
            | SyntaxKind::TokenMod => return Some(SemanticTokenKind::Operator),
            SyntaxKind::TokenSingleLineComment => return Some(SemanticTokenKind::Comment),
            SyntaxKind::TokenShortString | SyntaxKind::TokenString => {
                return Some(SemanticTokenKind::String);
            }
            _ => return None,
        };
        node = node.parent().unwrap();
        let identifier = ast::TerminalIdentifier::from_syntax_node(syntax_db, node.clone());

        if [SUPER_KW, SELF_TYPE_KW, CRATE_KW].contains(&identifier.text(syntax_db).as_str()) {
            return Some(SemanticTokenKind::Keyword);
        }

        let parent_node = node.parent().unwrap();
        let parent_kind = parent_node.kind(syntax_db);
        match parent_kind {
            SyntaxKind::ItemInlineMacro => return Some(SemanticTokenKind::InlineMacro),
            SyntaxKind::AliasClause => return Some(SemanticTokenKind::Class),
            _ if ast::ModuleItem::is_variant(parent_kind) => return Some(SemanticTokenKind::Class),
            SyntaxKind::StructArgSingle => return Some(SemanticTokenKind::Field),
            SyntaxKind::FunctionDeclaration => return Some(SemanticTokenKind::Function),
            SyntaxKind::GenericParamType => return Some(SemanticTokenKind::TypeParameter),
            SyntaxKind::PathSegmentSimple | SyntaxKind::PathSegmentWithGenericArgs => {
                match grandparent_kind(syntax_db, &parent_node) {
                    Some(SyntaxKind::GenericParamImplAnonymous) => {
                        return Some(SemanticTokenKind::GenericParamImpl);
                    }
                    Some(
                        SyntaxKind::GenericArgNamed
                        | SyntaxKind::GenericArgUnnamed
                        | SyntaxKind::GenericArgValueExpr,
                    ) => {
                        return Some(SemanticTokenKind::TypeParameter);
                    }
                    _ => {}
                }
            }

            _ => {}
        }

        // Identifier.
        while let Some(parent) = node.parent() {
            node = parent;

            match node.kind(syntax_db) {
                SyntaxKind::ExprInlineMacro => return Some(SemanticTokenKind::InlineMacro),
                SyntaxKind::ExprPath => {
                    expr_path_ptr =
                        Some(ast::ExprPath::from_syntax_node(syntax_db, node.clone()).stable_ptr());
                }
                SyntaxKind::Member => return Some(SemanticTokenKind::Variable),
                SyntaxKind::PatternIdentifier => return Some(SemanticTokenKind::Variable),
                SyntaxKind::Variant => return Some(SemanticTokenKind::EnumMember),
                SyntaxKind::Attribute => return Some(SemanticTokenKind::Annotation),
                _ => {}
            };

            for lookup_item_id in db.collect_lookup_items_leaf(&node)? {
                // Resolved items.
                if let Some(item) =
                    db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())
                {
                    return Some(match item {
                        ResolvedGenericItem::GenericConstant(_) => SemanticTokenKind::EnumMember,
                        ResolvedGenericItem::Module(_) => SemanticTokenKind::Namespace,
                        ResolvedGenericItem::GenericFunction(_)
                        | ResolvedGenericItem::TraitFunction(_) => SemanticTokenKind::Function,
                        ResolvedGenericItem::GenericType(_)
                        | ResolvedGenericItem::GenericTypeAlias(_) => SemanticTokenKind::Type,
                        ResolvedGenericItem::Variant(_) => SemanticTokenKind::EnumMember,
                        ResolvedGenericItem::Trait(_) => SemanticTokenKind::Interface,
                        ResolvedGenericItem::Impl(_) | ResolvedGenericItem::GenericImplAlias(_) => {
                            SemanticTokenKind::Class
                        }
                        ResolvedGenericItem::Variable(_) => SemanticTokenKind::Variable,
                    });
                }
                if let Some(item) =
                    db.lookup_resolved_concrete_item_by_ptr(lookup_item_id, identifier.stable_ptr())
                {
                    return Some(match item {
                        ResolvedConcreteItem::Constant(_) => SemanticTokenKind::EnumMember,
                        ResolvedConcreteItem::Module(_) => SemanticTokenKind::Namespace,
                        ResolvedConcreteItem::Function(_)
                        | ResolvedConcreteItem::TraitFunction(_) => SemanticTokenKind::Function,
                        ResolvedConcreteItem::Type(_) => SemanticTokenKind::Type,
                        ResolvedConcreteItem::Variant(_) => SemanticTokenKind::EnumMember,
                        ResolvedConcreteItem::Trait(_) => SemanticTokenKind::Interface,
                        ResolvedConcreteItem::Impl(_) => SemanticTokenKind::Class,
                    });
                }

                // Exprs and patterns..
                let Some(function_id) = lookup_item_id.function_with_body() else {
                    continue;
                };
                if let Some(expr_path_ptr) = expr_path_ptr {
                    if db.lookup_pattern_by_ptr(function_id, expr_path_ptr.into()).is_ok() {
                        return Some(SemanticTokenKind::Variable);
                    }
                }
            }
        }

        None
    }
    pub fn as_u32(&self) -> u32 {
        match self {
            SemanticTokenKind::Namespace => 0,
            SemanticTokenKind::Class => 1,
            SemanticTokenKind::Enum => 2,
            SemanticTokenKind::Interface => 3,
            SemanticTokenKind::Struct => 4,
            SemanticTokenKind::TypeParameter => 5,
            SemanticTokenKind::Type => 6,
            SemanticTokenKind::Parameter => 7,
            SemanticTokenKind::Variable => 8,
            SemanticTokenKind::Property => 9,
            SemanticTokenKind::EnumMember => 10,
            SemanticTokenKind::Function => 11,
            SemanticTokenKind::Comment => 12,
            SemanticTokenKind::Keyword => 13,
            SemanticTokenKind::Operator => 14,
            SemanticTokenKind::Number => 15,
            SemanticTokenKind::String => 16,
            SemanticTokenKind::Field => 17,
            SemanticTokenKind::Annotation => 18,
            SemanticTokenKind::InlineMacro => 19,
            SemanticTokenKind::GenericParamImpl => 20,
        }
    }
    pub fn legend() -> Vec<SemanticTokenType> {
        vec![
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::CLASS,
            SemanticTokenType::ENUM,
            SemanticTokenType::INTERFACE,
            SemanticTokenType::STRUCT,
            SemanticTokenType::TYPE_PARAMETER,
            SemanticTokenType::TYPE,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::ENUM_MEMBER,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::COMMENT,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::OPERATOR,
            SemanticTokenType::NUMBER,
            SemanticTokenType::STRING,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::DECORATOR,
            SemanticTokenType::MACRO,
            SemanticTokenType::INTERFACE,
        ]
    }
}
