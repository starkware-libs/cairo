use cairo_lang_defs::ids::{FileIndex, ModuleFileId};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::resolve::ResolvedGenericItem;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::OptionHelper;
use lsp::SemanticTokenType;

use crate::{find_node_module, lookup_item_from_ast};

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
}
impl SemanticTokenKind {
    pub fn from_syntax_node(
        db: &dyn SemanticGroup,
        file_id: FileId,
        mut node: SyntaxNode,
    ) -> Option<Self> {
        let syntax_db = db.upcast();
        let kind = node.kind(syntax_db);
        match kind {
            _ if kind.is_keyword_token() => return Some(SemanticTokenKind::Keyword),
            SyntaxKind::TokenIdentifier => {}
            SyntaxKind::TokenNumber => return Some(SemanticTokenKind::Number),
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
            SyntaxKind::TokenShortString => return Some(SemanticTokenKind::String),
            _ => return None,
        };
        node = node.parent().unwrap();
        let identifier = ast::TerminalIdentifier::from_syntax_node(syntax_db, node.clone());

        // Identifier.
        while let Some(parent) = node.parent() {
            node = parent;
            let kind = node.kind(syntax_db);

            let module_id = find_node_module(db, file_id, node.clone()).on_none(|| {
                eprintln!("Hover failed. Failed to find module.");
            })?;
            let file_index = FileIndex(0);
            let module_file_id = ModuleFileId(module_id, file_index);

            let lookup_items = lookup_item_from_ast(db, module_file_id, node.clone());
            for lookup_item_id in lookup_items {
                if let Some(item) =
                    db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())
                {
                    return Some(match item {
                        ResolvedGenericItem::Constant(_) => SemanticTokenKind::EnumMember,
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
                        ResolvedGenericItem::Variable(_, _) => SemanticTokenKind::Variable,
                    });
                }
            }

            if ast::Item::is_variant(kind) | matches!(kind, SyntaxKind::AliasClause) {
                return Some(SemanticTokenKind::Class);
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
        ]
    }
}
