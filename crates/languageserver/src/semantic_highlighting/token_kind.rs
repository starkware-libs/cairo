use lsp::SemanticTokenType;
use syntax::token::TokenKind;

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
}
impl SemanticTokenKind {
    pub fn from_token_kind(kind: TokenKind) -> Option<Self> {
        Some(match kind {
            TokenKind::Identifier => SemanticTokenKind::Variable,
            TokenKind::LiteralNumber => SemanticTokenKind::Number,
            TokenKind::False
            | TokenKind::True
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::Use => SemanticTokenKind::Keyword,
            TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div => SemanticTokenKind::Operator,
            TokenKind::SingleLineComment => SemanticTokenKind::Comment,
            _ => return None,
        })
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
        ]
    }
}
