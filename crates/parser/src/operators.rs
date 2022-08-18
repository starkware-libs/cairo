use syntax::token::TokenKind;

pub fn get_unary_operator_precedence(kind: TokenKind) -> Option<usize> {
    if [TokenKind::Not, TokenKind::Minus].contains(&kind) {
        get_binary_operator_precedence(kind)
    } else {
        None
    }
}
pub fn get_binary_operator_precedence(kind: TokenKind) -> Option<usize> {
    match kind {
        TokenKind::Dot => Some(0),
        // TODO(yuval): support unary-only/non-binary operators. "not" can't be binary.
        TokenKind::Not => Some(1),
        TokenKind::Mul | TokenKind::Div => Some(2),
        TokenKind::Plus | TokenKind::Minus => Some(3),
        TokenKind::EqEq => Some(4),
        // TODO(yuval): add more operators.
        _ => None,
    }
}
