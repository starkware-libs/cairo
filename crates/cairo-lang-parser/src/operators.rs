use cairo_lang_syntax::node::kind::SyntaxKind;

pub fn get_unary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    if [SyntaxKind::TerminalNot, SyntaxKind::TerminalMinus].contains(&kind) {
        get_post_operator_precedence(kind)
    } else {
        None
    }
}
pub fn get_post_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    match kind {
        // TODO(spapini): This should precede unary operators.
        SyntaxKind::TerminalDot => Some(0),
        SyntaxKind::TerminalQuestionMark => Some(1),

        // TODO(yuval): support unary-only/non-binary operators. "not" can't be binary.
        SyntaxKind::TerminalNot => Some(2),
        SyntaxKind::TerminalMul | SyntaxKind::TerminalDiv | SyntaxKind::TerminalMod => Some(3),
        SyntaxKind::TerminalPlus | SyntaxKind::TerminalMinus => Some(4),
        SyntaxKind::TerminalEqEq
        | SyntaxKind::TerminalNeq
        | SyntaxKind::TerminalLT
        | SyntaxKind::TerminalGT
        | SyntaxKind::TerminalLE
        | SyntaxKind::TerminalGE => Some(5),
        SyntaxKind::TerminalAnd => Some(6),
        SyntaxKind::TerminalOr => Some(7),
        SyntaxKind::TerminalXor => Some(8),
        SyntaxKind::TerminalEq => Some(9),

        // TODO(yuval): add more operators.
        _ => None,
    }
}
