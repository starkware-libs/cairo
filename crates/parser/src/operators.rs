use syntax::node::kind::SyntaxKind;

pub fn get_unary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    if [SyntaxKind::TerminalNot, SyntaxKind::TerminalMinus].contains(&kind) {
        get_binary_operator_precedence(kind)
    } else {
        None
    }
}
pub fn get_binary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    match kind {
        // TODO(spapini): This should precede unary operators.
        SyntaxKind::TerminalDot => Some(0),

        // TODO(yuval): support unary-only/non-binary operators. "not" can't be binary.
        SyntaxKind::TerminalNot => Some(1),
        SyntaxKind::TerminalMul | SyntaxKind::TerminalDiv | SyntaxKind::TerminalMod => Some(2),
        SyntaxKind::TerminalPlus | SyntaxKind::TerminalMinus => Some(3),
        SyntaxKind::TerminalEqEq
        | SyntaxKind::TerminalNeq
        | SyntaxKind::TerminalLT
        | SyntaxKind::TerminalGT
        | SyntaxKind::TerminalLE
        | SyntaxKind::TerminalGE => Some(4),
        SyntaxKind::TerminalAnd => Some(5),
        SyntaxKind::TerminalOr => Some(6),
        SyntaxKind::TerminalEq => Some(7),

        // TODO(yuval): add more operators.
        _ => None,
    }
}
