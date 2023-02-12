use cairo_lang_syntax::node::kind::SyntaxKind;

pub fn get_unary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    if [SyntaxKind::TerminalAt, SyntaxKind::TerminalNot, SyntaxKind::TerminalMinus].contains(&kind)
    {
        get_binary_operator_precedence(kind)
    } else {
        None
    }
}
pub fn get_binary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    match kind {
        // TODO(spapini): This should precede unary operators.
        SyntaxKind::TerminalDot => Some(0),
        SyntaxKind::TerminalAt | SyntaxKind::TerminalNot => Some(1),
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
        SyntaxKind::TerminalXor => Some(7),
        SyntaxKind::TerminalEq => Some(8),

        // TODO(yuval): add more operators.
        _ => None,
    }
}
