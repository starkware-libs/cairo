use cairo_lang_syntax::node::kind::SyntaxKind;

pub fn get_unary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    match kind {
        SyntaxKind::TerminalAt
        | SyntaxKind::TerminalNot
        | SyntaxKind::TerminalBitNot
        | SyntaxKind::TerminalMul
        | SyntaxKind::TerminalMinus => Some(2),
        _ => None,
    }
}
pub fn get_post_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    match kind {
        SyntaxKind::TerminalDot => Some(0),
        SyntaxKind::TerminalQuestionMark
        // [] Operator.
        | SyntaxKind::TerminalLBrack => Some(1),
        // TODO: 2 is a placeholder for << and >>.
        SyntaxKind::TerminalAnd => Some(3),
        SyntaxKind::TerminalXor => Some(4),
        SyntaxKind::TerminalOr => Some(5),
        SyntaxKind::TerminalMul | SyntaxKind::TerminalDiv | SyntaxKind::TerminalMod => Some(6),
        SyntaxKind::TerminalPlus | SyntaxKind::TerminalMinus => Some(7),
        SyntaxKind::TerminalEqEq
        | SyntaxKind::TerminalNeq
        | SyntaxKind::TerminalLT
        | SyntaxKind::TerminalGT
        | SyntaxKind::TerminalLE
        | SyntaxKind::TerminalGE => Some(8),
        SyntaxKind::TerminalAndAnd => Some(9),
        SyntaxKind::TerminalOrOr => Some(10),
        SyntaxKind::TerminalEq
        | SyntaxKind::TerminalPlusEq
        | SyntaxKind::TerminalMinusEq
        | SyntaxKind::TerminalMulEq
        | SyntaxKind::TerminalDivEq
        | SyntaxKind::TerminalModEq => Some(11),
        // TODO: Add <<= >>= &= ^= |= with 11.
        _ => None,
    }
}
