use cairo_lang_syntax::node::kind::SyntaxKind;

pub fn get_unary_operator_precedence(kind: SyntaxKind) -> Option<usize> {
    match kind {
        SyntaxKind::TerminalAt
        | SyntaxKind::TerminalBang
        | SyntaxKind::TerminalTilde
        | SyntaxKind::TerminalStar
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
        SyntaxKind::TerminalStar | SyntaxKind::TerminalSlash | SyntaxKind::TerminalPercent => Some(2),
        SyntaxKind::TerminalPlus | SyntaxKind::TerminalMinus => Some(3),
        SyntaxKind::TerminalAnd => Some(4),
        SyntaxKind::TerminalCaret => Some(5),
        SyntaxKind::TerminalOr => Some(6),
        SyntaxKind::TerminalEqEq
        | SyntaxKind::TerminalNeq
        | SyntaxKind::TerminalLT
        | SyntaxKind::TerminalGT
        | SyntaxKind::TerminalLE
        | SyntaxKind::TerminalGE => Some(7),
        SyntaxKind::TerminalAndAnd => Some(8),
        SyntaxKind::TerminalOrOr => Some(9),
        SyntaxKind::TerminalEq
        | SyntaxKind::TerminalPlusEq
        | SyntaxKind::TerminalMinusEq
        | SyntaxKind::TerminalStarEq
        | SyntaxKind::TerminalSlashEq
        | SyntaxKind::TerminalPercentEq => Some(10),
        _ => None,
    }
}
