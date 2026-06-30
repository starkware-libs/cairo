use cairo_lang_syntax::node::kind::LexemeKind;

pub fn get_unary_operator_precedence(kind: LexemeKind) -> Option<usize> {
    match kind {
        LexemeKind::At
        | LexemeKind::And
        | LexemeKind::AndAnd
        | LexemeKind::Not
        | LexemeKind::BitNot
        | LexemeKind::Mul
        | LexemeKind::Minus => Some(2),
        _ => None,
    }
}
pub fn get_post_operator_precedence(kind: LexemeKind) -> Option<usize> {
    match kind {
        LexemeKind::Dot => Some(0),
        LexemeKind::QuestionMark |
        // [] Operator.
        LexemeKind::LBrack => Some(1),
        LexemeKind::Mul | LexemeKind::Div | LexemeKind::Mod => Some(2),
        LexemeKind::Plus | LexemeKind::Minus => Some(3),
        LexemeKind::And => Some(4),
        LexemeKind::Xor => Some(5),
        LexemeKind::Or => Some(6),
        LexemeKind::EqEq
            | LexemeKind::Neq
            | LexemeKind::LT
            | LexemeKind::GT
            | LexemeKind::LE
            | LexemeKind::GE => Some(7),
        LexemeKind::AndAnd => Some(8),
        LexemeKind::OrOr => Some(9),
        LexemeKind::DotDot | LexemeKind::DotDotEq => Some(10),
        LexemeKind::Eq
            | LexemeKind::PlusEq
            | LexemeKind::MinusEq
            | LexemeKind::MulEq
            | LexemeKind::DivEq
            | LexemeKind::ModEq => Some(11),
        _ => None,
    }
}
