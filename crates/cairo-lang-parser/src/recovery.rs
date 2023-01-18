/// Macro that produces an inline function that gets a token kind and returns true iff it is in one
/// of the supplied groups.
macro_rules! is_of_kind {
    ($($element:ident),*) => {
        |kind: SyntaxKind| {
            match kind{
                $($crate::recovery::$element!() => true,)*
                SyntaxKind::TerminalEndOfFile => true,
                _ => false
            }
        }
    };
}
pub(crate) use is_of_kind;

macro_rules! lbrace {
    () => {
        SyntaxKind::TerminalLBrace
    };
}
pub(crate) use lbrace;

macro_rules! rbrace {
    () => {
        SyntaxKind::TerminalRBrace
    };
}
pub(crate) use rbrace;

macro_rules! rparen {
    () => {
        SyntaxKind::TerminalRParen
    };
}
pub(crate) use rparen;

macro_rules! rangle {
    () => {
        SyntaxKind::TerminalGT
    };
}
pub(crate) use rangle;

macro_rules! comma {
    () => {
        SyntaxKind::TerminalComma
    };
}
pub(crate) use comma;

macro_rules! semicolon {
    () => {
        SyntaxKind::TerminalSemicolon
    };
}
pub(crate) use semicolon;

macro_rules! eq {
    () => {
        SyntaxKind::TerminalEq
    };
}
pub(crate) use eq;

macro_rules! top_level {
    () => {
        SyntaxKind::TerminalConst
            | SyntaxKind::TerminalEnum
            | SyntaxKind::TerminalExtern
            | SyntaxKind::TerminalFunction
            | SyntaxKind::TerminalImpl
            | SyntaxKind::TerminalModule
            | SyntaxKind::TerminalStruct
            | SyntaxKind::TerminalTrait
            | SyntaxKind::TerminalType
            | SyntaxKind::TerminalUse
    };
}
pub(crate) use top_level;

macro_rules! block {
    () => {
        SyntaxKind::TerminalLet | SyntaxKind::TerminalMatch | SyntaxKind::TerminalReturn
    };
}
pub(crate) use block;
