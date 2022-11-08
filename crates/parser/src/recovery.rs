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

macro_rules! top_level {
    () => {
        SyntaxKind::TerminalExtern
            | SyntaxKind::TerminalType
            | SyntaxKind::TerminalEnum
            | SyntaxKind::TerminalTrait
            | SyntaxKind::TerminalImpl
            | SyntaxKind::TerminalFunction
            | SyntaxKind::TerminalModule
            | SyntaxKind::TerminalStruct
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
