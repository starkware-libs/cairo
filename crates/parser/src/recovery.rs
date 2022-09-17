/// Macro that produces an inline function that gets a token kind and returns true iff it is on of
/// the supplied groups.
macro_rules! is_of_kind {
    ($($element:ident),*) => {
        |kind: TokenKind| {
            match kind{
                $($crate::recovery::$element!() => true,)*
                TokenKind::EndOfFile => true,
                _ => false
            }
        }
    };
}
pub(crate) use is_of_kind;

macro_rules! lbrace {
    () => {
        TokenKind::LBrace
    };
}
pub(crate) use lbrace;

macro_rules! rbrace {
    () => {
        TokenKind::RBrace
    };
}
pub(crate) use rbrace;

macro_rules! rparen {
    () => {
        TokenKind::RParen
    };
}
pub(crate) use rparen;

macro_rules! rangle {
    () => {
        TokenKind::GT
    };
}
pub(crate) use rangle;

macro_rules! top_level {
    () => {
        TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Use
    };
}
pub(crate) use top_level;

macro_rules! block {
    () => {
        TokenKind::Let | TokenKind::Match | TokenKind::Return
    };
}
pub(crate) use block;
