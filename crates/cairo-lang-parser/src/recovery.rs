/// Macro that produces an inline function that gets a token kind and returns true iff it is in one
/// of the supplied groups.
macro_rules! is_of_kind {
    ($($element:ident),*) => {
        |kind: LexemeKind| {
            match kind{
                $($crate::recovery::$element!() => true,)*
                LexemeKind::EndOfFile => true,
                _ => false
            }
        }
    };
}
pub(crate) use is_of_kind;

macro_rules! match_arrow {
    () => {
        LexemeKind::MatchArrow
    };
}
pub(crate) use match_arrow;

macro_rules! lbrace {
    () => {
        LexemeKind::LBrace
    };
}
pub(crate) use lbrace;

macro_rules! rbrace {
    () => {
        LexemeKind::RBrace
    };
}
pub(crate) use rbrace;

macro_rules! rparen {
    () => {
        LexemeKind::RParen
    };
}
pub(crate) use rparen;

macro_rules! rbrack {
    () => {
        LexemeKind::RBrack
    };
}
pub(crate) use rbrack;

macro_rules! rangle {
    () => {
        LexemeKind::GT | LexemeKind::GE
    };
}
pub(crate) use rangle;

macro_rules! or {
    () => {
        LexemeKind::Or
    };
}
pub(crate) use or;

macro_rules! comma {
    () => {
        LexemeKind::Comma
    };
}
pub(crate) use comma;

macro_rules! semicolon {
    () => {
        LexemeKind::Semicolon
    };
}
pub(crate) use semicolon;

macro_rules! eq {
    () => {
        LexemeKind::Eq
    };
}
pub(crate) use eq;

macro_rules! module_item_kw {
    () => {
        LexemeKind::Const
            | LexemeKind::Enum
            | LexemeKind::Extern
            | LexemeKind::Function
            | LexemeKind::Impl
            | LexemeKind::Macro
            | LexemeKind::Module
            | LexemeKind::Struct
            | LexemeKind::Trait
            | LexemeKind::Type
            | LexemeKind::Use
    };
}
pub(crate) use module_item_kw;

macro_rules! block {
    () => {
        LexemeKind::Let | LexemeKind::Match | LexemeKind::Return
    };
}
pub(crate) use block;
