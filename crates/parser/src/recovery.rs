#[macro_export]
macro_rules! recovery {
    ($($element:ident),*) => {
        |kind: TokenKind| {
            match kind{
                $($crate::$element!() => true,)*
                TokenKind::EndOfFile => true,
                _ => false
            }
        }
    };
}

#[macro_export]
macro_rules! lbrace {
    () => {
        TokenKind::LBrace
    };
}

#[macro_export]
macro_rules! rbrace {
    () => {
        TokenKind::RBrace
    };
}

#[macro_export]
macro_rules! rparen {
    () => {
        TokenKind::RParen
    };
}

#[macro_export]
macro_rules! rangle {
    () => {
        TokenKind::GT
    };
}

// TODO(spapini): Add use.
#[macro_export]
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

#[macro_export]
macro_rules! block {
    () => {
        TokenKind::Let | TokenKind::Match | TokenKind::Return
    };
}
