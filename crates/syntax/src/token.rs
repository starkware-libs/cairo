

use smol_str::SmolStr;
#[derive(Clone, PartialEq, Debug, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub text: SmolStr,
}
impl Token {
    pub fn missing() -> Token {
        Token { kind: TokenKind::Missing, text: "".into() }
    }
    pub fn width(&self) -> u32 {
        self.text.len() as u32
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash)]
pub enum TokenKind {
    Identifier,

    // Literals.
    LiteralNumber,

    // Keywords.
    False,
    Function,
    Let,
    Module,
    Return,
    Struct,
    True,

    // Punctuation.
    And,
    AndAnd,
    Colon,
    ColonColon,
    Comma,
    Div,
    Dot,
    Eq,
    EqEq,
    GT,
    LT,
    Minus,
    Mul,
    Not,
    Neq,
    Or,
    Plus,
    Semi,
    Underscore,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,

    // Trivia.
    SingleLineComment,
    Whitespace,
    Newline,

    // Meta.
    Missing,
    EndOfFile,
    BadCharacter,
}
