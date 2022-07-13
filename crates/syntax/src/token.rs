use smol_str::SmolStr;
#[derive(Clone, PartialEq, Debug, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub text: SmolStr,
}
impl Token {
    pub fn missing() -> Token {
        Token { kind: TokenKind::Missing, text: SmolStr::new("") }
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
    True,
    Function,
    Module,
    Struct,
    Let,
    Return,

    // Punctuation.
    And,
    AndAnd,
    Div,
    EqEq,
    GE,
    GT,
    LE,
    LT,
    Minus,
    Mul,
    Neq,
    Not,
    Plus,

    Colon,
    ColonColon,
    Comma,
    Dot,
    Eq,
    OrOr,
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
