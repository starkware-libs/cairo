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
    OrOr,
    EqEq,
    Neq,
    GE,
    GT,
    LE,
    LT,
    Not,
    Plus,
    Minus,
    Mul,
    Div,

    Colon,
    ColonColon,
    Comma,
    Dot,
    DotDot,
    Eq,
    Semi,
    Underscore,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,
    Arrow,

    // Trivia.
    SingleLineComment,
    Whitespace, // A sequence of whitespace characters.
    Newline,    // A single new line.

    // Meta.
    Missing,
    EndOfFile,
    BadCharacters,
}
