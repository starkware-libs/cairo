use smol_str::SmolStr;

#[derive(Clone, PartialEq, Debug)]
pub struct Terminal {
    pub leading_trivia: Vec<Trivium>,
    pub token: Token,
    pub trailing_trivia: Vec<Trivium>,
    pub width: u32,
}
#[allow(dead_code)]
impl Terminal {
    pub fn kind(&self) -> TokenKind {
        self.token.kind
    }
    pub fn text(&self) -> SmolStr {
        self.token.text.clone()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Trivium {
    pub kind: TriviumKind,
    pub text: SmolStr,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TriviumKind {
    SingleLineComment,
    Whitespace,
    Newline,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: SmolStr,
}

#[derive(Clone, Copy, PartialEq, Debug)]
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

    EndOfFile,
    BadCharacter,
}
