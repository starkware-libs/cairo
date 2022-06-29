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
    pub fn text(&self) -> String {
        self.token.text.clone()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Trivium {
    pub kind: TriviumKind,
    pub text: String,
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
    pub text: String,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenKind {
    Identifier,
    Operator,
    LiteralFalse,
    LiteralTrue,
    LiteralNumber,

    KeywordFunction,
    KeywordModule,
    KeywordStruct,
    KeywordLet,
    KeywordReturn,

    CharComma,
    CharColon,
    CharColonColon,
    CharEquals,
    CharPeriod,
    CharSemiColon,
    CharUnderscore,
    CharOpenBrace,
    CharCloseBrace,
    CharOpenBracket,
    CharCloseBracket,
    CharOpenParenthesis,
    CharCloseParenthesis,

    EndOfFile,
    BadCharacter,
}
