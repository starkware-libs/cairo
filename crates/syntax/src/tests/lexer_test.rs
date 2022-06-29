use crate::{
    lexer::Lexer,
    text::CodeSource,
    token::{Terminal, Token, TokenKind, Trivium, TriviumKind},
};

fn token_kind_to_text(kind: TokenKind) -> Vec<&'static str> {
    match kind {
        TokenKind::Identifier => vec!["abc", "_az12f", "A90g5__"],
        TokenKind::Operator => vec![
            "*", "/", "+", "-", "<", ">", "!", "!=", "==", "&", "&&", "||",
        ],
        TokenKind::LiteralFalse => vec!["false"],
        TokenKind::LiteralTrue => vec!["true"],
        TokenKind::LiteralNumber => {
            vec!["0", "9", "00", "1234567890123456789012345678901234567890"]
        }
        TokenKind::KeywordFunction => vec!["fn"],
        TokenKind::KeywordModule => vec!["mod"],
        TokenKind::KeywordStruct => vec!["struct"],
        TokenKind::KeywordLet => vec!["let"],
        TokenKind::KeywordReturn => vec!["return"],
        TokenKind::CharColon => vec![":"],
        TokenKind::CharColonColon => vec!["::"],
        TokenKind::CharComma => vec![","],
        TokenKind::CharEquals => vec!["="],
        TokenKind::CharPeriod => vec!["."],
        TokenKind::CharSemiColon => vec![";"],
        TokenKind::CharUnderscore => vec!["_"],
        TokenKind::CharOpenBrace => vec!["{"],
        TokenKind::CharCloseBrace => vec!["}"],
        TokenKind::CharOpenBracket => vec!["["],
        TokenKind::CharCloseBracket => vec!["]"],
        TokenKind::CharOpenParenthesis => vec!["("],
        TokenKind::CharCloseParenthesis => vec![")"],
        TokenKind::EndOfFile => vec![],
        TokenKind::BadCharacter => vec![],
    }
}

fn token_kinds() -> Vec<TokenKind> {
    vec![
        TokenKind::Identifier,
        TokenKind::Operator,
        TokenKind::LiteralFalse,
        TokenKind::LiteralTrue,
        TokenKind::LiteralNumber,
        TokenKind::KeywordFunction,
        TokenKind::KeywordModule,
        TokenKind::KeywordStruct,
        TokenKind::KeywordLet,
        TokenKind::KeywordReturn,
        TokenKind::CharComma,
        TokenKind::CharColon,
        TokenKind::CharColonColon,
        TokenKind::CharEquals,
        TokenKind::CharPeriod,
        TokenKind::CharSemiColon,
        TokenKind::CharUnderscore,
        TokenKind::CharOpenBrace,
        TokenKind::CharCloseBrace,
        TokenKind::CharOpenBracket,
        TokenKind::CharCloseBracket,
        TokenKind::CharOpenParenthesis,
        TokenKind::CharCloseParenthesis,
        TokenKind::EndOfFile,
        TokenKind::BadCharacter,
    ]
}

fn token_separators() -> Vec<&'static str> {
    vec![" ", "\t", "\n", " // Comment\n"]
}

fn need_separator(
    kind0: TokenKind,
    text0: &'static str,
    kind1: TokenKind,
    text1: &'static str,
) -> bool {
    if is_identifier_like(kind0) {
        if is_identifier_like(kind1) {
            return true;
        }
        if matches!(kind1, TokenKind::LiteralNumber) {
            return true;
        }
    }
    if text0 == "&" && text1 == "&" {
        return true;
    }
    if text0 == "&" && text1 == "&&" {
        return true;
    }
    if text0 == "/" && text1 == "/" {
        return true;
    }
    if text0 == "!" && text1 == "=" {
        return true;
    }
    if text0 == "!" && text1 == "==" {
        return true;
    }
    if text0 == "=" && text1 == "=" {
        return true;
    }
    if text0 == "=" && text1 == "==" {
        return true;
    }
    if text0 == ":" && text1 == ":" {
        return true;
    }
    if text0 == ":" && text1 == "::" {
        return true;
    }
    if kind0 == TokenKind::LiteralNumber && kind0 == kind1 {
        return true;
    }
    false
}

fn is_identifier_like(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Identifier
            | TokenKind::LiteralFalse
            | TokenKind::LiteralTrue
            | TokenKind::KeywordFunction
            | TokenKind::KeywordModule
            | TokenKind::KeywordStruct
            | TokenKind::KeywordLet
            | TokenKind::KeywordReturn
            | TokenKind::CharUnderscore
    )
}

fn token_kind_and_text() -> Vec<(TokenKind, &'static str)> {
    let mut res: Vec<(TokenKind, &'static str)> = Vec::new();
    for kind in token_kinds() {
        for text in token_kind_to_text(kind) {
            res.push((kind, text));
        }
    }
    res
}

fn test_source() -> CodeSource {
    CodeSource {
        file_path: "test".into(),
    }
}

#[test]
fn test_lex_single_token() {
    for (kind, text) in token_kind_and_text() {
        let mut lexer = Lexer::from_text(test_source(), text);
        let token = lexer.next().unwrap();
        assert_eq!(
            token.kind(),
            kind,
            "Wrong token kind, with text: \"{}\".",
            text
        );
        assert_eq!(token.text(), text, "Wrong token text.");
        assert_eq!(
            lexer.next().unwrap().kind(),
            TokenKind::EndOfFile,
            "Wrong eof token, with text: \"{}\".",
            text
        );
        assert!(lexer.next().is_none(), "Expected end of lexer stream.");
    }
}

#[test]
fn test_lex_double_token() {
    for (kind0, text0) in token_kind_and_text() {
        for (kind1, text1) in token_kind_and_text() {
            let separators = if need_separator(kind0, text0, kind1, text1) {
                token_separators()
            } else {
                vec![""]
            };
            for separator in separators {
                let text = format!("{}{}{}", text0, separator, text1);
                let mut lexer = Lexer::from_text(test_source(), text.as_str());
                let token = lexer.next().unwrap();
                assert_eq!(
                    token.kind(),
                    kind0,
                    "Wrong token kind0, with text: \"{}\".",
                    text
                );
                assert_eq!(
                    token.text(),
                    text0,
                    "Wrong token text, with total text: \"{}\".",
                    text
                );
                let token = lexer.next().unwrap();
                assert_eq!(
                    token.kind(),
                    kind1,
                    "Wrong token kind0, with text: \"{}\".",
                    text
                );
                assert_eq!(
                    token.text(),
                    text1,
                    "Wrong token text, with total text: \"{}\".",
                    text
                );
                assert_eq!(
                    lexer.next().unwrap().kind(),
                    TokenKind::EndOfFile,
                    "Wrong eof token, with text: \"{}\".",
                    text
                );
                assert!(lexer.next().is_none(), "Expected end of lexer stream.");
            }
        }
    }
}

#[test]
fn test_cases() {
    let res: Vec<Terminal> =
        Lexer::from_text(test_source(), "let x: &T = @ 6; //  5+ 3;").collect();
    assert_eq!(
        res,
        vec![
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::KeywordLet,
                    text: "let".into()
                },
                trailing_trivia: vec![Trivium {
                    kind: TriviumKind::Whitespace,
                    text: " ".into()
                }],
                width: 3
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::Identifier,
                    text: "x".into()
                },
                trailing_trivia: vec![],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::CharColon,
                    text: ":".into()
                },
                trailing_trivia: vec![Trivium {
                    kind: TriviumKind::Whitespace,
                    text: " ".into()
                }],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::Operator,
                    text: "&".into()
                },
                trailing_trivia: vec![],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::Identifier,
                    text: "T".into()
                },
                trailing_trivia: vec![Trivium {
                    kind: TriviumKind::Whitespace,
                    text: " ".into()
                }],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::CharEquals,
                    text: "=".into()
                },
                trailing_trivia: vec![Trivium {
                    kind: TriviumKind::Whitespace,
                    text: " ".into()
                }],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::BadCharacter,
                    text: "@".into()
                },
                trailing_trivia: vec![Trivium {
                    kind: TriviumKind::Whitespace,
                    text: " ".into()
                }],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::LiteralNumber,
                    text: "6".into()
                },
                trailing_trivia: vec![],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::CharSemiColon,
                    text: ";".into()
                },
                trailing_trivia: vec![
                    Trivium {
                        kind: TriviumKind::Whitespace,
                        text: " ".into()
                    },
                    Trivium {
                        kind: TriviumKind::SingleLineComment,
                        text: "//  5+ 3;".into()
                    }
                ],
                width: 1
            },
            Terminal {
                leading_trivia: vec![],
                token: Token {
                    kind: TokenKind::EndOfFile,
                    text: "".into()
                },
                trailing_trivia: vec![],
                width: 0
            }
        ]
    );
}
