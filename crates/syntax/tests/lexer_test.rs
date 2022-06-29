use syntax::{
    lexer::Lexer,
    text::CodeSource,
    token::{Terminal, Token, TokenKind, Trivium, TriviumKind},
};

// TODO(spapini): Use snapshot/regression tests.

fn token_kind_to_text(kind: TokenKind) -> Vec<&'static str> {
    match kind {
        TokenKind::Identifier => vec!["abc", "_az12f", "A90g5__"],
        TokenKind::LiteralNumber => {
            vec!["0", "9", "00", "1234567890123456789012345678901234567890"]
        }
        TokenKind::False => vec!["false"],
        TokenKind::Function => vec!["fn"],
        TokenKind::Let => vec!["let"],
        TokenKind::Module => vec!["mod"],
        TokenKind::Struct => vec!["struct"],
        TokenKind::True => vec!["true"],
        TokenKind::Return => vec!["return"],
        TokenKind::And => vec!["&"],
        TokenKind::AndAnd => vec!["&&"],
        TokenKind::Colon => vec![":"],
        TokenKind::ColonColon => vec!["::"],
        TokenKind::Comma => vec![","],
        TokenKind::Div => vec!["/"],
        TokenKind::Dot => vec!["."],
        TokenKind::Eq => vec!["="],
        TokenKind::EqEq => vec!["=="],
        TokenKind::GT => vec![">"],
        TokenKind::LT => vec!["<"],
        TokenKind::Minus => vec!["-"],
        TokenKind::Mul => vec!["*"],
        TokenKind::Neq => vec!["!="],
        TokenKind::Not => vec!["!"],
        TokenKind::Or => vec!["||"],
        TokenKind::Plus => vec!["+"],
        TokenKind::Semi => vec![";"],
        TokenKind::Underscore => vec!["_"],
        TokenKind::LBrace => vec!["{"],
        TokenKind::RBrace => vec!["}"],
        TokenKind::LBrack => vec!["["],
        TokenKind::RBrack => vec!["]"],
        TokenKind::LParen => vec!["("],
        TokenKind::RParen => vec![")"],
        TokenKind::EndOfFile => vec![],
        TokenKind::BadCharacter => vec![],
    }
}

fn token_kinds() -> Vec<TokenKind> {
    vec![
        TokenKind::Identifier,
        TokenKind::LiteralNumber,
        TokenKind::False,
        TokenKind::Function,
        TokenKind::Let,
        TokenKind::Module,
        TokenKind::Return,
        TokenKind::Struct,
        TokenKind::True,
        TokenKind::And,
        TokenKind::AndAnd,
        TokenKind::Colon,
        TokenKind::ColonColon,
        TokenKind::Comma,
        TokenKind::Div,
        TokenKind::Dot,
        TokenKind::Eq,
        TokenKind::EqEq,
        TokenKind::GT,
        TokenKind::LT,
        TokenKind::Minus,
        TokenKind::Mul,
        TokenKind::Not,
        TokenKind::Neq,
        TokenKind::Or,
        TokenKind::Plus,
        TokenKind::Semi,
        TokenKind::Underscore,
        TokenKind::LBrace,
        TokenKind::RBrace,
        TokenKind::LBrack,
        TokenKind::RBrack,
        TokenKind::LParen,
        TokenKind::RParen,
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
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Underscore
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
                    kind: TokenKind::Let,
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
                    kind: TokenKind::Colon,
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
                    kind: TokenKind::And,
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
                    kind: TokenKind::Eq,
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
                    kind: TokenKind::Semi,
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
