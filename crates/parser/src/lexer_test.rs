use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::db::SyntaxGroup;
use syntax::node::Token;
use syntax::token::{self, TokenKind};
use utils::extract_matches;

use super::Lexer;
use crate::lexer::LexerTerminal;
use crate::test_utils::ParserDatabaseForTesting;

// TODO(spapini): Use snapshot/regression tests.

fn token_kind_to_text(kind: TokenKind) -> Vec<&'static str> {
    match kind {
        TokenKind::Identifier => vec!["abc", "_az12f", "A90g5__"],
        TokenKind::LiteralNumber => {
            vec!["0", "9", "00", "1234567890123456789012345678901234567890"]
        }
        TokenKind::False => vec!["false"],
        TokenKind::Extern => vec!["extern"],
        TokenKind::Type => vec!["type"],
        TokenKind::Function => vec!["func"],
        TokenKind::Let => vec!["let"],
        TokenKind::Module => vec!["mod"],
        TokenKind::Struct => vec!["struct"],
        TokenKind::True => vec!["true"],
        TokenKind::Return => vec!["return"],
        TokenKind::Match => vec!["match"],
        TokenKind::Use => vec!["use"],
        TokenKind::And => vec!["&"],
        TokenKind::AndAnd => vec!["&&"],
        TokenKind::Colon => vec![":"],
        TokenKind::ColonColon => vec!["::"],
        TokenKind::Comma => vec![","],
        TokenKind::Div => vec!["/"],
        TokenKind::Dot => vec!["."],
        TokenKind::DotDot => vec![".."],
        TokenKind::Eq => vec!["="],
        TokenKind::EqEq => vec!["=="],
        TokenKind::GE => vec![">="],
        TokenKind::GT => vec![">"],
        TokenKind::LE => vec!["<="],
        TokenKind::LT => vec!["<"],
        TokenKind::Minus => vec!["-"],
        TokenKind::Mul => vec!["*"],
        TokenKind::Neq => vec!["!="],
        TokenKind::Not => vec!["!"],
        TokenKind::OrOr => vec!["||"],
        TokenKind::Plus => vec!["+"],
        TokenKind::Semicolon => vec![";"],
        TokenKind::Underscore => vec!["_"],
        TokenKind::LBrace => vec!["{"],
        TokenKind::RBrace => vec!["}"],
        TokenKind::LBrack => vec!["["],
        TokenKind::RBrack => vec!["]"],
        TokenKind::LParen => vec!["("],
        TokenKind::RParen => vec![")"],
        TokenKind::Arrow => vec!["->"],
        TokenKind::MatchArrow => vec!["=>"],
        TokenKind::EndOfFile
        | TokenKind::BadCharacters
        | TokenKind::Missing
        | TokenKind::SingleLineComment
        | TokenKind::Whitespace
        | TokenKind::Newline => vec![],
    }
}

fn token_kinds() -> Vec<TokenKind> {
    vec![
        TokenKind::Identifier,
        TokenKind::LiteralNumber,
        TokenKind::False,
        TokenKind::True,
        TokenKind::Extern,
        TokenKind::Type,
        TokenKind::Function,
        TokenKind::Module,
        TokenKind::Struct,
        TokenKind::Let,
        TokenKind::Return,
        TokenKind::Match,
        TokenKind::Use,
        TokenKind::And,
        TokenKind::AndAnd,
        TokenKind::OrOr,
        TokenKind::EqEq,
        TokenKind::Neq,
        TokenKind::GE,
        TokenKind::GT,
        TokenKind::LE,
        TokenKind::LT,
        TokenKind::Not,
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Mul,
        TokenKind::Div,
        TokenKind::Colon,
        TokenKind::ColonColon,
        TokenKind::Comma,
        TokenKind::Dot,
        TokenKind::DotDot,
        TokenKind::Eq,
        TokenKind::Semicolon,
        TokenKind::Underscore,
        TokenKind::LBrace,
        TokenKind::RBrace,
        TokenKind::LBrack,
        TokenKind::RBrack,
        TokenKind::LParen,
        TokenKind::RParen,
        TokenKind::Arrow,
        TokenKind::MatchArrow,
        TokenKind::EndOfFile,
        TokenKind::BadCharacters,
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
    if is_identifier_like(kind0)
        && (is_identifier_like(kind1) || matches!(kind1, TokenKind::LiteralNumber))
    {
        return true;
    }
    if kind0 == TokenKind::LiteralNumber && kind0 == kind1 {
        return true;
    }
    if (text0 == "&" && text1.starts_with('&'))
        || (text0 == "/" && text1 == "/")
        || ((text0 == "=" || text0 == "!") && text1.starts_with('='))
        || ((text0 == "=") && text1.starts_with('>'))
        || ((text0 == "<" || text0 == ">") && text1.starts_with('='))
        || (text0 == ":" && text1.starts_with(':'))
        || (text0 == "." && text1.starts_with('.'))
        || (text0 == "-" && text1.starts_with('>'))
        || (kind0 == TokenKind::LiteralNumber && kind0 == kind1)
    {
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
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::Use
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

fn trivia_kinds() -> Vec<TokenKind> {
    vec![TokenKind::Whitespace, TokenKind::Newline, TokenKind::SingleLineComment]
}
fn trivia_kind_to_text(kind: TokenKind) -> Vec<&'static str> {
    match kind {
        TokenKind::SingleLineComment => vec!["// abc def\n", "///\n", "//=\n"],
        TokenKind::Whitespace => vec![" ", "\t", "\r"],
        TokenKind::Newline => vec!["\n"],
        _ => vec![],
    }
}
fn trivia_kind_and_text() -> Vec<(TokenKind, &'static str)> {
    let mut res: Vec<(TokenKind, &'static str)> = Vec::new();
    for kind in trivia_kinds() {
        for text in trivia_kind_to_text(kind) {
            res.push((kind, text));
        }
    }
    res
}

fn test_source() -> FileId {
    FileId::from_intern_id(InternId::from(100u32))
}

fn extract_token_from_terminal(
    db: &ParserDatabaseForTesting,
    terminal: &LexerTerminal,
) -> token::Token {
    extract_matches!(
        db.lookup_intern_green(terminal.token.0),
        syntax::node::green::GreenNode::Token
    )
}

#[test]
fn test_lex_single_token() {
    let db_val = ParserDatabaseForTesting::default();
    let db = &db_val;
    for (kind, text) in token_kind_and_text() {
        let mut lexer = Lexer::from_text(db, test_source(), text);
        let terminal = lexer.next().unwrap();
        // TODO(spapini): Remove calling new_root on non root elements.
        assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{}\".", text);
        assert_eq!(extract_token_from_terminal(db, &terminal).text, text, "Wrong token text.");

        assert_eq!(
            lexer.next().unwrap().kind,
            TokenKind::EndOfFile,
            "Wrong eof token, with text: \"{}\".",
            text
        );
        assert!(lexer.next().is_none(), "Expected end of lexer stream.");
    }
}

#[test]
fn test_lex_double_token() {
    let db_val = ParserDatabaseForTesting::default();
    let db = &db_val;
    for (kind0, text0) in token_kind_and_text() {
        for (kind1, text1) in token_kind_and_text() {
            let mut separators = token_separators();
            if !need_separator(kind0, text0, kind1, text1) {
                separators.push("");
            }
            for separator in separators {
                let text = format!("{}{}{}", text0, separator, text1);
                let mut lexer = Lexer::from_text(db, test_source(), text.as_str());

                let terminal = lexer.next().unwrap();
                let token_text = extract_token_from_terminal(db, &terminal).text;
                assert_eq!(
                    terminal.kind, kind0,
                    "Wrong token kind0, with text: \"{}\".",
                    token_text
                );
                assert_eq!(
                    token_text, text0,
                    "Wrong token text0, with total text: \"{}\".",
                    token_text
                );

                let terminal = lexer.next().unwrap();
                let token_text = extract_token_from_terminal(db, &terminal).text;
                assert_eq!(
                    terminal.kind, kind1,
                    "Wrong token kind1, with text: \"{}\".",
                    token_text
                );
                assert_eq!(
                    token_text, text1,
                    "Wrong token text1, with total text: \"{}\".",
                    token_text
                );

                assert_eq!(
                    lexer.next().unwrap().kind,
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
fn test_lex_token_with_trivia() {
    let db_val = ParserDatabaseForTesting::default();
    let db = &db_val;
    for (kind, expected_token_text) in token_kind_and_text() {
        for (_leading_kind, leading_trivia) in trivia_kind_and_text() {
            for (_trailing_kind, trailing_trivia) in trivia_kind_and_text() {
                let text = format!("{}{} {}", leading_trivia, expected_token_text, trailing_trivia);
                let mut lexer = Lexer::from_text(db, test_source(), text.as_str());
                let terminal = lexer.next().unwrap();
                let token_text = extract_token_from_terminal(db, &terminal).text;
                assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{}\".", text);
                assert_eq!(token_text, expected_token_text, "Wrong token text.");
                // TODO: verify trivia kinds and texts

                assert_eq!(
                    lexer.next().unwrap().kind,
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
    let db_val = ParserDatabaseForTesting::default();
    let db = &db_val;
    let res: Vec<LexerTerminal> =
        Lexer::from_text(db, test_source(), "let x: &T = @ 6; //  5+ 3;").collect();
    assert_eq!(
        res,
        vec![
            LexerTerminal {
                token: Token::new_green(db, TokenKind::Let, "let".into()),
                kind: TokenKind::Let,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    Token::new_green(db, TokenKind::Whitespace, " ".into()).into()
                ]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::Identifier, "x".into()),
                kind: TokenKind::Identifier,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::Colon, ":".into()),
                kind: TokenKind::Colon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    Token::new_green(db, TokenKind::Whitespace, " ".into()).into()
                ]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::And, "&".into()),
                kind: TokenKind::And,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::Identifier, "T".into()),
                kind: TokenKind::Identifier,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    Token::new_green(db, TokenKind::Whitespace, " ".into()).into()
                ]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::Eq, "=".into()),
                kind: TokenKind::Eq,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    Token::new_green(db, TokenKind::Whitespace, " ".into()).into()
                ]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::BadCharacters, "@".into()),
                kind: TokenKind::BadCharacters,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    Token::new_green(db, TokenKind::Whitespace, " ".into()).into()
                ]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::LiteralNumber, "6".into()),
                kind: TokenKind::LiteralNumber,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::Semicolon, ";".into()),
                kind: TokenKind::Semicolon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    Token::new_green(db, TokenKind::Whitespace, " ".into()).into(),
                    Token::new_green(db, TokenKind::SingleLineComment, "//  5+ 3;".into()).into()
                ]
            },
            LexerTerminal {
                token: Token::new_green(db, TokenKind::EndOfFile, "".into()),
                kind: TokenKind::EndOfFile,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            }
        ]
    );
}

#[test]
fn test_bad_character() {
    let db_val = ParserDatabaseForTesting::default();
    let db = &db_val;

    let text = "@";
    let mut lexer = Lexer::from_text(db, test_source(), text);
    let terminal = lexer.next().unwrap();
    let token_text = extract_token_from_terminal(db, &terminal).text;
    assert_eq!(
        terminal.kind,
        TokenKind::BadCharacters,
        "Wrong token kind, with text: \"{}\".",
        text
    );
    assert_eq!(token_text, text, "Wrong token text.");

    assert_eq!(
        lexer.next().unwrap().kind,
        TokenKind::EndOfFile,
        "Wrong eof token, with text: \"{}\".",
        text
    );
    assert!(lexer.next().is_none(), "Expected end of lexer stream.");
}
