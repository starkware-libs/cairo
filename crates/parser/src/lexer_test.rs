use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::ast::{TokenSingleLineComment, TokenWhitespace};
use syntax::node::kind::SyntaxKind;
use syntax::node::Token;
use test_log::test;

use super::Lexer;
use crate::lexer::LexerTerminal;
use crate::utils::SimpleParserDatabase;

// TODO(spapini): Use snapshot/regression tests.

fn terminal_kind_to_text(kind: SyntaxKind) -> Vec<&'static str> {
    match kind {
        SyntaxKind::TerminalIdentifier => vec!["abc", "_az12f", "A90g5__"],
        SyntaxKind::TerminalLiteralNumber => {
            vec![
                "0",
                "0xA2",
                "9",
                "00",
                "1234567890123456789012345678901234567890",
                "11_u128",
                "0xA2_u128",
            ]
        }
        SyntaxKind::TerminalFalse => vec!["false"],
        SyntaxKind::TerminalExtern => vec!["extern"],
        SyntaxKind::TerminalType => vec!["type"],
        SyntaxKind::TerminalFunction => vec!["func"],
        SyntaxKind::TerminalTrait => vec!["trait"],
        SyntaxKind::TerminalImpl => vec!["impl"],
        SyntaxKind::TerminalOf => vec!["of"],
        SyntaxKind::TerminalLet => vec!["let"],
        SyntaxKind::TerminalMut => vec!["mut"],
        SyntaxKind::TerminalRef => vec!["ref"],
        SyntaxKind::TerminalNoPanic => vec!["nopanic"],
        SyntaxKind::TerminalModule => vec!["mod"],
        SyntaxKind::TerminalStruct => vec!["struct"],
        SyntaxKind::TerminalEnum => vec!["enum"],
        SyntaxKind::TerminalTrue => vec!["true"],
        SyntaxKind::TerminalReturn => vec!["return"],
        SyntaxKind::TerminalMatch => vec!["match"],
        SyntaxKind::TerminalIf => vec!["if"],
        SyntaxKind::TerminalElse => vec!["else"],
        SyntaxKind::TerminalUse => vec!["use"],
        SyntaxKind::TerminalAnd => vec!["&"],
        SyntaxKind::TerminalAndAnd => vec!["&&"],
        SyntaxKind::TerminalColon => vec![":"],
        SyntaxKind::TerminalColonColon => vec!["::"],
        SyntaxKind::TerminalComma => vec![","],
        SyntaxKind::TerminalDiv => vec!["/"],
        SyntaxKind::TerminalMod => vec!["%"],
        SyntaxKind::TerminalDot => vec!["."],
        SyntaxKind::TerminalDotDot => vec![".."],
        SyntaxKind::TerminalEq => vec!["="],
        SyntaxKind::TerminalEqEq => vec!["=="],
        SyntaxKind::TerminalGE => vec![">="],
        SyntaxKind::TerminalGT => vec![">"],
        SyntaxKind::TerminalLE => vec!["<="],
        SyntaxKind::TerminalLT => vec!["<"],
        SyntaxKind::TerminalMinus => vec!["-"],
        SyntaxKind::TerminalMul => vec!["*"],
        SyntaxKind::TerminalNeq => vec!["!="],
        SyntaxKind::TerminalNot => vec!["!"],
        SyntaxKind::TerminalOr => vec!["|"],
        SyntaxKind::TerminalOrOr => vec!["||"],
        SyntaxKind::TerminalPlus => vec!["+"],
        SyntaxKind::TerminalSemicolon => vec![";"],
        SyntaxKind::TerminalQuestionMark => vec!["?"],
        SyntaxKind::TerminalUnderscore => vec!["_"],
        SyntaxKind::TerminalLBrace => vec!["{"],
        SyntaxKind::TerminalRBrace => vec!["}"],
        SyntaxKind::TerminalLBrack => vec!["["],
        SyntaxKind::TerminalRBrack => vec!["]"],
        SyntaxKind::TerminalLParen => vec!["("],
        SyntaxKind::TerminalRParen => vec![")"],
        SyntaxKind::TerminalArrow => vec!["->"],
        SyntaxKind::TerminalMatchArrow => vec!["=>"],
        SyntaxKind::TerminalEndOfFile => vec![],
        _ => {
            assert!(!kind.is_terminal());
            vec![]
        }
    }
}

fn terminal_kinds() -> Vec<SyntaxKind> {
    vec![
        SyntaxKind::TerminalIdentifier,
        SyntaxKind::TerminalLiteralNumber,
        SyntaxKind::TerminalFalse,
        SyntaxKind::TerminalTrue,
        SyntaxKind::TerminalExtern,
        SyntaxKind::TerminalType,
        SyntaxKind::TerminalFunction,
        SyntaxKind::TerminalTrait,
        SyntaxKind::TerminalImpl,
        SyntaxKind::TerminalOf,
        SyntaxKind::TerminalModule,
        SyntaxKind::TerminalStruct,
        SyntaxKind::TerminalEnum,
        SyntaxKind::TerminalLet,
        SyntaxKind::TerminalMut,
        SyntaxKind::TerminalRef,
        SyntaxKind::TerminalNoPanic,
        SyntaxKind::TerminalReturn,
        SyntaxKind::TerminalMatch,
        SyntaxKind::TerminalIf,
        SyntaxKind::TerminalElse,
        SyntaxKind::TerminalUse,
        SyntaxKind::TerminalAnd,
        SyntaxKind::TerminalAndAnd,
        SyntaxKind::TerminalOr,
        SyntaxKind::TerminalOrOr,
        SyntaxKind::TerminalEqEq,
        SyntaxKind::TerminalNeq,
        SyntaxKind::TerminalGE,
        SyntaxKind::TerminalGT,
        SyntaxKind::TerminalLE,
        SyntaxKind::TerminalLT,
        SyntaxKind::TerminalNot,
        SyntaxKind::TerminalPlus,
        SyntaxKind::TerminalMinus,
        SyntaxKind::TerminalMul,
        SyntaxKind::TerminalDiv,
        SyntaxKind::TerminalMod,
        SyntaxKind::TerminalColon,
        SyntaxKind::TerminalColonColon,
        SyntaxKind::TerminalComma,
        SyntaxKind::TerminalDot,
        SyntaxKind::TerminalDotDot,
        SyntaxKind::TerminalEq,
        SyntaxKind::TerminalSemicolon,
        SyntaxKind::TerminalQuestionMark,
        SyntaxKind::TerminalUnderscore,
        SyntaxKind::TerminalLBrace,
        SyntaxKind::TerminalRBrace,
        SyntaxKind::TerminalLBrack,
        SyntaxKind::TerminalRBrack,
        SyntaxKind::TerminalLParen,
        SyntaxKind::TerminalRParen,
        SyntaxKind::TerminalArrow,
        SyntaxKind::TerminalMatchArrow,
        SyntaxKind::TerminalEndOfFile,
    ]
}

fn token_separators() -> Vec<&'static str> {
    vec![" ", "\t", "\n", " // Comment\n"]
}

fn need_separator(
    kind0: SyntaxKind,
    text0: &'static str,
    kind1: SyntaxKind,
    text1: &'static str,
) -> bool {
    if is_identifier_like(kind0)
        && (is_identifier_like(kind1) || matches!(kind1, SyntaxKind::TerminalLiteralNumber))
    {
        return true;
    }
    if kind0 == SyntaxKind::TerminalLiteralNumber && (kind0 == kind1 || is_identifier_like(kind1)) {
        return true;
    }
    if (text0 == "&" && text1.starts_with('&'))
        || (text0 == "|" && text1.starts_with('|'))
        || (text0 == "/" && text1 == "/")
        || ((text0 == "=" || text0 == "!") && text1.starts_with('='))
        || ((text0 == "=") && text1.starts_with('>'))
        || ((text0 == "<" || text0 == ">") && text1.starts_with('='))
        || (text0 == ":" && text1.starts_with(':'))
        || (text0 == "." && text1.starts_with('.'))
        || (text0 == "-" && text1.starts_with('>'))
        || (kind0 == SyntaxKind::TerminalLiteralNumber && kind0 == kind1)
    {
        return true;
    }
    false
}

fn is_identifier_like(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::TerminalIdentifier | SyntaxKind::TerminalUnderscore)
        || kind.is_keyword_terminal()
}

fn terminal_kind_and_text() -> Vec<(SyntaxKind, &'static str)> {
    let mut res: Vec<(SyntaxKind, &'static str)> = Vec::new();
    for kind in terminal_kinds() {
        for text in terminal_kind_to_text(kind) {
            res.push((kind, text));
        }
    }
    res
}

fn trivia_kinds() -> Vec<SyntaxKind> {
    vec![SyntaxKind::TokenWhitespace, SyntaxKind::TokenNewline, SyntaxKind::TokenSingleLineComment]
}
fn trivia_kind_to_text(kind: SyntaxKind) -> Vec<&'static str> {
    match kind {
        SyntaxKind::TokenSingleLineComment => vec!["// abc def\n", "///\n", "//=\n"],
        SyntaxKind::TokenWhitespace => vec![" ", "\t", "\r"],
        SyntaxKind::TokenNewline => vec!["\n"],
        _ => vec![],
    }
}
fn trivia_texts() -> Vec<&'static str> {
    let mut res: Vec<&'static str> = Vec::new();
    for kind in trivia_kinds() {
        for text in trivia_kind_to_text(kind) {
            res.push(text);
        }
    }
    res
}

fn test_source() -> FileId {
    FileId::from_intern_id(InternId::from(100u32))
}

#[test]
fn test_lex_single_token() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    for (kind, text) in terminal_kind_and_text() {
        let mut lexer = Lexer::from_text(db, test_source(), text);
        let terminal = lexer.next().unwrap();
        // TODO(spapini): Remove calling new_root on non root elements.
        assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{}\".", text);
        assert_eq!(terminal.text, text, "Wrong token text.");

        assert_eq!(
            lexer.next().unwrap().kind,
            SyntaxKind::TerminalEndOfFile,
            "Wrong eof token, with text: \"{}\".",
            text
        );
        assert!(lexer.next().is_none(), "Expected end of lexer stream.");
    }
}

#[test]
fn test_lex_double_token() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    for (kind0, text0) in terminal_kind_and_text() {
        for (kind1, text1) in terminal_kind_and_text() {
            let mut separators = token_separators();
            if !need_separator(kind0, text0, kind1, text1) {
                separators.push("");
            }
            for separator in separators {
                let text = format!("{}{}{}", text0, separator, text1);
                let mut lexer = Lexer::from_text(db, test_source(), text.as_str());

                let terminal = lexer.next().unwrap();
                let token_text = terminal.text;
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
                let token_text = terminal.text;
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
                    SyntaxKind::TerminalEndOfFile,
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
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    for (kind, expected_token_text) in terminal_kind_and_text() {
        for leading_trivia in trivia_texts() {
            for trailing_trivia in trivia_texts() {
                let text = format!("{}{} {}", leading_trivia, expected_token_text, trailing_trivia);
                let mut lexer = Lexer::from_text(db, test_source(), text.as_str());
                let terminal = lexer.next().unwrap();
                let token_text = terminal.text;
                assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{}\".", text);
                assert_eq!(token_text, expected_token_text, "Wrong token text.");
                // TODO: verify trivia kinds and texts

                assert_eq!(
                    lexer.next().unwrap().kind,
                    SyntaxKind::TerminalEndOfFile,
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
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    let res: Vec<LexerTerminal> =
        Lexer::from_text(db, test_source(), "let x: &T = @ 6; //  5+ 3;").collect();
    assert_eq!(
        res,
        vec![
            LexerTerminal {
                text: "let".into(),
                kind: SyntaxKind::TerminalLet,
                leading_trivia: vec![],
                trailing_trivia: vec![TokenWhitespace::new_green(db, " ".into()).into()]
            },
            LexerTerminal {
                text: "x".into(),
                kind: SyntaxKind::TerminalIdentifier,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: ":".into(),
                kind: SyntaxKind::TerminalColon,
                leading_trivia: vec![],
                trailing_trivia: vec![TokenWhitespace::new_green(db, " ".into()).into()]
            },
            LexerTerminal {
                text: "&".into(),
                kind: SyntaxKind::TerminalAnd,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: "T".into(),
                kind: SyntaxKind::TerminalIdentifier,
                leading_trivia: vec![],
                trailing_trivia: vec![TokenWhitespace::new_green(db, " ".into()).into()]
            },
            LexerTerminal {
                text: "=".into(),
                kind: SyntaxKind::TerminalEq,
                leading_trivia: vec![],
                trailing_trivia: vec![TokenWhitespace::new_green(db, " ".into()).into()]
            },
            LexerTerminal {
                text: "@".into(),
                kind: SyntaxKind::TerminalBadCharacters,
                leading_trivia: vec![],
                trailing_trivia: vec![TokenWhitespace::new_green(db, " ".into()).into()]
            },
            LexerTerminal {
                text: "6".into(),
                kind: SyntaxKind::TerminalLiteralNumber,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: ";".into(),
                kind: SyntaxKind::TerminalSemicolon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, " ".into()).into(),
                    TokenSingleLineComment::new_green(db, "//  5+ 3;".into()).into()
                ]
            },
            LexerTerminal {
                text: "".into(),
                kind: SyntaxKind::TerminalEndOfFile,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            }
        ]
    );
}

#[test]
fn test_bad_character() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let text = "@";
    let mut lexer = Lexer::from_text(db, test_source(), text);
    let terminal = lexer.next().unwrap();
    let token_text = terminal.text;
    assert_eq!(
        terminal.kind,
        SyntaxKind::TerminalBadCharacters,
        "Wrong token kind, with text: \"{}\".",
        text
    );
    assert_eq!(token_text, text, "Wrong token text.");

    assert_eq!(
        lexer.next().unwrap().kind,
        SyntaxKind::TerminalEndOfFile,
        "Wrong eof token, with text: \"{}\".",
        text
    );
    assert!(lexer.next().is_none(), "Expected end of lexer stream.");
}
