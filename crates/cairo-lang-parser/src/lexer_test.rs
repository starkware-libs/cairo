use std::sync::Arc;

use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_syntax::node::Token;
use cairo_lang_syntax::node::ast::{TokenSingleLineComment, TokenWhitespace};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_test_utils::test;

use crate::lexer::{LexerTerminal, tokenize_all};
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
                "0b01",
                "0o04321",
                "1234567890123456789012345678901234567890",
                "11_u128",
                "0xA2_u128",
            ]
        }
        SyntaxKind::TerminalShortString => {
            vec!["''", "'a'", "'abc'", "'1234567890123456789012345678901'"]
        }
        SyntaxKind::TerminalString => {
            vec![
                "\"\"",
                "\"a\"",
                "\"abc\"",
                "\"1234567890123456789012345678901\"",
                "\"12345678901234567890123456789012\"",
            ]
        }
        SyntaxKind::TerminalFalse => vec!["false"],
        SyntaxKind::TerminalExtern => vec!["extern"],
        SyntaxKind::TerminalType => vec!["type"],
        SyntaxKind::TerminalFunction => vec!["fn"],
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
        SyntaxKind::TerminalLoop => vec!["loop"],
        SyntaxKind::TerminalBreak => vec!["break"],
        SyntaxKind::TerminalElse => vec!["else"],
        SyntaxKind::TerminalUse => vec!["use"],
        SyntaxKind::TerminalAnd => vec!["&"],
        SyntaxKind::TerminalAndAnd => vec!["&&"],
        SyntaxKind::TerminalAt => vec!["@"],
        SyntaxKind::TerminalColon => vec![":"],
        SyntaxKind::TerminalColonColon => vec!["::"],
        SyntaxKind::TerminalComma => vec![","],
        SyntaxKind::TerminalDiv => vec!["/"],
        SyntaxKind::TerminalDivEq => vec!["/="],
        SyntaxKind::TerminalMod => vec!["%"],
        SyntaxKind::TerminalModEq => vec!["%="],
        SyntaxKind::TerminalDot => vec!["."],
        SyntaxKind::TerminalDotDot => vec![".."],
        SyntaxKind::TerminalDotDotEq => vec!["..="],
        SyntaxKind::TerminalEq => vec!["="],
        SyntaxKind::TerminalEqEq => vec!["=="],
        SyntaxKind::TerminalGE => vec![">="],
        SyntaxKind::TerminalGT => vec![">"],
        SyntaxKind::TerminalLE => vec!["<="],
        SyntaxKind::TerminalLT => vec!["<"],
        SyntaxKind::TerminalMinus => vec!["-"],
        SyntaxKind::TerminalMinusEq => vec!["-="],
        SyntaxKind::TerminalMul => vec!["*"],
        SyntaxKind::TerminalMulEq => vec!["*="],
        SyntaxKind::TerminalNeq => vec!["!="],
        SyntaxKind::TerminalNot => vec!["!"],
        SyntaxKind::TerminalBitNot => vec!["~"],
        SyntaxKind::TerminalOr => vec!["|"],
        SyntaxKind::TerminalOrOr => vec!["||"],
        SyntaxKind::TerminalXor => vec!["^"],
        SyntaxKind::TerminalPlus => vec!["+"],
        SyntaxKind::TerminalPlusEq => vec!["+="],
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
        SyntaxKind::TerminalLoop,
        SyntaxKind::TerminalBreak,
        SyntaxKind::TerminalElse,
        SyntaxKind::TerminalUse,
        SyntaxKind::TerminalAnd,
        SyntaxKind::TerminalAndAnd,
        SyntaxKind::TerminalAt,
        SyntaxKind::TerminalOr,
        SyntaxKind::TerminalOrOr,
        SyntaxKind::TerminalXor,
        SyntaxKind::TerminalEqEq,
        SyntaxKind::TerminalNeq,
        SyntaxKind::TerminalGE,
        SyntaxKind::TerminalGT,
        SyntaxKind::TerminalLE,
        SyntaxKind::TerminalLT,
        SyntaxKind::TerminalNot,
        SyntaxKind::TerminalPlus,
        SyntaxKind::TerminalPlusEq,
        SyntaxKind::TerminalMinus,
        SyntaxKind::TerminalMinusEq,
        SyntaxKind::TerminalMul,
        SyntaxKind::TerminalMulEq,
        SyntaxKind::TerminalDiv,
        SyntaxKind::TerminalDivEq,
        SyntaxKind::TerminalMod,
        SyntaxKind::TerminalModEq,
        SyntaxKind::TerminalColon,
        SyntaxKind::TerminalColonColon,
        SyntaxKind::TerminalComma,
        SyntaxKind::TerminalDot,
        SyntaxKind::TerminalDotDot,
        SyntaxKind::TerminalDotDotEq,
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
        SyntaxKind::TerminalShortString,
        SyntaxKind::TerminalString,
    ]
}

fn token_separators() -> Vec<&'static str> {
    vec![" ", "\t", "\n", " // Comment with unicode é\n"]
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
    if kind0 == SyntaxKind::TerminalShortString
        && matches!(kind1, SyntaxKind::TerminalIdentifier | SyntaxKind::TerminalUnderscore)
    {
        return true;
    }
    if (text0 == "&" && text1.starts_with('&'))
        || (text0 == "|" && text1.starts_with('|'))
        || (text0 == "/" && text1.starts_with('/'))
        || ((text0 == "=" || text0 == "!") && text1.starts_with('='))
        || ((text0 == "=") && text1.starts_with('>'))
        || ((text0 == "<" || text0 == ">") && text1.starts_with('='))
        || (text0 == ":" && text1.starts_with(':'))
        || (text0 == "." && text1.starts_with('.'))
        || (text0 == "-" && (text1.starts_with('>') || text1.starts_with('=')))
        || ((text0 == "+" || text0 == "*" || text0 == "/" || text0 == "%")
            || (text0 == "..") && text1.starts_with('='))
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

#[test]
fn test_lex_single_token() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    for (kind, text) in terminal_kind_and_text() {
        let terminals = tokenize_all(db, (), Arc::from(text));
        let terminal = &terminals[0];
        // TODO(spapini): Remove calling new_root on non root elements.
        assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{text}\".");
        assert_eq!(terminal.text.long(db), text, "Wrong token text.");

        assert_eq!(
            terminals[1].kind,
            SyntaxKind::TerminalEndOfFile,
            "Wrong eof token, with text: \"{text}\"."
        );
        assert_eq!(terminals.len(), 2, "Expected exactly 2 terminals (token + EOF).");
    }
}

#[cairo_lang_proc_macros::test]
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
                let text = format!("{text0}{separator}{text1}");
                let terminals = tokenize_all(db, (), Arc::from(text.as_str()));
                let terminal = &terminals[0];
                let token_text = terminal.text(db);
                assert_eq!(
                    terminal.kind, kind0,
                    "Wrong first token kind: {}, expected: {kind0}. Text: \"{}\".",
                    terminal.kind, token_text
                );
                assert_eq!(
                    token_text, text0,
                    "Wrong first token text, with total text: \"{}\".",
                    token_text
                );

                let terminal = &terminals[1];
                let token_text = terminal.text(db);
                assert_eq!(
                    terminal.kind, kind1,
                    "Wrong second token kind {}, expected: {kind1}. Text: \"{token_text}\".",
                    terminal.kind
                );
                assert_eq!(
                    token_text, text1,
                    "Wrong second token text, with total text: \"{token_text}\".",
                );

                assert_eq!(
                    terminals[2].kind,
                    SyntaxKind::TerminalEndOfFile,
                    "Wrong eof token, with text: \"{text}\".",
                );
                assert_eq!(
                    terminals.len(),
                    3,
                    "Expected exactly 3 terminals (token0 + token1 + EOF)."
                );
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
                let text = format!("{leading_trivia}{expected_token_text} {trailing_trivia}");
                let terminals = tokenize_all(db, (), Arc::from(text.as_str()));
                let terminal = &terminals[0];
                let token_text = terminal.text(db);
                assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{text}\".");
                assert_eq!(token_text, expected_token_text, "Wrong token text.");
                // TODO: verify trivia kinds and texts

                assert_eq!(
                    terminals[1].kind,
                    SyntaxKind::TerminalEndOfFile,
                    "Wrong eof token, with text: \"{text}\"."
                );
                assert_eq!(terminals.len(), 2, "Expected exactly 2 terminals (token + EOF).");
            }
        }
    }
}

#[test]
fn test_cases() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    let res = tokenize_all(db, (), Arc::from("let x: &T = ` 6; //  5+ 3;"));
    assert_eq!(
        res.into_iter().collect::<Vec<_>>(),
        vec![
            LexerTerminal {
                text: SmolStrId::from(db, "let"),
                kind: SyntaxKind::TerminalLet,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "x"),
                kind: SyntaxKind::TerminalIdentifier,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: SmolStrId::from(db, ":"),
                kind: SyntaxKind::TerminalColon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "&"),
                kind: SyntaxKind::TerminalAnd,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "T"),
                kind: SyntaxKind::TerminalIdentifier,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "="),
                kind: SyntaxKind::TerminalEq,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "`"),
                kind: SyntaxKind::TerminalBadCharacters,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "6"),
                kind: SyntaxKind::TerminalLiteralNumber,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: SmolStrId::from(db, ";"),
                kind: SyntaxKind::TerminalSemicolon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into(),
                    TokenSingleLineComment::new_green(db, SmolStrId::from(db, "//  5+ 3;")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, ""),
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

    let text = "`";
    let terminals = tokenize_all(db, (), Arc::from(text));
    let terminal = &terminals[0];
    let token_text = terminal.text(db);
    assert_eq!(
        terminal.kind,
        SyntaxKind::TerminalBadCharacters,
        "Wrong token kind, with text: \"{text}\".",
    );
    assert_eq!(token_text, text, "Wrong token text.");

    assert_eq!(
        terminals[1].kind,
        SyntaxKind::TerminalEndOfFile,
        "Wrong eof token, with text: \"{text}\"."
    );
    assert_eq!(terminals.len(), 2, "Expected exactly 2 terminals (bad char + EOF).");
}
