use std::sync::Arc;

use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_syntax::node::Token;
use cairo_lang_syntax::node::ast::{TokenSingleLineComment, TokenWhitespace};
use cairo_lang_syntax::node::kind::{LexemeKind, SyntaxKind, TriviaKind};
use cairo_lang_test_utils::test;
use itertools::Itertools;
use salsa::Database;

use crate::lexer::{Lexer, LexerTerminal};
use crate::utils::SimpleParserDatabase;

// TODO(spapini): Use snapshot/regression tests.

fn terminal_kind_to_text(kind: LexemeKind) -> Vec<&'static str> {
    match kind {
        LexemeKind::Identifier => vec!["abc", "_az12f", "A90g5__"],
        LexemeKind::LiteralNumber => {
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
        LexemeKind::ShortString => {
            vec!["''", "'a'", "'abc'", "'1234567890123456789012345678901'"]
        }
        LexemeKind::String => {
            vec![
                "\"\"",
                "\"a\"",
                "\"abc\"",
                "\"1234567890123456789012345678901\"",
                "\"12345678901234567890123456789012\"",
            ]
        }
        LexemeKind::False => vec!["false"],
        LexemeKind::Extern => vec!["extern"],
        LexemeKind::Type => vec!["type"],
        LexemeKind::Function => vec!["fn"],
        LexemeKind::Trait => vec!["trait"],
        LexemeKind::Impl => vec!["impl"],
        LexemeKind::Of => vec!["of"],
        LexemeKind::Let => vec!["let"],
        LexemeKind::Mut => vec!["mut"],
        LexemeKind::Ref => vec!["ref"],
        LexemeKind::NoPanic => vec!["nopanic"],
        LexemeKind::Module => vec!["mod"],
        LexemeKind::Struct => vec!["struct"],
        LexemeKind::Enum => vec!["enum"],
        LexemeKind::True => vec!["true"],
        LexemeKind::Return => vec!["return"],
        LexemeKind::Match => vec!["match"],
        LexemeKind::If => vec!["if"],
        LexemeKind::Loop => vec!["loop"],
        LexemeKind::Break => vec!["break"],
        LexemeKind::Else => vec!["else"],
        LexemeKind::Use => vec!["use"],
        LexemeKind::And => vec!["&"],
        LexemeKind::AndAnd => vec!["&&"],
        LexemeKind::At => vec!["@"],
        LexemeKind::Colon => vec![":"],
        LexemeKind::ColonColon => vec!["::"],
        LexemeKind::Comma => vec![","],
        LexemeKind::Div => vec!["/"],
        LexemeKind::DivEq => vec!["/="],
        LexemeKind::Mod => vec!["%"],
        LexemeKind::ModEq => vec!["%="],
        LexemeKind::Dot => vec!["."],
        LexemeKind::DotDot => vec![".."],
        LexemeKind::DotDotEq => vec!["..="],
        LexemeKind::Eq => vec!["="],
        LexemeKind::EqEq => vec!["=="],
        LexemeKind::GE => vec![">="],
        LexemeKind::GT => vec![">"],
        LexemeKind::LE => vec!["<="],
        LexemeKind::LT => vec!["<"],
        LexemeKind::Minus => vec!["-"],
        LexemeKind::MinusEq => vec!["-="],
        LexemeKind::Mul => vec!["*"],
        LexemeKind::MulEq => vec!["*="],
        LexemeKind::Neq => vec!["!="],
        LexemeKind::Not => vec!["!"],
        LexemeKind::BitNot => vec!["~"],
        LexemeKind::Or => vec!["|"],
        LexemeKind::OrOr => vec!["||"],
        LexemeKind::Xor => vec!["^"],
        LexemeKind::Plus => vec!["+"],
        LexemeKind::PlusEq => vec!["+="],
        LexemeKind::Semicolon => vec![";"],
        LexemeKind::QuestionMark => vec!["?"],
        LexemeKind::Underscore => vec!["_"],
        LexemeKind::LBrace => vec!["{"],
        LexemeKind::RBrace => vec!["}"],
        LexemeKind::LBrack => vec!["["],
        LexemeKind::RBrack => vec!["]"],
        LexemeKind::LParen => vec!["("],
        LexemeKind::RParen => vec![")"],
        LexemeKind::Arrow => vec!["->"],
        LexemeKind::MatchArrow => vec!["=>"],
        LexemeKind::EndOfFile => vec![],
        _ => vec![],
    }
}

fn terminal_kinds() -> Vec<LexemeKind> {
    vec![
        LexemeKind::Identifier,
        LexemeKind::LiteralNumber,
        LexemeKind::False,
        LexemeKind::True,
        LexemeKind::Extern,
        LexemeKind::Type,
        LexemeKind::Function,
        LexemeKind::Trait,
        LexemeKind::Impl,
        LexemeKind::Of,
        LexemeKind::Module,
        LexemeKind::Struct,
        LexemeKind::Enum,
        LexemeKind::Let,
        LexemeKind::Mut,
        LexemeKind::Ref,
        LexemeKind::NoPanic,
        LexemeKind::Return,
        LexemeKind::Match,
        LexemeKind::If,
        LexemeKind::Loop,
        LexemeKind::Break,
        LexemeKind::Else,
        LexemeKind::Use,
        LexemeKind::And,
        LexemeKind::AndAnd,
        LexemeKind::At,
        LexemeKind::Or,
        LexemeKind::OrOr,
        LexemeKind::Xor,
        LexemeKind::EqEq,
        LexemeKind::Neq,
        LexemeKind::GE,
        LexemeKind::GT,
        LexemeKind::LE,
        LexemeKind::LT,
        LexemeKind::Not,
        LexemeKind::Plus,
        LexemeKind::PlusEq,
        LexemeKind::Minus,
        LexemeKind::MinusEq,
        LexemeKind::Mul,
        LexemeKind::MulEq,
        LexemeKind::Div,
        LexemeKind::DivEq,
        LexemeKind::Mod,
        LexemeKind::ModEq,
        LexemeKind::Colon,
        LexemeKind::ColonColon,
        LexemeKind::Comma,
        LexemeKind::Dot,
        LexemeKind::DotDot,
        LexemeKind::DotDotEq,
        LexemeKind::Eq,
        LexemeKind::Semicolon,
        LexemeKind::QuestionMark,
        LexemeKind::Underscore,
        LexemeKind::LBrace,
        LexemeKind::RBrace,
        LexemeKind::LBrack,
        LexemeKind::RBrack,
        LexemeKind::LParen,
        LexemeKind::RParen,
        LexemeKind::Arrow,
        LexemeKind::MatchArrow,
        LexemeKind::EndOfFile,
        LexemeKind::ShortString,
        LexemeKind::String,
    ]
}

fn token_separators() -> Vec<&'static str> {
    vec![" ", "\t", "\n", " // Comment with unicode é\n"]
}

fn need_separator(
    kind0: LexemeKind,
    text0: &'static str,
    kind1: LexemeKind,
    text1: &'static str,
) -> bool {
    if is_identifier_like(kind0)
        && (is_identifier_like(kind1) || matches!(kind1, LexemeKind::LiteralNumber))
    {
        return true;
    }
    if kind0 == LexemeKind::LiteralNumber && (kind0 == kind1 || is_identifier_like(kind1)) {
        return true;
    }
    if kind0 == LexemeKind::ShortString
        && matches!(kind1, LexemeKind::Identifier | LexemeKind::Underscore)
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
        || (kind0 == LexemeKind::LiteralNumber && kind0 == kind1)
    {
        return true;
    }
    false
}

fn is_identifier_like(kind: LexemeKind) -> bool {
    matches!(kind, LexemeKind::Identifier | LexemeKind::Underscore) || kind.is_keyword()
}

fn terminal_kind_and_text() -> Vec<(LexemeKind, &'static str)> {
    let mut res: Vec<(LexemeKind, &'static str)> = Vec::new();
    for kind in terminal_kinds() {
        for text in terminal_kind_to_text(kind) {
            res.push((kind, text));
        }
    }
    res
}

fn trivia_kinds() -> Vec<SyntaxKind> {
    vec![
        SyntaxKind::TriviaToken(TriviaKind::Whitespace),
        SyntaxKind::TriviaToken(TriviaKind::Newline),
        SyntaxKind::TriviaToken(TriviaKind::SingleLineComment),
    ]
}
fn trivia_kind_to_text(kind: SyntaxKind) -> Vec<&'static str> {
    match kind {
        SyntaxKind::TriviaToken(TriviaKind::SingleLineComment) => {
            vec!["// abc def\n", "///\n", "//=\n"]
        }
        SyntaxKind::TriviaToken(TriviaKind::Whitespace) => vec![" ", "\t", "\r"],
        SyntaxKind::TriviaToken(TriviaKind::Newline) => vec!["\n"],
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
        let terminals = tokenize_all(db, Arc::from(text));
        let terminal = &terminals[0];
        // TODO(spapini): Remove calling new_root on non root elements.
        assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{text}\".");
        assert_eq!(terminal.text.long(db), text, "Wrong token text.");

        assert_eq!(
            terminals[1].kind,
            LexemeKind::EndOfFile,
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
                let terminals = tokenize_all(db, Arc::from(text.as_str()));
                let terminal = &terminals[0];
                let token_text = terminal.text(db);
                assert_eq!(
                    terminal.kind, kind0,
                    "Wrong first token kind: {:?}, expected: {kind0:?}. Text: \"{}\".",
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
                    "Wrong second token kind {:?}, expected: {kind1:?}. Text: \"{token_text}\".",
                    terminal.kind
                );
                assert_eq!(
                    token_text, text1,
                    "Wrong second token text, with total text: \"{token_text}\".",
                );

                assert_eq!(
                    terminals[2].kind,
                    LexemeKind::EndOfFile,
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
                let terminals = tokenize_all(db, Arc::from(text.as_str()));
                let terminal = &terminals[0];
                let token_text = terminal.text(db);
                assert_eq!(terminal.kind, kind, "Wrong token kind, with text: \"{text}\".");
                assert_eq!(token_text, expected_token_text, "Wrong token text.");
                // TODO: verify trivia kinds and texts

                assert_eq!(
                    terminals[1].kind,
                    LexemeKind::EndOfFile,
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
    let res = tokenize_all(db, Arc::from("let x: &T = ` 6; //  5+ 3;"));
    assert_eq!(
        res.into_iter().collect::<Vec<_>>(),
        vec![
            LexerTerminal {
                text: SmolStrId::from(db, "let"),
                kind: LexemeKind::Let,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "x"),
                kind: LexemeKind::Identifier,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: SmolStrId::from(db, ":"),
                kind: LexemeKind::Colon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "&"),
                kind: LexemeKind::And,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "T"),
                kind: LexemeKind::Identifier,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "="),
                kind: LexemeKind::Eq,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "`"),
                kind: LexemeKind::BadCharacters,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, "6"),
                kind: LexemeKind::LiteralNumber,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            },
            LexerTerminal {
                text: SmolStrId::from(db, ";"),
                kind: LexemeKind::Semicolon,
                leading_trivia: vec![],
                trailing_trivia: vec![
                    TokenWhitespace::new_green(db, SmolStrId::from(db, " ")).into(),
                    TokenSingleLineComment::new_green(db, SmolStrId::from(db, "//  5+ 3;")).into()
                ]
            },
            LexerTerminal {
                text: SmolStrId::from(db, ""),
                kind: LexemeKind::EndOfFile,
                leading_trivia: vec![],
                trailing_trivia: vec![]
            }
        ]
    );
}

/// `///` is a doc comment and `//!` an inner doc comment, but `////` (4+ slashes) is a regular
/// comment, matching `cairo-lang-doc` (which discards a doc comment whose content starts with `/`).
#[test]
fn test_doc_comment_classification() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;
    let text = "// regular\n/// doc\n//! inner\n//// regular\n///// regular\n";
    let terminal = tokenize_all(db, Arc::from(text)).into_iter().exactly_one().unwrap();
    assert_eq!(
        terminal.leading_trivia.into_iter().map(|t| t.0.long(db).kind).collect_vec(),
        [
            SyntaxKind::TriviaToken(TriviaKind::SingleLineComment),
            SyntaxKind::TriviaToken(TriviaKind::Newline),
            SyntaxKind::TriviaToken(TriviaKind::SingleLineDocComment),
            SyntaxKind::TriviaToken(TriviaKind::Newline),
            SyntaxKind::TriviaToken(TriviaKind::SingleLineInnerComment),
            SyntaxKind::TriviaToken(TriviaKind::Newline),
            SyntaxKind::TriviaToken(TriviaKind::SingleLineComment),
            SyntaxKind::TriviaToken(TriviaKind::Newline),
            SyntaxKind::TriviaToken(TriviaKind::SingleLineComment),
            SyntaxKind::TriviaToken(TriviaKind::Newline)
        ]
    );
}

#[test]
fn test_bad_character() {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let text = "`";
    let terminals = tokenize_all(db, Arc::from(text));
    let terminal = &terminals[0];
    let token_text = terminal.text(db);
    assert_eq!(
        terminal.kind,
        LexemeKind::BadCharacters,
        "Wrong token kind, with text: \"{text}\".",
    );
    assert_eq!(token_text, text, "Wrong token text.");

    assert_eq!(terminals[1].kind, LexemeKind::EndOfFile, "Wrong eof token, with text: \"{text}\".");
    assert_eq!(terminals.len(), 2, "Expected exactly 2 terminals (bad char + EOF).");
}

/// Tokenizes the entire text and returns a vector of terminals.
pub fn tokenize_all<'db>(db: &'db dyn Database, text: Arc<str>) -> Vec<LexerTerminal<'db>> {
    let mut lexer = Lexer::new(text);
    let mut result = vec![];
    loop {
        let terminal = lexer.match_terminal(db);
        let is_eof = terminal.kind == LexemeKind::EndOfFile;
        result.push(terminal);
        if is_eof {
            break;
        }
    }
    result
}
