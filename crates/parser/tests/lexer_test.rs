use syntax::{
    node::{
        ast::{Terminal, Trivia, TriviumSingleLineComment, TriviumWhitespace},
        GreenDatabase, GreenId, SyntaxNode, SyntaxToken, TypedSyntaxNode,
    },
    token::TokenKind,
};

use parser::{lexer::Lexer, text::CodeSource};

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
        TokenKind::Missing => vec![],
        TokenKind::SingleLineComment => vec![],
        TokenKind::Whitespace => vec![],
        TokenKind::Newline => vec![],
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
    CodeSource { file_path: "test".into() }
}

#[salsa::database(GreenDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_lex_single_token() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    for (kind, text) in token_kind_and_text() {
        let mut lexer = Lexer::from_text(db, test_source(), text);
        let (_kind, token) = lexer.next().unwrap();
        let token = Terminal::from_syntax_node(db, SyntaxNode::new_root(token)).token(db).raw(db);
        assert_eq!(token.kind, kind, "Wrong token kind, with text: \"{}\".", text);
        assert_eq!(token.text, text, "Wrong token text.");

        assert_eq!(
            lexer.next().unwrap().0,
            TokenKind::EndOfFile,
            "Wrong eof token, with text: \"{}\".",
            text
        );
        assert!(lexer.next().is_none(), "Expected end of lexer stream.");
    }
}

#[test]
fn test_lex_double_token() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    for (kind0, text0) in token_kind_and_text() {
        for (kind1, text1) in token_kind_and_text() {
            let separators = if need_separator(kind0, text0, kind1, text1) {
                token_separators()
            } else {
                vec![""]
            };
            for separator in separators {
                let text = format!("{}{}{}", text0, separator, text1);
                let mut lexer = Lexer::from_text(db, test_source(), text.as_str());
                let (_kind, token) = lexer.next().unwrap();
                let token =
                    Terminal::from_syntax_node(db, SyntaxNode::new_root(token)).token(db).raw(db);
                assert_eq!(token.kind, kind0, "Wrong token kind0, with text: \"{}\".", text);
                assert_eq!(token.text, text0, "Wrong token text, with total text: \"{}\".", text);
                let (_kind, token) = lexer.next().unwrap();
                let token =
                    Terminal::from_syntax_node(db, SyntaxNode::new_root(token)).token(db).raw(db);
                assert_eq!(token.kind, kind1, "Wrong token kind0, with text: \"{}\".", text);
                assert_eq!(token.text, text1, "Wrong token text, with total text: \"{}\".", text);
                assert_eq!(
                    lexer.next().unwrap().0,
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
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    let res: Vec<GreenId> = Lexer::from_text(db, test_source(), "let x: &T = @ 6; //  5+ 3;")
        .map(|(_kind, term)| term)
        .collect();
    assert_eq!(
        res,
        vec![
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::Let, "let".into()),
                Trivia::new_green(
                    db,
                    vec![TriviumWhitespace::new_green(
                        db,
                        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into())
                    )]
                ),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::Identifier, "x".into()),
                Trivia::new_green(db, vec![]),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::Colon, ":".into()),
                Trivia::new_green(
                    db,
                    vec![TriviumWhitespace::new_green(
                        db,
                        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into())
                    )]
                ),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::And, "&".into()),
                Trivia::new_green(db, vec![]),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::Identifier, "T".into()),
                Trivia::new_green(
                    db,
                    vec![TriviumWhitespace::new_green(
                        db,
                        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into())
                    )]
                ),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::Eq, "=".into()),
                Trivia::new_green(
                    db,
                    vec![TriviumWhitespace::new_green(
                        db,
                        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into())
                    )]
                ),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::BadCharacter, "@".into()),
                Trivia::new_green(
                    db,
                    vec![TriviumWhitespace::new_green(
                        db,
                        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into())
                    )]
                ),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::LiteralNumber, "6".into()),
                Trivia::new_green(db, vec![]),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::Semi, ";".into()),
                Trivia::new_green(
                    db,
                    vec![
                        TriviumWhitespace::new_green(
                            db,
                            SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into())
                        ),
                        TriviumSingleLineComment::new_green(
                            db,
                            SyntaxToken::new_green(
                                db,
                                TokenKind::SingleLineComment,
                                "//  5+ 3;".into()
                            )
                        )
                    ]
                ),
            ),
            Terminal::new_green(
                db,
                Trivia::new_green(db, vec![]),
                SyntaxToken::new_green(db, TokenKind::EndOfFile, "".into()),
                Trivia::new_green(db, vec![]),
            )
        ]
    );
}
