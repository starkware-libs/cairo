use smol_str::SmolStr;

use syntax::{
    node::{
        ast::{Terminal, Trivia, TriviumNewline, TriviumSingleLineComment, TriviumWhitespace},
        GreenId, GreenInterner, SyntaxToken,
    },
    token::TokenKind,
};

use crate::text::{CodeSource, TextPosition, TextSpan};

pub struct Lexer<'a> {
    db: &'a dyn GreenInterner,
    source: CodeSource,
    text: &'a str,
    last_position: TextPosition,
    position: TextPosition,
    done: bool,
}

impl<'a> Lexer<'a> {
    // Ctors.
    #[allow(dead_code)]
    pub fn from_text(db: &'a dyn GreenInterner, source: CodeSource, text: &'a str) -> Lexer<'a> {
        Lexer {
            db,
            source,
            text,
            last_position: TextPosition::default(),
            position: TextPosition::default(),
            done: false,
        }
    }

    // Helpers.
    fn peek(&self) -> Option<char> {
        return self.text[self.position.offset as usize..].chars().next();
    }

    fn peek_nth(&self, n: usize) -> Option<char> {
        return self.text[self.position.offset as usize..].chars().nth(n);
    }

    fn take(&mut self) -> Option<char> {
        let res = self.peek()?;
        self.position.offset += 1;
        self.position.col += 1;
        if res == '\n' {
            self.position.line += 1;
            self.position.col = 1;
        }
        Some(res)
    }

    fn current_span(&self) -> &str {
        &self.text[self.last_position.offset as usize..self.position.offset as usize]
    }

    fn consume_span(&mut self) -> (SmolStr, TextSpan) {
        let span = TextSpan { from: self.last_position, to: self.position };
        let text = SmolStr::from(self.current_span());
        self.last_position = self.position;
        (text, span)
    }

    // Trivia matchers.
    fn match_trivia(&mut self, leading: bool) -> Vec<GreenId> {
        let mut res: Vec<GreenId> = Vec::new();
        while let Some(current) = self.peek() {
            let trivium = match current {
                ' ' | '\r' | '\t' => self.match_trivium_whitespace(),
                '\n' => self.match_trivium_newline(),
                '/' if self.peek_nth(1) == Some('/') => self.match_trivium_single_line_comment(),
                _ => break,
            };
            res.push(trivium);
            if current == '\n' && !leading {
                break;
            }
        }
        res
    }

    fn match_trivium_whitespace(&mut self) -> GreenId {
        while self.peek().map(|s| matches!(s, ' ' | '\t' | '\r')).unwrap_or(false) {
            self.take();
        }
        let (text, _span) = self.consume_span();
        TriviumWhitespace::new_green(
            self.db,
            SyntaxToken::new_green(self.db, TokenKind::Whitespace, text),
        )
    }

    fn match_trivium_newline(&mut self) -> GreenId {
        self.take();
        let (text, _span) = self.consume_span();
        TriviumNewline::new_green(
            self.db,
            SyntaxToken::new_green(self.db, TokenKind::Newline, text),
        )
    }

    // Assumes the next 2 characters are "//".
    fn match_trivium_single_line_comment(&mut self) -> GreenId {
        while !matches!(self.take(), Some('\n') | None) {}
        let (text, _span) = self.consume_span();
        TriviumSingleLineComment::new_green(
            self.db,
            SyntaxToken::new_green(self.db, TokenKind::SingleLineComment, text),
        )
    }

    // Token matchers.
    fn match_token_number(&mut self) -> TokenKind {
        while self.peek().map(|s| s.is_ascii_digit()).unwrap_or(false) {
            self.take();
        }
        TokenKind::LiteralNumber
    }

    fn match_token_identifier(&mut self) -> TokenKind {
        // TODO(spapini): Support or excplicitly report general unicode characters.
        while self.peek().map(|s| s.is_ascii_alphanumeric() || s == '_').unwrap_or(false) {
            self.take();
        }

        let word = self.current_span();
        match word {
            "false" => TokenKind::False,
            "true" => TokenKind::True,
            "fn" => TokenKind::Function,
            "mod" => TokenKind::Module,
            "struct" => TokenKind::Struct,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            "_" => TokenKind::Underscore,
            _ => TokenKind::Identifier,
        }
    }

    fn match_single(&mut self, kind: TokenKind) -> TokenKind {
        self.take();
        kind
    }

    fn match_terminal(&mut self) -> (TokenKind, GreenId) {
        let leading_trivia = self.match_trivia(true);

        let kind = if let Some(current) = self.peek() {
            match current {
                '0'..='9' => self.match_token_number(),
                ',' => self.match_single(TokenKind::Comma),
                ';' => self.match_single(TokenKind::Semi),
                '{' => self.match_single(TokenKind::LBrace),
                '}' => self.match_single(TokenKind::RBrace),
                '[' => self.match_single(TokenKind::LBrack),
                ']' => self.match_single(TokenKind::RBrack),
                '(' => self.match_single(TokenKind::LParen),
                ')' => self.match_single(TokenKind::RParen),
                '.' => self.match_single(TokenKind::Dot),
                '*' => self.match_single(TokenKind::Mul),
                '/' => self.match_single(TokenKind::Div),
                '+' => self.match_single(TokenKind::Plus),
                '-' => self.match_single(TokenKind::Minus),
                '<' => self.match_single(TokenKind::LT),
                '>' => self.match_single(TokenKind::GT),
                'a'..='z' | 'A'..='Z' | '_' => self.match_token_identifier(),
                ':' => {
                    self.take();
                    if self.peek() == Some(':') {
                        self.take();
                        TokenKind::ColonColon
                    } else {
                        TokenKind::Colon
                    }
                }
                '!' => {
                    self.take();
                    if self.peek() == Some('=') {
                        self.take();
                        TokenKind::Neq
                    } else {
                        TokenKind::Not
                    }
                }
                '=' => {
                    self.take();
                    if self.peek() == Some('=') {
                        self.take();
                        TokenKind::EqEq
                    } else {
                        TokenKind::Eq
                    }
                }
                '&' => {
                    self.take();
                    if self.peek() == Some('&') {
                        self.take();
                        TokenKind::AndAnd
                    } else {
                        TokenKind::And
                    }
                }
                '|' if self.peek() == Some('|') => {
                    self.take();
                    self.take();
                    TokenKind::Or
                }
                _ => self.match_single(TokenKind::BadCharacter),
            }
        } else {
            TokenKind::EndOfFile
        };

        let (text, span) = self.consume_span();
        let trailing_trivia = self.match_trivia(false);

        if kind == TokenKind::BadCharacter {
            // TODO(spapini): Add to diagnostics.
            println!("Bad character at {}", span.for_source(self.source.clone()));
        }

        (
            kind,
            Terminal::new_green(
                self.db,
                Trivia::new_green(self.db, leading_trivia),
                SyntaxToken::new_green(self.db, kind, text),
                Trivia::new_green(self.db, trailing_trivia),
            ),
        )
    }
}

impl Iterator for Lexer<'_> {
    type Item = (TokenKind, GreenId);

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let (kind, current) = self.match_terminal();
        if kind == TokenKind::EndOfFile {
            self.done = true;
        };
        Some((kind, current))
    }
}
