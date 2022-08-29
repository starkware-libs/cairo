#[cfg(test)]
#[path = "lexer_test.rs"]
mod test;

use filesystem::ids::FileId;
use filesystem::span::{TextOffset, TextSpan};
use smol_str::SmolStr;
use syntax::node::ast::{Terminal, Trivia};
use syntax::node::db::GreenInterner;
use syntax::node::ids::GreenId;
use syntax::node::Token;
use syntax::token::TokenKind;

pub struct Lexer<'a> {
    db: &'a dyn GreenInterner,
    // TODO(yuval): add diagnostics. FileId should be included in diagnostics.
    source: FileId,
    text: &'a str,
    previous_position: TextOffset,
    current_position: TextOffset,
    done: bool,
}

impl<'a> Lexer<'a> {
    // Ctors.
    pub fn from_text(db: &'a dyn GreenInterner, source: FileId, text: &'a str) -> Lexer<'a> {
        Lexer {
            db,
            source,
            text,
            previous_position: TextOffset::default(),
            current_position: TextOffset::default(),
            done: false,
        }
    }

    pub fn position(&self) -> TextOffset {
        self.current_position
    }

    // Helpers.
    fn peek(&self) -> Option<char> {
        self.text[self.current_position.0..].chars().next()
    }

    fn peek_nth(&self, n: usize) -> Option<char> {
        self.text[self.current_position.0..].chars().nth(n)
    }

    fn take(&mut self) -> Option<char> {
        let res = self.peek()?;
        self.current_position.inc();
        Some(res)
    }

    /// Takes a character while the given function returns true.
    fn take_while<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        while self.peek().map(&f).unwrap_or(false) {
            self.take();
        }
    }

    fn peek_span_text(&self) -> &'a str {
        &self.text[self.previous_position.0..self.current_position.0]
    }
    fn peek_span(&self) -> TextSpan {
        TextSpan { start: self.previous_position, end: self.current_position }
    }

    fn consume_span(&mut self) -> &str {
        let val = self.peek_span_text();
        self.previous_position = self.current_position;
        val
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

    /// Assumes the next character is one of [' ', '\r', '\t'].
    fn match_trivium_whitespace(&mut self) -> GreenId {
        self.take_while(|s| matches!(s, ' ' | '\r' | '\t'));
        Token::new_green(self.db, TokenKind::Whitespace, SmolStr::from(self.consume_span()))
    }

    /// Assumes the next character '/n'.
    fn match_trivium_newline(&mut self) -> GreenId {
        self.take();
        Token::new_green(self.db, TokenKind::Newline, SmolStr::from(self.consume_span()))
    }

    /// Assumes the next 2 characters are "//".
    fn match_trivium_single_line_comment(&mut self) -> GreenId {
        self.take_while(|c| c != '\n');
        Token::new_green(self.db, TokenKind::SingleLineComment, SmolStr::from(self.consume_span()))
    }

    // Token matchers.
    fn take_token_literal_number(&mut self) -> TokenKind {
        self.take_while(|c| c.is_ascii_digit());
        TokenKind::LiteralNumber
    }

    /// Assumes the next character is [a-zA-Z_].
    fn take_token_identifier(&mut self) -> TokenKind {
        // TODO(spapini): Support or explicitly report general unicode characters.
        self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        match self.peek_span_text() {
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

    /// Takes a single character and returns the given kind.
    fn take_token_of_kind(&mut self, kind: TokenKind) -> TokenKind {
        self.take();
        kind
    }

    /// If the next character is `second_char`, returns `long_kind`, otherwise returns `short_kind`.
    fn pick_kind(
        &mut self,
        second_char: char,
        long_kind: TokenKind,
        short_kind: TokenKind,
    ) -> TokenKind {
        self.take();
        if self.peek() == Some(second_char) {
            self.take();
            long_kind
        } else {
            short_kind
        }
    }

    fn match_terminal(&mut self) -> TerminalWithKind {
        let leading_trivia = self.match_trivia(true);

        let kind = if let Some(current) = self.peek() {
            match current {
                '0'..='9' => self.take_token_literal_number(),
                ',' => self.take_token_of_kind(TokenKind::Comma),
                ';' => self.take_token_of_kind(TokenKind::Semi),
                '{' => self.take_token_of_kind(TokenKind::LBrace),
                '}' => self.take_token_of_kind(TokenKind::RBrace),
                '[' => self.take_token_of_kind(TokenKind::LBrack),
                ']' => self.take_token_of_kind(TokenKind::RBrack),
                '(' => self.take_token_of_kind(TokenKind::LParen),
                ')' => self.take_token_of_kind(TokenKind::RParen),
                '.' => self.pick_kind('.', TokenKind::DotDot, TokenKind::Dot),
                '*' => self.take_token_of_kind(TokenKind::Mul),
                '/' => self.take_token_of_kind(TokenKind::Div),
                '+' => self.take_token_of_kind(TokenKind::Plus),
                '-' => self.pick_kind('>', TokenKind::Arrow, TokenKind::Minus),
                '<' => self.pick_kind('=', TokenKind::LE, TokenKind::LT),
                '>' => self.pick_kind('=', TokenKind::GE, TokenKind::GT),
                'a'..='z' | 'A'..='Z' | '_' => self.take_token_identifier(),
                ':' => self.pick_kind(':', TokenKind::ColonColon, TokenKind::Colon),
                '!' => self.pick_kind('=', TokenKind::Neq, TokenKind::Not),
                '=' => self.pick_kind('=', TokenKind::EqEq, TokenKind::Eq),
                '&' => self.pick_kind('&', TokenKind::AndAnd, TokenKind::And),
                '|' if self.peek() == Some('|') => {
                    self.take();
                    self.take();
                    TokenKind::OrOr
                }
                _ => {
                    // TODO(yuval): Add to diagnostics instead of printing.
                    println!(
                        "Bad character at {:?}, offset {}",
                        self.source,
                        self.peek_span().start.0
                    );
                    self.take_token_of_kind(TokenKind::BadCharacters)
                }
            }
        } else {
            TokenKind::EndOfFile
        };

        let token_text = SmolStr::from(self.consume_span());
        let trailing_trivia = self.match_trivia(false);

        // TODO(yuval): log(verbose) "consumed text: ..."
        TerminalWithKind {
            terminal: Terminal::new_green(
                self.db,
                Trivia::new_green(self.db, leading_trivia),
                Token::new_green(self.db, kind, token_text),
                Trivia::new_green(self.db, trailing_trivia),
            ),
            kind,
        }
    }
}

pub struct TerminalWithKind {
    /// The green ID of the terminal node
    pub terminal: GreenId,
    /// the kind of the inner token of the terminal
    pub kind: TokenKind,
}

impl Iterator for Lexer<'_> {
    type Item = TerminalWithKind;

    /// Returns the next token. Once there are no more tokens left, returns token EOF.
    /// One should not call this after EOF was returned. If one does, None is returned.
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let terminal_with_kind = self.match_terminal();
        if terminal_with_kind.kind == TokenKind::EndOfFile {
            self.done = true;
        };
        Some(terminal_with_kind)
    }
}
