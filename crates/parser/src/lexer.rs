#[cfg(test)]
#[path = "lexer_test.rs"]
mod test;

use filesystem::ids::FileId;
use filesystem::span::{TextOffset, TextSpan};
use smol_str::SmolStr;
use syntax::node::ast::{TokenNewline, TokenSingleLineComment, TokenWhitespace, TriviumGreen};
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::Token;

pub struct Lexer<'a> {
    db: &'a dyn SyntaxGroup,
    // TODO(yuval): add diagnostics. FileId should be included in diagnostics.
    source: FileId,
    text: &'a str,
    previous_position: TextOffset,
    current_position: TextOffset,
    done: bool,
}

impl<'a> Lexer<'a> {
    // Ctors.
    pub fn from_text(db: &'a dyn SyntaxGroup, source: FileId, text: &'a str) -> Lexer<'a> {
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
    fn match_trivia(&mut self, leading: bool) -> Vec<TriviumGreen> {
        let mut res: Vec<TriviumGreen> = Vec::new();
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
    fn match_trivium_whitespace(&mut self) -> TriviumGreen {
        self.take_while(|s| matches!(s, ' ' | '\r' | '\t'));
        TokenWhitespace::new_green(self.db, SmolStr::from(self.consume_span())).into()
    }

    /// Assumes the next character '/n'.
    fn match_trivium_newline(&mut self) -> TriviumGreen {
        self.take();
        TokenNewline::new_green(self.db, SmolStr::from(self.consume_span())).into()
    }

    /// Assumes the next 2 characters are "//".
    fn match_trivium_single_line_comment(&mut self) -> TriviumGreen {
        self.take_while(|c| c != '\n');
        TokenSingleLineComment::new_green(self.db, SmolStr::from(self.consume_span())).into()
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
            "extern" => TokenKind::Extern,
            "type" => TokenKind::Type,
            "func" => TokenKind::Function,
            "mod" => TokenKind::Module,
            "struct" => TokenKind::Struct,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            "match" => TokenKind::Match,
            "use" => TokenKind::Use,
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

    fn match_terminal(&mut self) -> LexerTerminal {
        let leading_trivia = self.match_trivia(true);

        let kind = if let Some(current) = self.peek() {
            match current {
                '0'..='9' => self.take_token_literal_number(),
                ',' => self.take_token_of_kind(TokenKind::Comma),
                ';' => self.take_token_of_kind(TokenKind::Semicolon),
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
                '=' => {
                    self.take();
                    match self.peek() {
                        Some('=') => self.take_token_of_kind(TokenKind::EqEq),
                        Some('>') => self.take_token_of_kind(TokenKind::MatchArrow),
                        _ => TokenKind::Eq,
                    }
                }
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

        let text = SmolStr::from(self.consume_span());
        let trailing_trivia = self.match_trivia(false);
        let terminal_kind = token_kind_to_terminal_syntax_kind(kind);

        // TODO(yuval): log(verbose) "consumed text: ..."
        LexerTerminal { text, kind: terminal_kind, leading_trivia, trailing_trivia }
    }
}

/// Output terminal emitted by the lexer.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LexerTerminal {
    pub text: SmolStr,
    /// The kind of the inner token of this terminal.
    pub kind: SyntaxKind,
    pub leading_trivia: Vec<TriviumGreen>,
    pub trailing_trivia: Vec<TriviumGreen>,
}
impl LexerTerminal {
    pub fn width(&self, db: &dyn SyntaxGroup) -> u32 {
        self.leading_trivia.iter().map(|t| t.0.width(db)).sum::<u32>()
            + self.text.len() as u32
            + self.trailing_trivia.iter().map(|t| t.0.width(db)).sum::<u32>()
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexerTerminal;

    /// Returns the next token. Once there are no more tokens left, returns token EOF.
    /// One should not call this after EOF was returned. If one does, None is returned.
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let lexer_terminal = self.match_terminal();
        if lexer_terminal.kind == SyntaxKind::TerminalEndOfFile {
            self.done = true;
        };
        Some(lexer_terminal)
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash)]
enum TokenKind {
    Identifier,

    // Literals.
    LiteralNumber,

    // Keywords.
    False,
    True,
    Extern,
    Type,
    Function,
    Module,
    Struct,
    Let,
    Return,
    Match,
    Use,

    // Punctuation.
    And,
    AndAnd,
    OrOr,
    EqEq,
    Neq,
    GE,
    GT,
    LE,
    LT,
    Not,
    Plus,
    Minus,
    Mul,
    Div,

    Colon,
    ColonColon,
    Comma,
    Dot,
    DotDot,
    Eq,
    Semicolon,
    Underscore,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,
    Arrow,
    MatchArrow,

    // Meta.
    EndOfFile,
    BadCharacters,
}

// TODO(yg): consider moving this match into lexer/SyntaxKind, return SyntaxKind from there and
// make TokenKind private there.
fn token_kind_to_terminal_syntax_kind(kind: TokenKind) -> SyntaxKind {
    match kind {
        TokenKind::Identifier => SyntaxKind::TerminalIdentifier,
        TokenKind::LiteralNumber => SyntaxKind::TerminalLiteralNumber,
        TokenKind::False => SyntaxKind::TerminalFalse,
        TokenKind::True => SyntaxKind::TerminalTrue,
        TokenKind::Extern => SyntaxKind::TerminalExtern,
        TokenKind::Type => SyntaxKind::TerminalType,
        TokenKind::Function => SyntaxKind::TerminalFunction,
        TokenKind::Module => SyntaxKind::TerminalModule,
        TokenKind::Struct => SyntaxKind::TerminalStruct,
        TokenKind::Let => SyntaxKind::TerminalLet,
        TokenKind::Return => SyntaxKind::TerminalReturn,
        TokenKind::Match => SyntaxKind::TerminalMatch,
        TokenKind::Use => SyntaxKind::TerminalUse,
        TokenKind::And => SyntaxKind::TerminalAnd,
        TokenKind::AndAnd => SyntaxKind::TerminalAndAnd,
        TokenKind::OrOr => SyntaxKind::TerminalOrOr,
        TokenKind::EqEq => SyntaxKind::TerminalEqEq,
        TokenKind::Neq => SyntaxKind::TerminalNeq,
        TokenKind::GE => SyntaxKind::TerminalGE,
        TokenKind::GT => SyntaxKind::TerminalGT,
        TokenKind::LE => SyntaxKind::TerminalLE,
        TokenKind::LT => SyntaxKind::TerminalLT,
        TokenKind::Not => SyntaxKind::TerminalNot,
        TokenKind::Plus => SyntaxKind::TerminalPlus,
        TokenKind::Minus => SyntaxKind::TerminalMinus,
        TokenKind::Mul => SyntaxKind::TerminalMul,
        TokenKind::Div => SyntaxKind::TerminalDiv,
        TokenKind::Colon => SyntaxKind::TerminalColon,
        TokenKind::ColonColon => SyntaxKind::TerminalColonColon,
        TokenKind::Comma => SyntaxKind::TerminalComma,
        TokenKind::Dot => SyntaxKind::TerminalDot,
        TokenKind::DotDot => SyntaxKind::TerminalDotDot,
        TokenKind::Eq => SyntaxKind::TerminalEq,
        TokenKind::Semicolon => SyntaxKind::TerminalSemicolon,
        TokenKind::Underscore => SyntaxKind::TerminalUnderscore,
        TokenKind::LBrace => SyntaxKind::TerminalLBrace,
        TokenKind::RBrace => SyntaxKind::TerminalRBrace,
        TokenKind::LBrack => SyntaxKind::TerminalLBrack,
        TokenKind::RBrack => SyntaxKind::TerminalRBrack,
        TokenKind::LParen => SyntaxKind::TerminalLParen,
        TokenKind::RParen => SyntaxKind::TerminalRParen,
        TokenKind::Arrow => SyntaxKind::TerminalArrow,
        TokenKind::MatchArrow => SyntaxKind::TerminalMatchArrow,
        TokenKind::BadCharacters => SyntaxKind::TerminalBadCharacters,
        TokenKind::EndOfFile => SyntaxKind::TerminalEndOfFile,
    }
}
