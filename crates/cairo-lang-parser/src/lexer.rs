#[cfg(test)]
#[path = "lexer_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::Token;
use cairo_lang_syntax::node::ast::{
    TokenNewline, TokenSingleLineComment, TokenSingleLineDocComment, TokenSingleLineInnerComment,
    TokenWhitespace, TriviumGreen,
};
use cairo_lang_syntax::node::kind::LexemeKind;
use salsa::Database;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Lexer {
    text: Arc<str>,
    previous_position: TextOffset,
    current_position: TextOffset,
}

impl Lexer {
    /// Creates a new lexer with the given text.
    pub fn new(text: Arc<str>) -> Self {
        Self { text, previous_position: TextOffset::START, current_position: TextOffset::START }
    }

    // Helpers.
    fn peek(&self) -> Option<char> {
        self.current_position.take_from(&self.text).chars().next()
    }

    fn peek_nth(&self, n: usize) -> Option<char> {
        self.current_position.take_from(&self.text).chars().nth(n)
    }

    fn take(&mut self) -> Option<char> {
        let res = self.peek()?;
        self.current_position = self.current_position.add_width(TextWidth::from_char(res));
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

    fn peek_text_span(&self) -> TextSpan {
        TextSpan::new(self.previous_position, self.current_position)
    }

    fn consume_text_span(&mut self) -> TextSpan {
        let val = self.peek_text_span();
        self.previous_position = self.current_position;
        val
    }

    // Trivia matchers.
    fn match_trivia<'a>(&mut self, db: &'a dyn Database, leading: bool) -> Vec<TriviumGreen<'a>> {
        let mut res: Vec<TriviumGreen<'a>> = Vec::new();
        while let Some(current) = self.peek() {
            let trivium = match current {
                ' ' | '\r' | '\t' => self.match_trivium_whitespace(db),
                '\n' => self.match_trivium_newline(db),
                '/' if self.peek_nth(1) == Some('/') => self.match_trivium_single_line_comment(db),
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
    fn match_trivium_whitespace<'a>(&mut self, db: &'a dyn Database) -> TriviumGreen<'a> {
        self.take_while(|s| matches!(s, ' ' | '\r' | '\t'));
        let span = self.consume_text_span();
        let text = span.take(&self.text);
        TokenWhitespace::new_green(db, SmolStrId::from(db, text)).into()
    }

    /// Assumes the next character is '\n'.
    fn match_trivium_newline<'a>(&mut self, db: &'a dyn Database) -> TriviumGreen<'a> {
        self.take();
        let span = self.consume_text_span();
        let text = span.take(&self.text);
        TokenNewline::new_green(db, SmolStrId::from(db, text)).into()
    }

    /// Assumes the next 2 characters are "//".
    fn match_trivium_single_line_comment<'a>(&mut self, db: &'a dyn Database) -> TriviumGreen<'a> {
        match self.peek_nth(2) {
            // `///` is a doc comment, but `////` (4+ slashes) is a regular comment. This matches
            // `cairo-lang-doc`, which discards a doc comment whose content starts with `/`.
            Some('/') if self.peek_nth(3) != Some('/') => {
                self.take_while(|c| c != '\n');
                let span = self.consume_text_span();
                let text = span.take(&self.text);
                TokenSingleLineDocComment::new_green(db, SmolStrId::from(db, text)).into()
            }
            Some('!') => {
                self.take_while(|c| c != '\n');
                let span = self.consume_text_span();
                let text = span.take(&self.text);
                TokenSingleLineInnerComment::new_green(db, SmolStrId::from(db, text)).into()
            }
            _ => {
                self.take_while(|c| c != '\n');
                let span = self.consume_text_span();
                let text = span.take(&self.text);
                TokenSingleLineComment::new_green(db, SmolStrId::from(db, text)).into()
            }
        }
    }

    // Token matchers.
    // =================================================================================

    /// Takes a number. May be decimal, hex, oct or bin.
    fn take_token_literal_number(&mut self) -> LexemeKind {
        let special = if self.peek() == Some('0') {
            self.take();
            match self.peek() {
                Some('x' | 'o' | 'b') => {
                    match self.take() {
                        Some('x') => self.take_while(|c| c.is_ascii_hexdigit()),
                        Some('o') => self.take_while(|c| matches!(c, '0'..='7')),
                        Some('b') => self.take_while(|c| matches!(c, '0'..='1')),
                        _ => unreachable!(),
                    }
                    true
                }
                _ => false,
            }
        } else {
            false
        };
        // Not a special case - so just reading the rest of the digits.
        if !special {
            self.take_while(|c| c.is_ascii_digit());
        }

        // Parse _type suffix.
        if self.peek() == Some('_') {
            self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        }
        LexemeKind::LiteralNumber
    }

    /// Takes a short string.
    fn take_token_short_string(&mut self) -> LexemeKind {
        self.take_token_string_helper('\'');

        // Parse _type suffix.
        if self.peek() == Some('_') {
            self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        }
        LexemeKind::ShortString
    }

    /// Takes a string.
    fn take_token_string(&mut self) -> LexemeKind {
        self.take_token_string_helper('"');
        LexemeKind::String
    }

    fn take_token_string_helper(&mut self, delimiter: char) {
        self.take();
        let mut escaped = false;
        while let Some(token) = self.peek() {
            self.take();
            match token {
                _ if escaped => escaped = false,
                '\\' => escaped = true,
                _ if token == delimiter => {
                    break;
                }
                _ => {}
            };
        }
    }

    /// Assumes the next character is [a-zA-Z_].
    fn take_token_identifier(&mut self) -> LexemeKind {
        // TODO(spapini): Support or explicitly report general unicode characters.
        self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        let span = self.peek_text_span();
        match span.take(&self.text) {
            "as" => LexemeKind::As,
            "const" => LexemeKind::Const,
            "false" => LexemeKind::False,
            "true" => LexemeKind::True,
            "extern" => LexemeKind::Extern,
            "type" => LexemeKind::Type,
            "fn" => LexemeKind::Function,
            "trait" => LexemeKind::Trait,
            "impl" => LexemeKind::Impl,
            "of" => LexemeKind::Of,
            "mod" => LexemeKind::Module,
            "struct" => LexemeKind::Struct,
            "enum" => LexemeKind::Enum,
            "let" => LexemeKind::Let,
            "return" => LexemeKind::Return,
            "match" => LexemeKind::Match,
            "macro" => LexemeKind::Macro,
            "if" => LexemeKind::If,
            "loop" => LexemeKind::Loop,
            "continue" => LexemeKind::Continue,
            "break" => LexemeKind::Break,
            "else" => LexemeKind::Else,
            "while" => LexemeKind::While,
            "use" => LexemeKind::Use,
            "implicits" => LexemeKind::Implicits,
            "ref" => LexemeKind::Ref,
            "mut" => LexemeKind::Mut,
            "for" => LexemeKind::For,
            "nopanic" => LexemeKind::NoPanic,
            "pub" => LexemeKind::Pub,
            "_" => LexemeKind::Underscore,
            _ => LexemeKind::Identifier,
        }
    }

    /// Takes a single character and returns the given kind.
    fn take_token_of_kind(&mut self, kind: LexemeKind) -> LexemeKind {
        self.take();
        kind
    }

    /// If the next character is `second_char`, returns `long_kind`, otherwise returns `short_kind`.
    fn pick_kind(
        &mut self,
        second_char: char,
        long_kind: LexemeKind,
        short_kind: LexemeKind,
    ) -> LexemeKind {
        self.take();
        if self.peek() == Some(second_char) {
            self.take();
            long_kind
        } else {
            short_kind
        }
    }

    pub fn match_terminal<'a>(&mut self, db: &'a dyn Database) -> LexerTerminal<'a> {
        let leading_trivia = self.match_trivia(db, true);

        let kind = if let Some(current) = self.peek() {
            match current {
                '0'..='9' => self.take_token_literal_number(),
                '\'' => self.take_token_short_string(),
                '"' => self.take_token_string(),
                ',' => self.take_token_of_kind(LexemeKind::Comma),
                ';' => self.take_token_of_kind(LexemeKind::Semicolon),
                '?' => self.take_token_of_kind(LexemeKind::QuestionMark),
                '{' => self.take_token_of_kind(LexemeKind::LBrace),
                '}' => self.take_token_of_kind(LexemeKind::RBrace),
                '[' => self.take_token_of_kind(LexemeKind::LBrack),
                ']' => self.take_token_of_kind(LexemeKind::RBrack),
                '(' => self.take_token_of_kind(LexemeKind::LParen),
                ')' => self.take_token_of_kind(LexemeKind::RParen),
                '.' => {
                    self.take();
                    match self.peek() {
                        Some('.') => self.pick_kind('=', LexemeKind::DotDotEq, LexemeKind::DotDot),
                        _ => LexemeKind::Dot,
                    }
                }
                '*' => self.pick_kind('=', LexemeKind::MulEq, LexemeKind::Mul),
                '/' => self.pick_kind('=', LexemeKind::DivEq, LexemeKind::Div),
                '%' => self.pick_kind('=', LexemeKind::ModEq, LexemeKind::Mod),
                '+' => self.pick_kind('=', LexemeKind::PlusEq, LexemeKind::Plus),
                '#' => self.take_token_of_kind(LexemeKind::Hash),
                '$' => self.take_token_of_kind(LexemeKind::Dollar),
                '-' => {
                    self.take();
                    match self.peek() {
                        Some('>') => self.take_token_of_kind(LexemeKind::Arrow),
                        Some('=') => self.take_token_of_kind(LexemeKind::MinusEq),
                        _ => LexemeKind::Minus,
                    }
                }
                '<' => self.pick_kind('=', LexemeKind::LE, LexemeKind::LT),
                '>' => self.pick_kind('=', LexemeKind::GE, LexemeKind::GT),
                'a'..='z' | 'A'..='Z' | '_' => self.take_token_identifier(),
                ':' => self.pick_kind(':', LexemeKind::ColonColon, LexemeKind::Colon),
                '!' => self.pick_kind('=', LexemeKind::Neq, LexemeKind::Not),
                '~' => self.take_token_of_kind(LexemeKind::BitNot),
                '=' => {
                    self.take();
                    match self.peek() {
                        Some('=') => self.take_token_of_kind(LexemeKind::EqEq),
                        Some('>') => self.take_token_of_kind(LexemeKind::MatchArrow),
                        _ => LexemeKind::Eq,
                    }
                }
                '&' => self.pick_kind('&', LexemeKind::AndAnd, LexemeKind::And),
                '|' => self.pick_kind('|', LexemeKind::OrOr, LexemeKind::Or),
                '^' => self.take_token_of_kind(LexemeKind::Xor),
                '@' => self.take_token_of_kind(LexemeKind::At),
                _ => self.take_token_of_kind(LexemeKind::BadCharacters),
            }
        } else {
            LexemeKind::EndOfFile
        };

        let span = self.consume_text_span();
        let text = SmolStrId::from(db, span.take(&self.text));
        let trailing_trivia = self.match_trivia(db, false);
        LexerTerminal { text, kind, leading_trivia, trailing_trivia }
    }
}

/// Output terminal emitted by the lexer.
#[derive(Clone, PartialEq, Eq, Debug, salsa::Update)]
pub struct LexerTerminal<'a> {
    pub text: SmolStrId<'a>,
    /// The kind of the inner token of this terminal.
    pub kind: LexemeKind,
    pub leading_trivia: Vec<TriviumGreen<'a>>,
    pub trailing_trivia: Vec<TriviumGreen<'a>>,
}
impl<'a> LexerTerminal<'a> {
    pub fn width(&self, db: &dyn Database) -> TextWidth {
        self.leading_trivia.iter().map(|t| t.0.width(db)).sum::<TextWidth>()
            + TextWidth::from_str(self.text.long(db))
            + self.trailing_trivia.iter().map(|t| t.0.width(db)).sum::<TextWidth>()
    }

    pub fn text(&self, db: &'a dyn Database) -> &'a str {
        self.text.long(db)
    }
}
