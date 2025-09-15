#[cfg(test)]
#[path = "lexer_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_filesystem::ids::{SmolStrId, Tracked};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::Token;
use cairo_lang_syntax::node::ast::{
    TokenNewline, TokenSingleLineComment, TokenSingleLineDocComment, TokenSingleLineInnerComment,
    TokenWhitespace, TriviumGreen,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_utils::deque::Deque;
use salsa::Database;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Lexer {
    text: Arc<str>,
    previous_position: TextOffset,
    current_position: TextOffset,
}

impl Lexer {
    pub fn position(&self) -> TextOffset {
        self.current_position
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

    /// Assumes the next character '\n'.
    fn match_trivium_newline<'a>(&mut self, db: &'a dyn Database) -> TriviumGreen<'a> {
        self.take();
        let span = self.consume_text_span();
        let text = span.take(&self.text);
        TokenNewline::new_green(db, SmolStrId::from(db, text)).into()
    }

    /// Assumes the next 2 characters are "//".
    fn match_trivium_single_line_comment<'a>(&mut self, db: &'a dyn Database) -> TriviumGreen<'a> {
        match self.peek_nth(2) {
            Some('/') => {
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
    fn take_token_literal_number(&mut self) -> TokenKind {
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
        TokenKind::LiteralNumber
    }

    /// Takes a short string.
    fn take_token_short_string(&mut self) -> TokenKind {
        self.take_token_string_helper('\'');

        // Parse _type suffix.
        if self.peek() == Some('_') {
            self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        }
        TokenKind::ShortString
    }

    /// Takes a string.
    fn take_token_string(&mut self) -> TokenKind {
        self.take_token_string_helper('"');
        TokenKind::String
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
    fn take_token_identifier(&mut self) -> TokenKind {
        // TODO(spapini): Support or explicitly report general unicode characters.
        self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        let span = self.peek_text_span();
        match span.take(&self.text) {
            "as" => TokenKind::As,
            "const" => TokenKind::Const,
            "false" => TokenKind::False,
            "true" => TokenKind::True,
            "extern" => TokenKind::Extern,
            "type" => TokenKind::Type,
            "fn" => TokenKind::Function,
            "trait" => TokenKind::Trait,
            "impl" => TokenKind::Impl,
            "of" => TokenKind::Of,
            "mod" => TokenKind::Module,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            "match" => TokenKind::Match,
            "macro" => TokenKind::Macro,
            "if" => TokenKind::If,
            "loop" => TokenKind::Loop,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "use" => TokenKind::Use,
            "implicits" => TokenKind::Implicits,
            "ref" => TokenKind::Ref,
            "mut" => TokenKind::Mut,
            "for" => TokenKind::For,
            "nopanic" => TokenKind::NoPanic,
            "pub" => TokenKind::Pub,
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

    fn match_terminal<'a>(&mut self, db: &'a dyn Database) -> LexerTerminal<'a> {
        let leading_trivia = self.match_trivia(db, true);

        let kind = if let Some(current) = self.peek() {
            match current {
                '0'..='9' => self.take_token_literal_number(),
                '\'' => self.take_token_short_string(),
                '"' => self.take_token_string(),
                ',' => self.take_token_of_kind(TokenKind::Comma),
                ';' => self.take_token_of_kind(TokenKind::Semicolon),
                '?' => self.take_token_of_kind(TokenKind::QuestionMark),
                '{' => self.take_token_of_kind(TokenKind::LBrace),
                '}' => self.take_token_of_kind(TokenKind::RBrace),
                '[' => self.take_token_of_kind(TokenKind::LBrack),
                ']' => self.take_token_of_kind(TokenKind::RBrack),
                '(' => self.take_token_of_kind(TokenKind::LParen),
                ')' => self.take_token_of_kind(TokenKind::RParen),
                '.' => {
                    self.take();
                    match self.peek() {
                        Some('.') => self.pick_kind('=', TokenKind::DotDotEq, TokenKind::DotDot),
                        _ => TokenKind::Dot,
                    }
                }
                '*' => self.pick_kind('=', TokenKind::MulEq, TokenKind::Mul),
                '/' => self.pick_kind('=', TokenKind::DivEq, TokenKind::Div),
                '%' => self.pick_kind('=', TokenKind::ModEq, TokenKind::Mod),
                '+' => self.pick_kind('=', TokenKind::PlusEq, TokenKind::Plus),
                '#' => self.take_token_of_kind(TokenKind::Hash),
                '$' => self.take_token_of_kind(TokenKind::Dollar),
                '-' => {
                    self.take();
                    match self.peek() {
                        Some('>') => self.take_token_of_kind(TokenKind::Arrow),
                        Some('=') => self.take_token_of_kind(TokenKind::MinusEq),
                        _ => TokenKind::Minus,
                    }
                }
                '<' => self.pick_kind('=', TokenKind::LE, TokenKind::LT),
                '>' => self.pick_kind('=', TokenKind::GE, TokenKind::GT),
                'a'..='z' | 'A'..='Z' | '_' => self.take_token_identifier(),
                ':' => self.pick_kind(':', TokenKind::ColonColon, TokenKind::Colon),
                '!' => self.pick_kind('=', TokenKind::Neq, TokenKind::Not),
                '~' => self.take_token_of_kind(TokenKind::BitNot),
                '=' => {
                    self.take();
                    match self.peek() {
                        Some('=') => self.take_token_of_kind(TokenKind::EqEq),
                        Some('>') => self.take_token_of_kind(TokenKind::MatchArrow),
                        _ => TokenKind::Eq,
                    }
                }
                '&' => self.pick_kind('&', TokenKind::AndAnd, TokenKind::And),
                '|' => self.pick_kind('|', TokenKind::OrOr, TokenKind::Or),
                '^' => self.take_token_of_kind(TokenKind::Xor),
                '@' => self.take_token_of_kind(TokenKind::At),
                _ => self.take_token_of_kind(TokenKind::BadCharacters),
            }
        } else {
            TokenKind::EndOfFile
        };

        let span = self.consume_text_span();
        let text_arc = self.text.clone();
        let text = span.take(&text_arc);
        let trailing_trivia = self.match_trivia(db, false);
        let terminal_kind = token_kind_to_terminal_syntax_kind(kind);

        // TODO(yuval): log(verbose) "consumed text: ..."
        LexerTerminal {
            text: SmolStrId::from(db, text),
            kind: terminal_kind,
            leading_trivia,
            trailing_trivia,
        }
    }
}

/// Tokenizes the entire text and returns a deque of terminals.
#[salsa::tracked]
pub fn tokenize_all<'a>(
    db: &'a dyn Database,
    _tracked: Tracked,
    text: Arc<str>,
) -> cairo_lang_utils::deque::Deque<LexerTerminal<'a>> {
    let mut lexer =
        Lexer { text, previous_position: TextOffset::START, current_position: TextOffset::START };
    let mut result: Deque<LexerTerminal<'a>> = Default::default();
    loop {
        let terminal = lexer.match_terminal(db);
        let is_eof = terminal.kind == SyntaxKind::TerminalEndOfFile;
        result.push_back(terminal);
        if is_eof {
            break;
        }
    }
    result
}

/// Output terminal emitted by the lexer.
#[derive(Clone, PartialEq, Eq, Debug, salsa::Update)]
pub struct LexerTerminal<'a> {
    pub text: SmolStrId<'a>,
    /// The kind of the inner token of this terminal.
    pub kind: SyntaxKind,
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

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash)]
enum TokenKind {
    Identifier,

    // Literals.
    LiteralNumber,
    ShortString,
    String,

    // Keywords.
    As,
    Const,
    False,
    True,
    Extern,
    Type,
    Function,
    Trait,
    Impl,
    Of,
    Module,
    Struct,
    Enum,
    Let,
    Return,
    Match,
    Macro,
    If,
    While,
    For,
    Loop,
    Continue,
    Break,
    Else,
    Use,
    Implicits,
    NoPanic,
    Pub,

    // Modifiers.
    Ref,
    Mut,

    // Punctuation.
    And,
    AndAnd,
    At,
    Or,
    OrOr,
    Xor,
    EqEq,
    Neq,
    GE,
    GT,
    LE,
    LT,
    Not,
    BitNot,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    Mod,
    ModEq,

    Colon,
    ColonColon,
    Comma,
    Dollar,
    Dot,
    DotDot,
    DotDotEq,
    Eq,
    Hash,
    Semicolon,
    QuestionMark,
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

fn token_kind_to_terminal_syntax_kind(kind: TokenKind) -> SyntaxKind {
    match kind {
        TokenKind::As => SyntaxKind::TerminalAs,
        TokenKind::Const => SyntaxKind::TerminalConst,
        TokenKind::Identifier => SyntaxKind::TerminalIdentifier,
        TokenKind::LiteralNumber => SyntaxKind::TerminalLiteralNumber,
        TokenKind::ShortString => SyntaxKind::TerminalShortString,
        TokenKind::String => SyntaxKind::TerminalString,
        TokenKind::False => SyntaxKind::TerminalFalse,
        TokenKind::True => SyntaxKind::TerminalTrue,
        TokenKind::Extern => SyntaxKind::TerminalExtern,
        TokenKind::Type => SyntaxKind::TerminalType,
        TokenKind::Function => SyntaxKind::TerminalFunction,
        TokenKind::Trait => SyntaxKind::TerminalTrait,
        TokenKind::Impl => SyntaxKind::TerminalImpl,
        TokenKind::Of => SyntaxKind::TerminalOf,
        TokenKind::Module => SyntaxKind::TerminalModule,
        TokenKind::Struct => SyntaxKind::TerminalStruct,
        TokenKind::Enum => SyntaxKind::TerminalEnum,
        TokenKind::Let => SyntaxKind::TerminalLet,
        TokenKind::Return => SyntaxKind::TerminalReturn,
        TokenKind::Match => SyntaxKind::TerminalMatch,
        TokenKind::If => SyntaxKind::TerminalIf,
        TokenKind::While => SyntaxKind::TerminalWhile,
        TokenKind::For => SyntaxKind::TerminalFor,
        TokenKind::Loop => SyntaxKind::TerminalLoop,
        TokenKind::Continue => SyntaxKind::TerminalContinue,
        TokenKind::Break => SyntaxKind::TerminalBreak,
        TokenKind::Else => SyntaxKind::TerminalElse,
        TokenKind::Use => SyntaxKind::TerminalUse,
        TokenKind::Implicits => SyntaxKind::TerminalImplicits,
        TokenKind::NoPanic => SyntaxKind::TerminalNoPanic,
        TokenKind::Pub => SyntaxKind::TerminalPub,
        TokenKind::Macro => SyntaxKind::TerminalMacro,
        TokenKind::And => SyntaxKind::TerminalAnd,
        TokenKind::AndAnd => SyntaxKind::TerminalAndAnd,
        TokenKind::At => SyntaxKind::TerminalAt,
        TokenKind::Or => SyntaxKind::TerminalOr,
        TokenKind::OrOr => SyntaxKind::TerminalOrOr,
        TokenKind::Xor => SyntaxKind::TerminalXor,
        TokenKind::EqEq => SyntaxKind::TerminalEqEq,
        TokenKind::Neq => SyntaxKind::TerminalNeq,
        TokenKind::GE => SyntaxKind::TerminalGE,
        TokenKind::GT => SyntaxKind::TerminalGT,
        TokenKind::LE => SyntaxKind::TerminalLE,
        TokenKind::LT => SyntaxKind::TerminalLT,
        TokenKind::Not => SyntaxKind::TerminalNot,
        TokenKind::BitNot => SyntaxKind::TerminalBitNot,
        TokenKind::Plus => SyntaxKind::TerminalPlus,
        TokenKind::PlusEq => SyntaxKind::TerminalPlusEq,
        TokenKind::Minus => SyntaxKind::TerminalMinus,
        TokenKind::MinusEq => SyntaxKind::TerminalMinusEq,
        TokenKind::Mul => SyntaxKind::TerminalMul,
        TokenKind::MulEq => SyntaxKind::TerminalMulEq,
        TokenKind::Div => SyntaxKind::TerminalDiv,
        TokenKind::DivEq => SyntaxKind::TerminalDivEq,
        TokenKind::Mod => SyntaxKind::TerminalMod,
        TokenKind::ModEq => SyntaxKind::TerminalModEq,
        TokenKind::Colon => SyntaxKind::TerminalColon,
        TokenKind::ColonColon => SyntaxKind::TerminalColonColon,
        TokenKind::Comma => SyntaxKind::TerminalComma,
        TokenKind::Dollar => SyntaxKind::TerminalDollar,
        TokenKind::Dot => SyntaxKind::TerminalDot,
        TokenKind::DotDot => SyntaxKind::TerminalDotDot,
        TokenKind::DotDotEq => SyntaxKind::TerminalDotDotEq,
        TokenKind::Eq => SyntaxKind::TerminalEq,
        TokenKind::Hash => SyntaxKind::TerminalHash,
        TokenKind::Semicolon => SyntaxKind::TerminalSemicolon,
        TokenKind::QuestionMark => SyntaxKind::TerminalQuestionMark,
        TokenKind::Underscore => SyntaxKind::TerminalUnderscore,
        TokenKind::LBrace => SyntaxKind::TerminalLBrace,
        TokenKind::RBrace => SyntaxKind::TerminalRBrace,
        TokenKind::LBrack => SyntaxKind::TerminalLBrack,
        TokenKind::RBrack => SyntaxKind::TerminalRBrack,
        TokenKind::LParen => SyntaxKind::TerminalLParen,
        TokenKind::RParen => SyntaxKind::TerminalRParen,
        TokenKind::Ref => SyntaxKind::TerminalRef,
        TokenKind::Mut => SyntaxKind::TerminalMut,
        TokenKind::Arrow => SyntaxKind::TerminalArrow,
        TokenKind::MatchArrow => SyntaxKind::TerminalMatchArrow,
        TokenKind::BadCharacters => SyntaxKind::TerminalBadCharacters,
        TokenKind::EndOfFile => SyntaxKind::TerminalEndOfFile,
    }
}
