use crate::{
    text::{CodeSource, TextPosition, TextSpan},
    token::{Terminal, Token, TokenKind, Trivium, TriviumKind},
};

pub struct Lexer<'a> {
    source: CodeSource,
    text: &'a str,
    last_position: TextPosition,
    position: TextPosition,
    done: bool,
}

impl Lexer<'_> {
    // Ctors.
    #[allow(dead_code)]
    pub fn from_text(source: CodeSource, text: &str) -> Lexer<'_> {
        Lexer {
            source,
            text,
            last_position: TextPosition::new(),
            position: TextPosition::new(),
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

    fn consume_span(&mut self) -> (String, TextSpan) {
        let span = TextSpan {
            from: self.last_position,
            to: self.position,
        };
        let text = String::from(self.current_span());
        self.last_position = self.position;
        (text, span)
    }

    // Trivia matchers.
    fn match_trivia(&mut self, leading: bool) -> Vec<Trivium> {
        let mut res: Vec<Trivium> = Vec::new();
        while let Some(current) = self.peek() {
            let trivium = match current {
                ' ' | '\r' | '\t' => self.match_trivium_whitespace(),
                '\n' => self.match_trivium_newline(),
                '/' if self.peek_nth(1) == Some('/') => self.match_trivium_single_line_comment(),
                _ => break,
            };
            let kind = trivium.kind;
            res.push(trivium);
            if matches!(kind, TriviumKind::Newline) && !leading {
                break;
            }
        }
        res
    }

    fn match_trivium_whitespace(&mut self) -> Trivium {
        while self
            .peek()
            .map(|s| matches!(s, ' ' | '\t' | '\r'))
            .unwrap_or(false)
        {
            self.take();
        }
        let (text, _span) = self.consume_span();
        Trivium {
            kind: TriviumKind::Whitespace,
            text,
        }
    }

    fn match_trivium_newline(&mut self) -> Trivium {
        self.take();
        let (text, _span) = self.consume_span();
        Trivium {
            kind: TriviumKind::Newline,
            text,
        }
    }

    // Assumes the next 2 characters are "//".
    fn match_trivium_single_line_comment(&mut self) -> Trivium {
        while !matches!(self.take(), Some('\n') | None) {}
        let (text, _span) = self.consume_span();
        Trivium {
            kind: TriviumKind::SingleLineComment,
            text,
        }
    }

    // Token matchers.
    fn match_token_number(&mut self) -> TokenKind {
        while self.peek().map(|s| s.is_ascii_digit()).unwrap_or(false) {
            self.take();
        }
        TokenKind::LiteralNumber
    }

    fn match_token_identifier(&mut self) -> TokenKind {
        while self
            .peek()
            .map(|s| s.is_ascii_alphanumeric() || s == '_')
            .unwrap_or(false)
        {
            self.take();
        }

        let word = self.current_span();
        match word {
            "false" => TokenKind::LiteralFalse,
            "true" => TokenKind::LiteralTrue,
            "fn" => TokenKind::KeywordFunction,
            "mod" => TokenKind::KeywordModule,
            "struct" => TokenKind::KeywordStruct,
            "let" => TokenKind::KeywordLet,
            "return" => TokenKind::KeywordReturn,
            "_" => TokenKind::CharUnderscore,
            _ => TokenKind::Identifier,
        }
    }

    fn match_single(&mut self, kind: TokenKind) -> TokenKind {
        self.take();
        kind
    }

    fn match_terminal(&mut self) -> Terminal {
        let leading_trivia = self.match_trivia(true);

        let kind = if let Some(current) = self.peek() {
            match current {
                '0'..='9' => self.match_token_number(),
                ',' => self.match_single(TokenKind::CharComma),
                ';' => self.match_single(TokenKind::CharSemiColon),
                '{' => self.match_single(TokenKind::CharOpenBrace),
                '}' => self.match_single(TokenKind::CharCloseBrace),
                '[' => self.match_single(TokenKind::CharOpenBracket),
                ']' => self.match_single(TokenKind::CharCloseBracket),
                '(' => self.match_single(TokenKind::CharOpenParenthesis),
                ')' => self.match_single(TokenKind::CharCloseParenthesis),
                '.' => self.match_single(TokenKind::CharPeriod),
                'a'..='z' | 'A'..='Z' | '_' => self.match_token_identifier(),
                '*' | '/' | '+' | '-' | '<' | '>' => self.match_single(TokenKind::Operator),
                ':' => {
                    self.take();
                    if self.peek() == Some(':') {
                        self.take();
                        TokenKind::CharColonColon
                    } else {
                        TokenKind::CharColon
                    }
                }
                '!' => {
                    self.take();
                    if self.peek() == Some('=') {
                        self.take();
                    }
                    TokenKind::Operator
                }
                '=' => {
                    self.take();
                    if self.peek() == Some('=') {
                        self.take();
                        TokenKind::Operator
                    } else {
                        TokenKind::CharEquals
                    }
                }
                '&' => {
                    self.take();
                    if self.peek() == Some('&') {
                        self.take();
                    }
                    TokenKind::Operator
                }
                '|' if self.peek() == Some('|') => {
                    self.take();
                    self.take();
                    TokenKind::Operator
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

        Terminal {
            leading_trivia,
            token: Token { kind, text },
            trailing_trivia,
            width: span.width(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Terminal;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let current = self.match_terminal();
        if current.kind() == TokenKind::EndOfFile {
            self.done = true;
        };
        Some(current)
    }
}
