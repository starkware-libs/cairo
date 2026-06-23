use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::green::GreenNodeDetails;
use cairo_lang_syntax::node::kind::{LexemeKind, MissingKind, SyntaxKind, TriviaKind};
use colored::{ColoredString, Colorize};
use salsa::Database;

struct ColoredPrinter<'a> {
    db: &'a dyn Database,
    /// Whether to also print empty and missing tokens/nodes.
    verbose: bool,
    result: String,
}
impl ColoredPrinter<'_> {
    fn print(&mut self, syntax_node: &SyntaxNode<'_>) {
        let node = syntax_node.green_node(self.db);
        match &node.details {
            GreenNodeDetails::Token(text) => {
                if self.verbose && node.kind == SyntaxKind::Missing(MissingKind::Token) {
                    self.result.push_str(&format!("{}", "<m>".red()));
                } else {
                    self.result.push_str(&set_color(text.long(self.db), node.kind).to_string());
                }
            }
            GreenNodeDetails::Node { .. } => {
                if self.verbose && node.kind.is_missing() {
                    self.result.push_str(&format!("{}", "<m>".red()));
                } else if self.verbose && is_empty_kind(node.kind) {
                    self.result.push_str(&format!("{}", "<e>".red()));
                } else {
                    for child in syntax_node.get_children(self.db).iter() {
                        self.print(child);
                    }
                }
            }
        }
    }
}

// TODO(yuval): Move to SyntaxKind.
pub fn is_empty_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::OptionStructArgExprEmpty
            | SyntaxKind::OptionTypeClauseEmpty
            | SyntaxKind::OptionReturnTypeClauseEmpty
            | SyntaxKind::OptionTerminalSemicolonEmpty
            | SyntaxKind::OptionTerminalColonColonEmpty
            | SyntaxKind::OptionWrappedGenericParamListEmpty
    )
}

fn set_color(text: &str, kind: SyntaxKind) -> ColoredString {
    match kind {
        SyntaxKind::Token(lexeme) => set_lexeme_color(text, lexeme),
        SyntaxKind::TriviaToken(TriviaKind::Skipped) => text.on_red(), // red background
        SyntaxKind::TriviaToken(_) | SyntaxKind::Missing(MissingKind::Token) => text.clear(),
        _ => unreachable!("Only tokens get `set_color`, got {kind:?}."),
    }
}

/// Colors the token backing a terminal, exhaustively over `LexemeKind` so a newly-added lexeme
/// forces a coloring decision here.
fn set_lexeme_color(text: &str, lexeme: LexemeKind) -> ColoredString {
    // TODO(yuval): drive coloring from tags on `LexemeKind`.
    match lexeme {
        LexemeKind::Identifier => text.truecolor(255, 255, 100), // Yellow
        LexemeKind::Plus
        | LexemeKind::Minus
        | LexemeKind::Mul
        | LexemeKind::Div
        | LexemeKind::Mod
        | LexemeKind::Dot
        | LexemeKind::At
        | LexemeKind::BitNot => text.bright_magenta(),
        LexemeKind::LiteralNumber
        | LexemeKind::False
        | LexemeKind::True
        | LexemeKind::ShortString
        | LexemeKind::String => text.bright_cyan(),
        LexemeKind::Extern
        | LexemeKind::Type
        | LexemeKind::Function
        | LexemeKind::Module
        | LexemeKind::Enum
        | LexemeKind::Struct
        | LexemeKind::Trait
        | LexemeKind::Impl
        | LexemeKind::Const
        | LexemeKind::Macro
        | LexemeKind::Pub => text.bright_blue(),
        LexemeKind::Of
        | LexemeKind::Let
        | LexemeKind::Return
        | LexemeKind::Match
        | LexemeKind::If
        | LexemeKind::Else
        | LexemeKind::Use
        | LexemeKind::Implicits
        | LexemeKind::Ref
        | LexemeKind::Mut
        | LexemeKind::NoPanic
        | LexemeKind::As
        | LexemeKind::While
        | LexemeKind::For
        | LexemeKind::Loop
        | LexemeKind::Continue
        | LexemeKind::Break => text.bright_blue(),
        LexemeKind::Arrow
        | LexemeKind::MatchArrow
        | LexemeKind::Colon
        | LexemeKind::ColonColon
        | LexemeKind::DotDot
        | LexemeKind::DotDotEq
        | LexemeKind::Semicolon
        | LexemeKind::And
        | LexemeKind::AndAnd
        | LexemeKind::Or
        | LexemeKind::OrOr
        | LexemeKind::Xor
        | LexemeKind::Not
        | LexemeKind::QuestionMark
        | LexemeKind::Underscore
        | LexemeKind::Hash
        | LexemeKind::Dollar => text.truecolor(255, 180, 255), // Pink
        LexemeKind::Eq
        | LexemeKind::EqEq
        | LexemeKind::GE
        | LexemeKind::GT
        | LexemeKind::LE
        | LexemeKind::LT
        | LexemeKind::Neq
        | LexemeKind::PlusEq
        | LexemeKind::MinusEq
        | LexemeKind::MulEq
        | LexemeKind::DivEq
        | LexemeKind::ModEq => text.truecolor(255, 165, 0), // Orange
        LexemeKind::LBrace
        | LexemeKind::RBrace
        | LexemeKind::LBrack
        | LexemeKind::RBrack
        | LexemeKind::LParen
        | LexemeKind::RParen
        | LexemeKind::Comma => text.clear(),
        LexemeKind::EndOfFile => text.clear(),
        LexemeKind::BadCharacters => text.red(),
        LexemeKind::Empty => text.clear(),
    }
}

pub fn print_colored(db: &dyn Database, syntax_root: &SyntaxNode<'_>, verbose: bool) -> String {
    let mut printer = ColoredPrinter { db, verbose, result: Default::default() };
    printer.print(syntax_root);
    printer.result
}
