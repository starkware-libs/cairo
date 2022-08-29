use colored::{ColoredString, Colorize};
use smol_str::SmolStr;
use syntax::node::db::GreenInterner;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, SyntaxNodeDetails};
use syntax::token::TokenKind;

struct ColoredPrinter<'a> {
    db: &'a dyn GreenInterner,
    /// Whether to also print empty and missing tokens/nodes
    verbose: bool,
    result: String,
}
impl<'a> ColoredPrinter<'a> {
    fn print(&mut self, syntax_node: &SyntaxNode) {
        match syntax_node.details(self.db) {
            SyntaxNodeDetails::Token(token) => {
                if self.verbose && token.kind == TokenKind::Missing {
                    self.result.push_str(format!("{}", "<m>".red()).as_str());
                } else {
                    let bla = set_color(token.text, token.kind).to_string();
                    self.result.push_str(bla.as_str());
                }
            }
            SyntaxNodeDetails::Syntax(kind) => {
                if self.verbose && is_missing_kind(kind) {
                    self.result.push_str(format!("{}", "<m>".red()).as_str());
                } else if self.verbose && is_empty_kind(kind) {
                    self.result.push_str(format!("{}", "<e>".red()).as_str());
                } else {
                    for child in syntax_node.children(self.db).iter() {
                        self.print(child);
                    }
                }
            }
        }
    }
}

// TODO(yuval): autogenerate both.
fn is_missing_kind(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::ExprMissing | SyntaxKind::StatementMissing)
}
fn is_empty_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::OptionStructArgExprEmpty
            | SyntaxKind::OptionTypeClauseEmpty
            | SyntaxKind::OptionReturnTypeClauseEmpty
            | SyntaxKind::OptionSemicolonEmpty
            | SyntaxKind::OptionGenericArgsEmpty
    )
}

fn set_color(text: SmolStr, kind: TokenKind) -> ColoredString {
    // TODO(yuval): use tags on SyntaxKind
    match kind {
        TokenKind::Identifier => text.truecolor(255, 255, 100), // Yellow
        TokenKind::Plus | TokenKind::Minus | TokenKind::Mul | TokenKind::Div | TokenKind::Dot => {
            text.bright_magenta()
        }
        TokenKind::LiteralNumber | TokenKind::False | TokenKind::True => text.bright_cyan(),
        TokenKind::Function | TokenKind::Module | TokenKind::Struct => text.bright_blue(),
        TokenKind::Let | TokenKind::Return => text.bright_blue(),
        TokenKind::Arrow
        | TokenKind::Colon
        | TokenKind::ColonColon
        | TokenKind::DotDot
        | TokenKind::Semi
        | TokenKind::Underscore
        | TokenKind::And
        | TokenKind::Not => text.truecolor(255, 180, 255), // Pink
        TokenKind::Eq
        | TokenKind::EqEq
        | TokenKind::GE
        | TokenKind::GT
        | TokenKind::LE
        | TokenKind::LT
        | TokenKind::Neq => {
            text.truecolor(255, 165, 0) // Orange
        }
        TokenKind::AndAnd | TokenKind::OrOr => text.truecolor(255, 165, 0), // Orange
        TokenKind::LBrace
        | TokenKind::RBrace
        | TokenKind::LBrack
        | TokenKind::RBrack
        | TokenKind::LParen
        | TokenKind::RParen
        | TokenKind::Comma => text.clear(),
        TokenKind::EndOfFile => text.clear(),
        TokenKind::BadCharacters => text.red(),
        TokenKind::Missing => text.clear(),
        TokenKind::SingleLineComment | TokenKind::Whitespace | TokenKind::Newline => text.clear(),
    }
}

pub fn print_colored(db: &dyn GreenInterner, syntax_root: &SyntaxNode, verbose: bool) -> String {
    let mut printer = ColoredPrinter { db, verbose, result: "".to_string() };
    printer.print(syntax_root);
    printer.result
}
