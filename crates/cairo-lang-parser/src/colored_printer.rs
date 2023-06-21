use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::green::GreenNodeDetails;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::SyntaxNode;
use colored::{ColoredString, Colorize};
use smol_str::SmolStr;

struct ColoredPrinter<'a> {
    db: &'a dyn SyntaxGroup,
    /// Whether to also print empty and missing tokens/nodes
    verbose: bool,
    result: String,
}
impl<'a> ColoredPrinter<'a> {
    fn print(&mut self, syntax_node: &SyntaxNode) {
        let node = syntax_node.green_node(self.db);
        match node.details {
            GreenNodeDetails::Token(text) => {
                if self.verbose && node.kind == SyntaxKind::TokenMissing {
                    self.result.push_str(format!("{}", "<m>".red()).as_str());
                } else {
                    self.result.push_str(set_color(text, node.kind).to_string().as_str());
                }
            }
            GreenNodeDetails::Node { .. } => {
                if self.verbose && is_missing_kind(node.kind) {
                    self.result.push_str(format!("{}", "<m>".red()).as_str());
                } else if self.verbose && is_empty_kind(node.kind) {
                    self.result.push_str(format!("{}", "<e>".red()).as_str());
                } else {
                    for child in syntax_node.children(self.db) {
                        self.print(&child);
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

fn set_color(text: SmolStr, kind: SyntaxKind) -> ColoredString {
    // TODO(yuval): use tags on SyntaxKind
    match kind {
        SyntaxKind::TokenIdentifier => text.truecolor(255, 255, 100), // Yellow
        SyntaxKind::TokenPlus
        | SyntaxKind::TokenMinus
        | SyntaxKind::TokenMul
        | SyntaxKind::TokenDiv
        | SyntaxKind::TokenMod
        | SyntaxKind::TokenDot => text.bright_magenta(),
        SyntaxKind::TokenLiteralNumber
        | SyntaxKind::TokenFalse
        | SyntaxKind::TokenTrue
        | SyntaxKind::TokenShortString
        | SyntaxKind::TokenString => text.bright_cyan(),
        SyntaxKind::TokenExtern
        | SyntaxKind::TokenType
        | SyntaxKind::TokenFunction
        | SyntaxKind::TokenModule
        | SyntaxKind::TokenEnum
        | SyntaxKind::TokenStruct
        | SyntaxKind::TokenTrait
        | SyntaxKind::TokenImpl => text.bright_blue(),
        SyntaxKind::TokenOf
        | SyntaxKind::TokenLet
        | SyntaxKind::TokenReturn
        | SyntaxKind::TokenMatch
        | SyntaxKind::TokenIf
        | SyntaxKind::TokenElse
        | SyntaxKind::TokenUse
        | SyntaxKind::TokenImplicits
        | SyntaxKind::TokenRef
        | SyntaxKind::TokenMut
        | SyntaxKind::TokenNoPanic => text.bright_blue(),
        SyntaxKind::TokenArrow
        | SyntaxKind::TokenMatchArrow
        | SyntaxKind::TokenColon
        | SyntaxKind::TokenColonColon
        | SyntaxKind::TokenDotDot
        | SyntaxKind::TokenSemicolon
        | SyntaxKind::TokenAnd
        | SyntaxKind::TokenAndAnd
        | SyntaxKind::TokenOr
        | SyntaxKind::TokenOrOr
        | SyntaxKind::TokenXor
        | SyntaxKind::TokenNot
        | SyntaxKind::TokenQuestionMark
        | SyntaxKind::TokenUnderscore
        | SyntaxKind::TokenHash => text.truecolor(255, 180, 255), // Pink
        SyntaxKind::TokenEq
        | SyntaxKind::TokenEqEq
        | SyntaxKind::TokenGE
        | SyntaxKind::TokenGT
        | SyntaxKind::TokenLE
        | SyntaxKind::TokenLT
        | SyntaxKind::TokenNeq => {
            text.truecolor(255, 165, 0) // Orange
        }
        SyntaxKind::TokenLBrace
        | SyntaxKind::TokenRBrace
        | SyntaxKind::TokenLBrack
        | SyntaxKind::TokenRBrack
        | SyntaxKind::TokenLParen
        | SyntaxKind::TokenRParen
        | SyntaxKind::TokenComma => text.clear(),
        SyntaxKind::TokenEndOfFile => text.clear(),
        SyntaxKind::TokenBadCharacters => text.red(),
        SyntaxKind::TokenMissing => text.clear(),
        SyntaxKind::TokenSkipped => text.on_red(), // red background
        SyntaxKind::TokenSingleLineComment
        | SyntaxKind::TokenWhitespace
        | SyntaxKind::TokenNewline => text.clear(),
        // TODO(yuval): Can this be made exhaustive?
        _ => panic!("Unexpected syntax kind: {kind:?}"),
    }
}

pub fn print_colored(db: &dyn SyntaxGroup, syntax_root: &SyntaxNode, verbose: bool) -> String {
    let mut printer = ColoredPrinter { db, verbose, result: "".to_string() };
    printer.print(syntax_root);
    printer.result
}
