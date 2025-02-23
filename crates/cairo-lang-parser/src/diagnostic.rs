use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::kind::SyntaxKind;
use smol_str::SmolStr;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ParserDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
}
impl ParserDiagnostic {
    /// Converts a `SyntaxKind` to its corresponding operator string.
    fn kind_to_string(&self, kind: SyntaxKind) -> String {
        format!(
            "'{}'",
            match kind {
                SyntaxKind::TerminalAnd => "&",
                SyntaxKind::TerminalAndAnd => "&&",
                SyntaxKind::TerminalArrow => "->",
                SyntaxKind::TerminalAs => "as",
                SyntaxKind::TerminalAt => "@",
                SyntaxKind::TerminalBitNot => "~",
                SyntaxKind::TerminalBreak => "break",
                SyntaxKind::TerminalColon => ":",
                SyntaxKind::TerminalColonColon => "::",
                SyntaxKind::TerminalComma => ",",
                SyntaxKind::TerminalConst => "const",
                SyntaxKind::TerminalContinue => "continue",
                SyntaxKind::TerminalDiv => "/",
                SyntaxKind::TerminalDivEq => "/=",
                SyntaxKind::TerminalDot => ".",
                SyntaxKind::TerminalDotDot => "..",
                SyntaxKind::TerminalDotDotEq => "..=",
                SyntaxKind::TerminalElse => "else",
                SyntaxKind::TerminalEnum => "enum",
                SyntaxKind::TerminalEq => "=",
                SyntaxKind::TerminalEqEq => "==",
                SyntaxKind::TerminalExtern => "extern",
                SyntaxKind::TerminalFalse => "false",
                SyntaxKind::TerminalFor => "for",
                SyntaxKind::TerminalFunction => "fn",
                SyntaxKind::TerminalGE => ">=",
                SyntaxKind::TerminalGT => ">",
                SyntaxKind::TerminalHash => "#",
                SyntaxKind::TerminalIf => "if",
                SyntaxKind::TerminalImpl => "impl",
                SyntaxKind::TerminalImplicits => "implicits",
                SyntaxKind::TerminalLBrace => "{",
                SyntaxKind::TerminalLBrack => "[",
                SyntaxKind::TerminalLE => "<=",
                SyntaxKind::TerminalLParen => "(",
                SyntaxKind::TerminalLT => "<",
                SyntaxKind::TerminalLet => "let",
                SyntaxKind::TerminalLoop => "loop",
                SyntaxKind::TerminalMatch => "match",
                SyntaxKind::TerminalMatchArrow => "=>",
                SyntaxKind::TerminalMinus => "-",
                SyntaxKind::TerminalMinusEq => "-=",
                SyntaxKind::TerminalMod => "%",
                SyntaxKind::TerminalModEq => "%=",
                SyntaxKind::TerminalModule => "mod",
                SyntaxKind::TerminalMul => "*",
                SyntaxKind::TerminalMulEq => "*=",
                SyntaxKind::TerminalMut => "mut",
                SyntaxKind::TerminalNeq => "!=",
                SyntaxKind::TerminalNoPanic => "nopanic",
                SyntaxKind::TerminalNot => "!",
                SyntaxKind::TerminalOf => "of",
                SyntaxKind::TerminalOr => "|",
                SyntaxKind::TerminalOrOr => "||",
                SyntaxKind::TerminalPlus => "+",
                SyntaxKind::TerminalPlusEq => "+=",
                SyntaxKind::TerminalPub => "pub",
                SyntaxKind::TerminalQuestionMark => "?",
                SyntaxKind::TerminalRBrace => "}",
                SyntaxKind::TerminalRBrack => "]",
                SyntaxKind::TerminalRParen => ")",
                SyntaxKind::TerminalRef => "ref",
                SyntaxKind::TerminalReturn => "return",
                SyntaxKind::TerminalSemicolon => ";",
                SyntaxKind::TerminalStruct => "struct",
                SyntaxKind::TerminalTrait => "trait",
                SyntaxKind::TerminalTrue => "true",
                SyntaxKind::TerminalType => "type",
                SyntaxKind::TerminalUnderscore => "_",
                SyntaxKind::TerminalUse => "use",
                SyntaxKind::TerminalWhile => "while",
                SyntaxKind::TerminalXor => "^",
                _ => return format!("{:?}", kind),
            }
        )
    }
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKind {
    // TODO(spapini): Add tokens from the recovery set to the message.
    SkippedElement { element_name: SmolStr },
    MissingToken(SyntaxKind),
    MissingExpression,
    MissingPathSegment,
    MissingTypeClause,
    MissingTypeExpression,
    MissingWrappedArgList,
    MissingPattern,
    ExpectedInToken,
    ItemInlineMacroWithoutBang { identifier: SmolStr, bracket_type: SyntaxKind },
    ReservedIdentifier { identifier: SmolStr },
    UnderscoreNotAllowedAsIdentifier,
    MissingLiteralSuffix,
    InvalidNumericLiteralValue,
    IllegalStringEscaping,
    ShortStringMustBeAscii,
    StringMustBeAscii,
    UnterminatedShortString,
    UnterminatedString,
    VisibilityWithoutItem,
    AttributesWithoutItem,
    AttributesWithoutTraitItem,
    AttributesWithoutImplItem,
    AttributesWithoutStatement,
    DisallowedTrailingSeparatorOr,
    ConsecutiveMathOperators { first_op: SyntaxKind, second_op: SyntaxKind },
}
impl DiagnosticEntry for ParserDiagnostic {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn FilesGroup) -> String {
        match &self.kind {
            ParserDiagnosticKind::SkippedElement { element_name } => {
                format!("Skipped tokens. Expected: {element_name}.")
            }
            ParserDiagnosticKind::MissingToken(kind) => {
                format!("Missing token {}.", self.kind_to_string(*kind))
            }
            ParserDiagnosticKind::MissingExpression => {
                "Missing tokens. Expected an expression.".to_string()
            }
            ParserDiagnosticKind::MissingPathSegment => {
                "Missing tokens. Expected a path segment.".to_string()
            }
            ParserDiagnosticKind::MissingTypeClause => {
                "Unexpected token, expected ':' followed by a type.".to_string()
            }
            ParserDiagnosticKind::MissingTypeExpression => {
                "Missing tokens. Expected a type expression.".to_string()
            }
            ParserDiagnosticKind::MissingWrappedArgList => "Missing tokens. Expected an argument \
                                                            list wrapped in either parentheses, \
                                                            brackets, or braces."
                .to_string(),
            ParserDiagnosticKind::MissingPattern => {
                "Missing tokens. Expected a pattern.".to_string()
            }
            ParserDiagnosticKind::ExpectedInToken => {
                "Missing identifier token, expected 'in'.".to_string()
            }
            ParserDiagnosticKind::ItemInlineMacroWithoutBang { identifier, bracket_type } => {
                let (left, right) = match bracket_type {
                    SyntaxKind::TerminalLParen => ("(", ")"),
                    SyntaxKind::TerminalLBrack => ("[", "]"),
                    SyntaxKind::TerminalLBrace => ("{", "}"),
                    _ => ("", ""),
                };
                format!(
                    "Expected a '!' after the identifier '{identifier}' to start an inline macro.
Did you mean to write `{identifier}!{left}...{right}'?",
                )
            }
            ParserDiagnosticKind::ReservedIdentifier { identifier } => {
                format!("'{identifier}' is a reserved identifier.")
            }
            ParserDiagnosticKind::UnderscoreNotAllowedAsIdentifier => {
                "An underscore ('_') is not allowed as an identifier in this context.".to_string()
            }
            ParserDiagnosticKind::MissingLiteralSuffix => "Missing literal suffix.".to_string(),
            ParserDiagnosticKind::InvalidNumericLiteralValue => {
                "Literal is not a valid number.".to_string()
            }
            ParserDiagnosticKind::IllegalStringEscaping => "Invalid string escaping.".to_string(),
            ParserDiagnosticKind::ShortStringMustBeAscii => {
                "Short string literals can only include ASCII characters.".into()
            }
            ParserDiagnosticKind::StringMustBeAscii => {
                "String literals can only include ASCII characters.".into()
            }
            ParserDiagnosticKind::UnterminatedShortString => {
                "Unterminated short string literal.".into()
            }
            ParserDiagnosticKind::UnterminatedString => "Unterminated string literal.".into(),
            ParserDiagnosticKind::VisibilityWithoutItem => {
                "Missing tokens. Expected an item after visibility.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutItem => {
                "Missing tokens. Expected an item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutTraitItem => {
                "Missing tokens. Expected a trait item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutImplItem => {
                "Missing tokens. Expected an impl item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutStatement => {
                "Missing tokens. Expected a statement after attributes.".to_string()
            }
            ParserDiagnosticKind::DisallowedTrailingSeparatorOr => {
                "A trailing `|` is not allowed in an or-pattern.".to_string()
            }
            ParserDiagnosticKind::ConsecutiveMathOperators { first_op, second_op } => {
                format!(
                    "Consecutive comparison operators are not allowed: {} followed by {}",
                    self.kind_to_string(*first_op),
                    self.kind_to_string(*second_op)
                )
            }
        }
    }

    fn location(&self, _db: &dyn FilesGroup) -> cairo_lang_diagnostics::DiagnosticLocation {
        cairo_lang_diagnostics::DiagnosticLocation { file_id: self.file_id, span: self.span }
    }

    fn is_same_kind(&self, other: &Self) -> bool {
        other.kind == self.kind
    }
}
