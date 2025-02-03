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
    fn operator_to_string(&self, kind: SyntaxKind) -> String {
        match kind {
            SyntaxKind::TerminalLT => "<".to_string(),
            SyntaxKind::TerminalGT => ">".to_string(),
            SyntaxKind::TerminalLE => "<=".to_string(),
            SyntaxKind::TerminalGE => ">=".to_string(),
            SyntaxKind::TerminalEqEq => "==".to_string(),
            SyntaxKind::TerminalNeq => "!=".to_string(),
            _ => format!("{:?}", kind),
        }
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
<<<<<<< HEAD
    MissingPattern,
=======
    MissingPatteren,
    MissingMacroRuleParamKind,
>>>>>>> d25faa495 (Restrict macro param kind to a predefined enum. (#7194))
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
                format!("Missing token {kind:?}.")
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
            ParserDiagnosticKind::MissingMacroRuleParamKind => {
                "Missing tokens. Expected a macro rule parameter kind.".to_string()
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
                    "Consecutive comparison operators are not allowed: '{}' followed by '{}'",
                    self.operator_to_string(*first_op),
                    self.operator_to_string(*second_op)
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
