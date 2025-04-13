use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, MacroDeclarationId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe, skip_diagnostic};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_parser::macro_helpers::as_expr_macro_token_tree;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::{ExprPath, TokenTreeParam};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::resolve::{MACRO_DEF_SITE, Resolver, ResolverData};

/// A unique identifier for a repetition block inside a macro rule.
/// Each `$( ... )` group in the macro pattern gets a new `RepetitionId`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RepetitionId(usize);

/// The captures collected during macro pattern matching.
/// Each macro parameter name maps to a flat list of matched strings.
type Captures = OrderedHashMap<String, Vec<CapturedValue>>;

/// Context used during macro pattern matching and expansion.
/// Tracks captured values, active repetition scopes, and repetition ownership per placeholder.
#[derive(Default, Clone)]
pub struct MatcherContext {
    /// The captured values per macro parameter name.
    /// These are flat lists, even for repeated placeholders.
    pub captures: Captures,

    /// Maps each placeholder to the `RepetitionId` of the repetition block
    /// they are part of. This helps the expansion phase know which iterators to advance together.
    pub placeholder_to_rep_id: OrderedHashMap<String, RepetitionId>,

    /// Stack of currently active repetition blocks. Used to assign placeholders
    /// to their correct `RepetitionId` while recursing into nested repetitions.
    pub current_repetition_stack: Vec<RepetitionId>,

    /// Counter for generating unique `RepetitionId`s.
    pub next_repetition_id: usize,

    /// Tracks the current index for each active repetition during expansion.
    pub repetition_indices: OrderedHashMap<RepetitionId, usize>,

    /// Count how many times each repetition matched.
    pub repetition_match_counts: OrderedHashMap<RepetitionId, usize>,

    /// Store the repetition operator for each repetition.
    pub repetition_operators: OrderedHashMap<RepetitionId, ast::MacroRepetitionOperator>,
}

impl MatcherContext {
    /// Returns true if all captures for the given repetition ID have been exhausted.
    fn is_repetition_done(&self, rep_id: RepetitionId) -> bool {
        if let Some((name, _)) = self.placeholder_to_rep_id.iter().find(|(_, &r)| r == rep_id) {
            let index = *self.repetition_indices.get(&rep_id).unwrap_or(&0);
            if let Some(values) = self.captures.get(name) {
                return index >= values.len();
            }
        }
        true
    }
}

/// The semantic data for a macro declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroDeclarationData {
    rules: Vec<MacroRuleData>,
    attributes: Vec<Attribute>,
    diagnostics: Diagnostics<SemanticDiagnostic>,
    resolver_data: Arc<ResolverData>,
}

/// The semantic data for a single macro rule in a macro declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroRuleData {
    pub pattern: ast::MacroMatcher,
    pub expansion: ast::ExprBlock,
}

/// The possible kinds of placeholders in a macro rule.
#[derive(Debug, Clone, PartialEq, Eq)]
enum PlaceholderKind {
    Identifier,
    Expr,
}

impl From<ast::MacroRuleParamKind> for PlaceholderKind {
    fn from(kind: ast::MacroRuleParamKind) -> Self {
        match kind {
            ast::MacroRuleParamKind::Identifier(_) => PlaceholderKind::Identifier,
            ast::MacroRuleParamKind::Expr(_) => PlaceholderKind::Expr,
            ast::MacroRuleParamKind::Missing(_) => unreachable!(
                "Missing macro rule param kind, should have been handled by the parser."
            ),
        }
    }
}

/// Information about a captured value in a macro.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CapturedValue {
    pub text: String,
    pub stable_ptr: SyntaxStablePtrId,
}

/// Query implementation of [crate::db::SemanticGroup::priv_macro_declaration_data].
pub fn priv_macro_declaration_data(
    db: &dyn SemanticGroup,
    macro_declaration_id: MacroDeclarationId,
) -> Maybe<MacroDeclarationData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();

    let module_file_id = macro_declaration_id.module_file_id(db.upcast());
    let macro_declaration_syntax =
        db.module_macro_declaration_by_id(macro_declaration_id)?.to_maybe()?;
    let attributes = macro_declaration_syntax.attributes(syntax_db).structurize(syntax_db);
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::MacroDeclaration(macro_declaration_id),
    ));
    let resolver = Resolver::new(db, module_file_id, inference_id);

    // TODO(Dean): Verify uniqueness of param names.
    // TODO(Dean): Verify consistency bracket terminals.
    let mut rules = vec![];
    for rule_syntax in macro_declaration_syntax.rules(syntax_db).elements(syntax_db) {
        let pattern = rule_syntax.lhs(db.upcast());
        let expansion = rule_syntax.rhs(db.upcast());

        let pattern_elements = get_pattern_elements(db.upcast(), pattern.clone());

        // Collect defined placeholders from pattern
        let defined_placeholders = OrderedHashSet::<_>::from_iter(
            pattern_elements.elements(syntax_db).into_iter().filter_map(|element| match element {
                ast::MacroRuleElement::Param(param) => {
                    Some(param.name(syntax_db).as_syntax_node().get_text_without_trivia(syntax_db))
                }
                _ => None,
            }),
        );

        // Verify all used placeholders are defined
        for (placeholder_ptr, used_placeholder) in
            collect_expansion_placeholders(syntax_db, expansion.as_syntax_node())
        {
            if !defined_placeholders.contains(&used_placeholder) {
                diagnostics.report(
                    placeholder_ptr,
                    SemanticDiagnosticKind::UndefinedMacroPlaceholder(used_placeholder),
                );
            }
        }

        rules.push(MacroRuleData { pattern, expansion });
    }
    let resolver_data = Arc::new(resolver.data);
    Ok(MacroDeclarationData { diagnostics: diagnostics.build(), attributes, resolver_data, rules })
}

/// Helper function to extract pattern elements from a MacroMatcher
fn get_pattern_elements(
    db: &dyn SyntaxGroup,
    pattern: ast::MacroMatcher,
) -> ast::MacroRuleElements {
    match pattern {
        ast::MacroMatcher::Parenthesized(inner) => inner.elements(db),
        ast::MacroMatcher::Braced(inner) => inner.elements(db),
        ast::MacroMatcher::Bracketed(inner) => inner.elements(db),
    }
}

/// Helper function to extract a placeholder name from an ExprPath node, if it represents a macro
/// placeholder. Returns None if the path is not a valid macro placeholder.
fn extract_placeholder(db: &dyn SyntaxGroup, path_node: &ExprPath) -> Option<String> {
    if let ast::OptionTerminalDollar::TerminalDollar(_) = path_node.dollar(db) {
        let placeholder_name = path_node.identifier(db).to_string();
        if path_node.segments(db).elements(db).len() == 1 && placeholder_name != MACRO_DEF_SITE {
            return Some(placeholder_name);
        }
    }
    None
}

/// Extracts the placeholder name from a TokenTreeParam node, if it's a macro placeholder.
/// Returns None if the token is not a placeholder.
fn extract_placeholder_from_param(db: &dyn SyntaxGroup, param: &TokenTreeParam) -> Option<String> {
    let placeholder_name = param.name(db).as_syntax_node().get_text_without_trivia(db);
    (placeholder_name != MACRO_DEF_SITE).then_some(placeholder_name)
}

/// Helper function to collect all placeholder names used in a macro expansion.
fn collect_expansion_placeholders(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
) -> Vec<(SyntaxStablePtrId, String)> {
    let mut placeholders = Vec::new();

    if node.kind(db) == SyntaxKind::ExprPath {
        let path_node = ExprPath::from_syntax_node(db, node.clone());
        if let Some(placeholder_name) = extract_placeholder(db, &path_node) {
            placeholders.push((path_node.stable_ptr().untyped(), placeholder_name));
            return placeholders;
        }
    }
    if !node.kind(db).is_terminal() {
        for child in db.get_children(node).iter() {
            placeholders.extend(collect_expansion_placeholders(db, child.clone()));
        }
    }
    placeholders
}

/// Given a macro declaration and an input token tree, checks if the input the given rule, and
/// returns the captured params if it does.
pub fn is_macro_rule_match(
    db: &dyn SemanticGroup,
    rule: &MacroRuleData,
    input: &ast::TokenTreeNode,
) -> Option<(Captures, OrderedHashMap<String, RepetitionId>)> {
    let mut ctx = MatcherContext::default();

    let matcher_elements = get_pattern_elements(db.upcast(), rule.pattern.clone());
    let input_elements = match input.subtree(db.upcast()) {
        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db.upcast()),
        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db.upcast()),
        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db.upcast()),
        ast::WrappedTokenTree::Missing(_) => unreachable!(),
    }
    .elements(db.upcast());
    let mut input_iter = input_elements.iter().peekable();
    is_macro_rule_match_ex(db, matcher_elements, &mut input_iter, &mut ctx, true)?;
    if !validate_repetition_operator_constraints(&ctx) {
        return None;
    }
    Some((ctx.captures, ctx.placeholder_to_rep_id))
}

/// Helper function for [expand_macro_rule].
/// Traverses the macro expansion and replaces the placeholders with the provided values,
/// while collecting the result in `res_buffer`.
fn is_macro_rule_match_ex(
    db: &dyn SemanticGroup,
    matcher_elements: ast::MacroRuleElements,
    input_iter: &mut std::iter::Peekable<std::slice::Iter<'_, ast::TokenTree>>,
    ctx: &mut MatcherContext,
    consume_all_input: bool,
) -> Option<()> {
    for matcher_element in matcher_elements.elements(db.upcast()) {
        match matcher_element {
            ast::MacroRuleElement::Token(matcher_token) => {
                let input_token = input_iter.next()?;
                match input_token {
                    ast::TokenTree::Token(token_tree_leaf) => {
                        if matcher_token.as_syntax_node().get_text_without_trivia(db.upcast())
                            != token_tree_leaf.as_syntax_node().get_text_without_trivia(db.upcast())
                        {
                            return None;
                        }
                        continue;
                    }
                    ast::TokenTree::Subtree(_) => return None,
                    ast::TokenTree::Repetition(_) => return None,
                    ast::TokenTree::Param(_) => return None,
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
            ast::MacroRuleElement::Param(param) => {
                let placeholder_kind: PlaceholderKind = param.kind(db.upcast()).into();
                let placeholder_name =
                    param.name(db.upcast()).as_syntax_node().get_text_without_trivia(db.upcast());
                match placeholder_kind {
                    PlaceholderKind::Identifier => {
                        let input_token = input_iter.next()?;
                        let captured_text = match input_token {
                            ast::TokenTree::Token(token_tree_leaf) => {
                                match token_tree_leaf.leaf(db.upcast()) {
                                    ast::TokenNode::TerminalIdentifier(terminal_identifier) => {
                                        terminal_identifier.text(db.upcast()).to_string()
                                    }
                                    _ => return None,
                                }
                            }
                            _ => return None,
                        };
                        ctx.captures.entry(placeholder_name.clone()).or_default().push(
                            CapturedValue {
                                text: captured_text,
                                stable_ptr: input_token.stable_ptr().untyped(),
                            },
                        );
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name.clone(), *rep_id);
                        }
                        continue;
                    }
                    PlaceholderKind::Expr => {
                        let mut cloned_iter = input_iter.clone();
                        let peek_token = cloned_iter.peek()?;
                        let file_id = peek_token.as_syntax_node().stable_ptr().file_id(db.upcast());
                        let expr_node = as_expr_macro_token_tree(
                            input_iter.clone().cloned(),
                            file_id,
                            db.upcast(),
                        )?;
                        let expr_text = expr_node.as_syntax_node().get_text(db.upcast());
                        let expr_length = expr_text.len();
                        // An empty expression is parsed successfully. However we don't want to
                        // capture it a valid expr.
                        if expr_length == 0 {
                            return None;
                        }

                        ctx.captures.entry(placeholder_name.clone()).or_default().push(
                            CapturedValue {
                                text: expr_text.to_string(),
                                stable_ptr: peek_token.stable_ptr().untyped(),
                            },
                        );
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name.clone(), *rep_id);
                        }
                        let expr_length = expr_text.len();
                        let mut current_length = 0;

                        // TODO(Dean): Use the iterator directly in the parser and advance it while
                        // parsing the expression, instead of manually tracking the length and
                        // iterating separately.
                        for token_tree_leaf in input_iter.by_ref() {
                            let token_text = match token_tree_leaf {
                                ast::TokenTree::Token(token_tree_leaf) => {
                                    token_tree_leaf.as_syntax_node().get_text(db.upcast())
                                }
                                ast::TokenTree::Subtree(token_subtree) => {
                                    token_subtree.as_syntax_node().get_text(db.upcast())
                                }
                                ast::TokenTree::Repetition(token_repetition) => {
                                    token_repetition.as_syntax_node().get_text(db.upcast())
                                }
                                ast::TokenTree::Param(token_param) => {
                                    token_param.as_syntax_node().get_text(db.upcast())
                                }
                                ast::TokenTree::Missing(_) => unreachable!(),
                            };
                            current_length += token_text.len();
                            if current_length >= expr_length {
                                break;
                            }
                        }
                        continue;
                    }
                }
            }
            ast::MacroRuleElement::Subtree(matcher_subtree) => {
                let input_token = input_iter.next()?;
                if let ast::TokenTree::Subtree(input_subtree) = input_token {
                    let inner_elements =
                        get_pattern_elements(db.upcast(), matcher_subtree.subtree(db.upcast()));
                    let inner_input_elements = match input_subtree.subtree(db.upcast()) {
                        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db.upcast()),
                        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db.upcast()),
                        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db.upcast()),
                        ast::WrappedTokenTree::Missing(_) => unreachable!(),
                    }
                    .elements(db.upcast());
                    let mut inner_input_iter = inner_input_elements.iter().peekable();
                    is_macro_rule_match_ex(db, inner_elements, &mut inner_input_iter, ctx, true)?;
                    continue;
                } else {
                    return None;
                }
            }
            ast::MacroRuleElement::Repetition(repetition) => {
                let rep_id = RepetitionId(ctx.next_repetition_id);
                ctx.next_repetition_id += 1;
                ctx.current_repetition_stack.push(rep_id);
                let elements = repetition.elements(db.upcast());
                let operator = repetition.operator(db.upcast());
                let separator_token = repetition.separator(db.upcast());
                let expected_separator = match separator_token {
                    ast::OptionTerminalComma::TerminalComma(sep) => {
                        Some(sep.as_syntax_node().get_text_without_trivia(db.upcast()))
                    }
                    ast::OptionTerminalComma::Empty(_) => None,
                };
                let mut match_count = 0;
                loop {
                    let mut inner_ctx = ctx.clone();
                    let mut temp_iter = input_iter.clone();
                    if is_macro_rule_match_ex(
                        db,
                        elements.clone(),
                        &mut temp_iter,
                        &mut inner_ctx,
                        false,
                    )
                    .is_none()
                    {
                        break;
                    }
                    *ctx = inner_ctx;
                    *input_iter = temp_iter;
                    match_count += 1;
                    if let Some(expected_sep) = &expected_separator {
                        if let Some(ast::TokenTree::Token(token_leaf)) = input_iter.peek() {
                            let actual =
                                token_leaf.as_syntax_node().get_text_without_trivia(db.upcast());
                            if actual == *expected_sep {
                                input_iter.next();
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                ctx.repetition_match_counts.insert(rep_id, match_count);
                ctx.repetition_operators.insert(rep_id, operator.clone());
                ctx.current_repetition_stack.pop();
                continue;
            }
        }
    }

    if consume_all_input && input_iter.next().is_some() {
        return None;
    }
    Some(())
}

fn validate_repetition_operator_constraints(ctx: &MatcherContext) -> bool {
    for (rep_id, count) in ctx.repetition_match_counts.clone() {
        match ctx.repetition_operators.get(&rep_id) {
            Some(ast::MacroRepetitionOperator::ZeroOrOne(_)) if count > 1 => return false,
            Some(ast::MacroRepetitionOperator::OneOrMore(_)) if count < 1 => return false,
            Some(ast::MacroRepetitionOperator::ZeroOrMore(_)) | None => {}
            _ => {}
        }
    }
    true
}

/// The result of expanding a macro rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroExpansionResult {
    /// The expanded text.
    pub text: String,
    /// Information about placeholder expansions in this macro expansion.
    /// Must be kept ordered by span.start to enable binary search in get_placeholder_at.
    pub code_mappings: Vec<CodeMapping>,
}

impl MacroExpansionResult {
    /// Returns the placeholder that was expanded at the given offset, if any.
    pub fn get_placeholder_at(&self, offset: TextOffset) -> Option<&CodeMapping> {
        // Binary search to find a code mapping containing the offset
        match self.code_mappings.binary_search_by_key(&offset, |p| p.span.start) {
            Ok(i) => Some(&self.code_mappings[i]),
            Err(i) if i > 0 && self.code_mappings[i - 1].span.end > offset => {
                Some(&self.code_mappings[i - 1])
            }
            _ => None,
        }
    }
}

/// Traverse the macro expansion and replace the placeholders with the provided values, creates a
/// string representation of the expanded macro.
///
/// Returns an error if any used placeholder in the expansion is not found in the captures.
/// When an error is returned, appropriate diagnostics will already have been reported.
pub fn expand_macro_rule(
    db: &dyn SyntaxGroup,
    rule: &MacroRuleData,
    matcher_ctx: &mut MatcherContext,
) -> Maybe<MacroExpansionResult> {
    let node = rule.expansion.as_syntax_node();
    let mut res_buffer = String::new();
    let mut code_mappings = Vec::new();
    expand_macro_rule_ex(db, node, matcher_ctx, &mut res_buffer, &mut code_mappings)?;
    Ok(MacroExpansionResult { text: res_buffer, code_mappings })
}

/// Helper function for [expand_macro_rule]. Traverses the macro expansion and replaces the
/// placeholders with the provided values while collecting the result in res_buffer.
///
/// Returns an error if a placeholder is not found in captures.
/// When an error is returned, appropriate diagnostics will already have been reported.
fn expand_macro_rule_ex(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
    matcher_ctx: &mut MatcherContext,
    res_buffer: &mut String,
    code_mappings: &mut Vec<CodeMapping>,
) -> Maybe<()> {
    match node.kind(db) {
        SyntaxKind::ExprPath => {
            let path_node = ExprPath::from_syntax_node(db, node.clone());
            if let Some(name) = extract_placeholder(db, &path_node) {
                let rep_index = matcher_ctx
                    .placeholder_to_rep_id
                    .get(&name)
                    .and_then(|rep_id| matcher_ctx.repetition_indices.get(rep_id))
                    .copied();

                let value = matcher_ctx
                    .captures
                    .get(&name)
                    .and_then(|v| rep_index.map_or_else(|| v.first(), |i| v.get(i)))
                    .ok_or_else(skip_diagnostic)?;

                let start_offset = TextWidth::from_str(res_buffer).as_offset();
                res_buffer.push_str(&value.text);
                let end_offset = TextWidth::from_str(res_buffer).as_offset();
                let value_stable_ptr = value.stable_ptr.lookup(db);
                code_mappings.push(CodeMapping {
                    span: TextSpan { start: start_offset, end: end_offset },
                    origin: CodeOrigin::Span(TextSpan {
                        start: value_stable_ptr.span_start_without_trivia(db),
                        end: value_stable_ptr.span_end_without_trivia(db),
                    }),
                });
                return Ok(());
            }
        }
        SyntaxKind::TokenTreeParam => {
            let path_node = ast::TokenTreeParam::from_syntax_node(db, node.clone());
            if let Some(name) = extract_placeholder_from_param(db, &path_node) {
                let rep_index = matcher_ctx
                    .placeholder_to_rep_id
                    .get(&name)
                    .and_then(|rep_id| matcher_ctx.repetition_indices.get(rep_id))
                    .copied();

                let value = matcher_ctx
                    .captures
                    .get(&name)
                    .and_then(|v| rep_index.map_or_else(|| v.first(), |i| v.get(i)))
                    .ok_or_else(skip_diagnostic)?;

                let start_offset = TextWidth::from_str(res_buffer).as_offset();
                res_buffer.push_str(&value.text);
                let end_offset = TextWidth::from_str(res_buffer).as_offset();
                let value_stable_ptr = value.stable_ptr.lookup(db);
                code_mappings.push(CodeMapping {
                    span: TextSpan { start: start_offset, end: end_offset },
                    origin: CodeOrigin::Span(TextSpan {
                        start: value_stable_ptr.span_start_without_trivia(db),
                        end: value_stable_ptr.span_end_without_trivia(db),
                    }),
                });
                return Ok(());
            }
        }
        SyntaxKind::TokenTreeRepetition => {
            let repetition = ast::TokenTreeRepetition::from_syntax_node(db, node);
            let elements = repetition.elements(db).elements(db);
            let first_param = elements
                .iter()
                .find_map(|el| match el {
                    ast::TokenTree::Param(param) => Some(param),
                    _ => None,
                })
                .ok_or_else(skip_diagnostic)?;
            let placeholder_name = first_param.name(db).text(db).to_string();
            let rep_id = *matcher_ctx
                .placeholder_to_rep_id
                .get(&placeholder_name)
                .ok_or_else(skip_diagnostic)?;
            let mut index = 0;
            loop {
                matcher_ctx.repetition_indices.insert(rep_id, index);
                if matcher_ctx.is_repetition_done(rep_id) {
                    break;
                }

                if index > 0 {
                    if let ast::OptionTerminalComma::TerminalComma(sep) = repetition.separator(db) {
                        res_buffer.push_str(&sep.as_syntax_node().get_text(db));
                    }
                }

                for element in &elements {
                    expand_macro_rule_ex(
                        db,
                        element.as_syntax_node(),
                        matcher_ctx,
                        res_buffer,
                        code_mappings,
                    )?;
                }

                index += 1;
            }

            matcher_ctx.repetition_indices.swap_remove(&rep_id);
            return Ok(());
        }
        _ => {
            if node.kind(db).is_terminal() {
                res_buffer.push_str(&node.get_text(db));
                return Ok(());
            }

            for child in db.get_children(node).as_ref() {
                expand_macro_rule_ex(db, child.clone(), matcher_ctx, res_buffer, code_mappings)?;
            }
            return Ok(());
        }
    }
    if node.kind(db).is_terminal() {
        res_buffer.push_str(&node.get_text(db));
        return Ok(());
    }
    for child in db.get_children(node).iter() {
        expand_macro_rule_ex(db, child.clone(), matcher_ctx, res_buffer, code_mappings)?;
    }
    Ok(())
}

/// Query implementation of [crate::db::SemanticGroup::macro_declaration_diagnostics].
pub fn macro_declaration_diagnostics(
    db: &dyn SemanticGroup,
    macro_declaration_id: MacroDeclarationId,
) -> Diagnostics<SemanticDiagnostic> {
    priv_macro_declaration_data(db, macro_declaration_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::macro_declaration_attributes].
pub fn macro_declaration_attributes(
    db: &dyn SemanticGroup,
    macro_declaration_id: MacroDeclarationId,
) -> Maybe<Vec<Attribute>> {
    priv_macro_declaration_data(db, macro_declaration_id).map(|data| data.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::macro_declaration_resolver_data].
pub fn macro_declaration_resolver_data(
    db: &dyn SemanticGroup,
    macro_declaration_id: MacroDeclarationId,
) -> Maybe<Arc<ResolverData>> {
    priv_macro_declaration_data(db, macro_declaration_id).map(|data| data.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::macro_declaration_rules].
pub fn macro_declaration_rules(
    db: &dyn SemanticGroup,
    macro_declaration_id: MacroDeclarationId,
) -> Maybe<Vec<MacroRuleData>> {
    priv_macro_declaration_data(db, macro_declaration_id).map(|data| data.rules)
}
