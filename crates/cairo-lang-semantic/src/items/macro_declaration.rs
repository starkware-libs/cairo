use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MacroDeclarationId, ModuleId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, skip_diagnostic};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin, SmolStrId};
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_parser::macro_helpers::as_expr_macro_token_tree;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::{MacroElement, MacroParam};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use salsa::Database;

use crate::SemanticDiagnostic;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::keyword::{MACRO_CALL_SITE, MACRO_DEF_SITE};
use crate::resolve::{Resolver, ResolverData};

/// A unique identifier for a repetition block inside a macro rule.
/// Each `$( ... )` group in the macro pattern gets a new `RepetitionId`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RepetitionId(usize);

/// The captures collected during macro pattern matching.
/// Each macro parameter name maps to a flat list of matched strings.
type Captures<'db> = OrderedHashMap<SmolStrId<'db>, Vec<CapturedValue<'db>>>;

/// Context used during macro pattern matching and expansion.
/// Tracks captured values, active repetition scopes, and repetition ownership per placeholder.
#[derive(Default, Clone, Debug)]
pub struct MatcherContext<'db> {
    /// The captured values per macro parameter name.
    /// These are flat lists, even for repeated placeholders.
    pub captures: Captures<'db>,

    /// Maps each placeholder to the `RepetitionId` of the repetition block
    /// they are part of. This helps the expansion phase know which iterators to advance together.
    pub placeholder_to_rep_id: OrderedHashMap<SmolStrId<'db>, RepetitionId>,

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
    pub repetition_operators: OrderedHashMap<RepetitionId, ast::MacroRepetitionOperator<'db>>,
}

/// The semantic data for a macro declaration.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct MacroDeclarationData<'db> {
    rules: Vec<MacroRuleData<'db>>,
    attributes: Vec<Attribute<'db>>,
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// The semantic data for a single macro rule in a macro declaration.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct MacroRuleData<'db> {
    pub pattern: ast::WrappedMacro<'db>,
    pub expansion: ast::MacroElements<'db>,
}

/// The possible kinds of placeholders in a macro rule.
#[derive(Debug, Clone, PartialEq, Eq)]
enum PlaceholderKind {
    Identifier,
    Expr,
}

impl<'db> From<ast::MacroParamKind<'db>> for PlaceholderKind {
    fn from(kind: ast::MacroParamKind<'db>) -> Self {
        match kind {
            ast::MacroParamKind::Identifier(_) => PlaceholderKind::Identifier,
            ast::MacroParamKind::Expr(_) => PlaceholderKind::Expr,
            ast::MacroParamKind::Missing(_) => unreachable!(
                "Missing macro rule param kind, should have been handled by the parser."
            ),
        }
    }
}

/// Information about a captured value in a macro.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CapturedValue<'db> {
    pub text: String,
    pub stable_ptr: SyntaxStablePtrId<'db>,
}

/// Implementation of [MacroDeclarationSemantic::priv_macro_declaration_data].
fn priv_macro_declaration_data<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<MacroDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();

    let module_id = macro_declaration_id.module_id(db);
    let macro_declaration_syntax = db.module_macro_declaration_by_id(macro_declaration_id)?;
    if !are_user_defined_inline_macros_enabled(db, module_id) {
        diagnostics.report(
            macro_declaration_syntax.stable_ptr(db).untyped(),
            SemanticDiagnosticKind::UserDefinedInlineMacrosDisabled,
        );
    }

    let attributes = macro_declaration_syntax.attributes(db).structurize(db);
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::MacroDeclaration(macro_declaration_id),
    ));
    let resolver = Resolver::new(db, module_id, inference_id);

    // TODO(Dean): Verify uniqueness of param names.
    // TODO(Dean): Verify consistency bracket terminals.
    let mut rules = vec![];
    for rule_syntax in macro_declaration_syntax.rules(db).elements(db) {
        let pattern = rule_syntax.lhs(db);
        let expansion = rule_syntax.rhs(db).elements(db);
        let pattern_elements = get_macro_elements(db, pattern.clone());
        // Collect defined placeholders from pattern
        let defined_placeholders =
            OrderedHashSet::<_>::from_iter(pattern_elements.elements(db).filter_map(|element| {
                match element {
                    ast::MacroElement::Param(param) => {
                        Some(param.name(db).as_syntax_node().get_text_without_trivia(db))
                    }
                    ast::MacroElement::Repetition(repetition) => repetition
                        .elements(db)
                        .elements(db)
                        .filter_map(|inner_element| match inner_element {
                            ast::MacroElement::Param(inner_param) => Some(
                                inner_param.name(db).as_syntax_node().get_text_without_trivia(db),
                            ),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                        .into_iter()
                        .next(),
                    _ => None,
                }
            }));

        let used_placeholders = collect_expansion_placeholders(db, expansion.as_syntax_node());
        // Verify all used placeholders are defined
        for (placeholder_ptr, used_placeholder) in used_placeholders {
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

/// Query implementation of [MacroDeclarationSemantic::priv_macro_declaration_data].
#[salsa::tracked]
fn priv_macro_declaration_data_tracked<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<MacroDeclarationData<'db>> {
    priv_macro_declaration_data(db, macro_declaration_id)
}

/// Helper function to extract pattern elements from a WrappedMacro.
fn get_macro_elements<'db>(
    db: &'db dyn Database,
    pattern: ast::WrappedMacro<'db>,
) -> ast::MacroElements<'db> {
    match pattern {
        ast::WrappedMacro::Parenthesized(inner) => inner.elements(db),
        ast::WrappedMacro::Braced(inner) => inner.elements(db),
        ast::WrappedMacro::Bracketed(inner) => inner.elements(db),
    }
}

/// Helper function to extract a placeholder name from an ExprPath node, if it represents a macro
/// placeholder. Returns None if the path is not a valid macro placeholder.
fn extract_placeholder<'db>(
    db: &'db dyn Database,
    path_node: &MacroParam<'db>,
) -> Option<SmolStrId<'db>> {
    let placeholder_name = path_node.name(db).as_syntax_node().get_text_without_trivia(db);
    if ![MACRO_DEF_SITE, MACRO_CALL_SITE].contains(&placeholder_name.long(db).as_str()) {
        return Some(placeholder_name);
    }
    None
}

/// Helper function to collect all placeholder names used in a macro expansion.
fn collect_expansion_placeholders<'db>(
    db: &'db dyn Database,
    node: SyntaxNode<'db>,
) -> Vec<(SyntaxStablePtrId<'db>, SmolStrId<'db>)> {
    let mut placeholders = Vec::new();
    if node.kind(db) == SyntaxKind::MacroParam {
        let path_node = MacroParam::from_syntax_node(db, node);
        if let Some(placeholder_name) = extract_placeholder(db, &path_node) {
            placeholders.push((path_node.stable_ptr(db).untyped(), placeholder_name));
            return placeholders;
        }
    }
    if node.kind(db) == SyntaxKind::MacroRepetition {
        let repetition = ast::MacroRepetition::from_syntax_node(db, node);
        for element in repetition.elements(db).elements(db) {
            placeholders.extend(collect_expansion_placeholders(db, element.as_syntax_node()));
        }
        return placeholders;
    }
    if !node.kind(db).is_terminal() {
        for child in node.get_children(db).iter() {
            placeholders.extend(collect_expansion_placeholders(db, *child));
        }
    }
    placeholders
}

/// Given a macro declaration and an input token tree, checks if the input the given rule, and
/// returns the captured params if it does.
pub fn is_macro_rule_match<'db>(
    db: &'db dyn Database,
    rule: &MacroRuleData<'db>,
    input: &ast::TokenTreeNode<'db>,
) -> Option<(Captures<'db>, OrderedHashMap<SmolStrId<'db>, RepetitionId>)> {
    let mut ctx = MatcherContext::default();

    let matcher_elements = get_macro_elements(db, rule.pattern.clone());
    let input_elements = match input.subtree(db) {
        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db),
        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db),
        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db),
        ast::WrappedTokenTree::Missing(_) => unreachable!(),
    }
    .elements_vec(db);
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
fn is_macro_rule_match_ex<'db>(
    db: &'db dyn Database,
    matcher_elements: ast::MacroElements<'db>,
    input_iter: &mut std::iter::Peekable<std::slice::Iter<'_, ast::TokenTree<'db>>>,
    ctx: &mut MatcherContext<'db>,
    consume_all_input: bool,
) -> Option<()> {
    for matcher_element in matcher_elements.elements(db) {
        match matcher_element {
            ast::MacroElement::Token(matcher_token) => {
                let input_token = input_iter.next()?;
                match input_token {
                    ast::TokenTree::Token(token_tree_leaf) => {
                        if matcher_token.as_syntax_node().get_text_without_trivia(db)
                            != token_tree_leaf.as_syntax_node().get_text_without_trivia(db)
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
            ast::MacroElement::Param(param) => {
                let placeholder_kind: PlaceholderKind =
                    if let ast::OptionParamKind::ParamKind(param_kind) = param.kind(db) {
                        param_kind.kind(db).into()
                    } else {
                        unreachable!(
                            "Missing macro rule param kind, should have been handled by the \
                             parser."
                        )
                    };
                let placeholder_name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                match placeholder_kind {
                    PlaceholderKind::Identifier => {
                        let input_token = input_iter.next()?;
                        let captured_text = match input_token {
                            ast::TokenTree::Token(token_tree_leaf) => {
                                match token_tree_leaf.leaf(db) {
                                    ast::TokenNode::TerminalIdentifier(terminal_identifier) => {
                                        terminal_identifier.text(db).to_string(db)
                                    }
                                    _ => return None,
                                }
                            }
                            _ => return None,
                        };
                        ctx.captures.entry(placeholder_name).or_default().push(CapturedValue {
                            text: captured_text,
                            stable_ptr: input_token.stable_ptr(db).untyped(),
                        });
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name, *rep_id);
                        }
                        continue;
                    }
                    PlaceholderKind::Expr => {
                        let mut cloned_iter = input_iter.clone();
                        let peek_token = cloned_iter.peek()?;
                        let file_id = peek_token.as_syntax_node().stable_ptr(db).file_id(db);
                        let expr_node =
                            as_expr_macro_token_tree(input_iter.clone().cloned(), file_id, db)?;
                        let expr_text = expr_node.as_syntax_node().get_text(db);
                        let expr_length = expr_text.len();
                        // An empty expression is parsed successfully. However we don't want to
                        // capture it a valid expr.
                        if expr_length == 0 {
                            return None;
                        }

                        ctx.captures.entry(placeholder_name).or_default().push(CapturedValue {
                            text: expr_text.to_string(),
                            stable_ptr: peek_token.stable_ptr(db).untyped(),
                        });
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name, *rep_id);
                        }
                        let expr_length = expr_text.len();
                        let mut current_length = 0;

                        // TODO(Dean): Use the iterator directly in the parser and advance it while
                        // parsing the expression, instead of manually tracking the length and
                        // iterating separately.
                        for token_tree_leaf in input_iter.by_ref() {
                            let token_text = match token_tree_leaf {
                                ast::TokenTree::Token(token_tree_leaf) => {
                                    token_tree_leaf.as_syntax_node().get_text(db)
                                }
                                ast::TokenTree::Subtree(token_subtree) => {
                                    token_subtree.as_syntax_node().get_text(db)
                                }
                                ast::TokenTree::Repetition(token_repetition) => {
                                    token_repetition.as_syntax_node().get_text(db)
                                }
                                ast::TokenTree::Param(token_param) => {
                                    token_param.as_syntax_node().get_text(db)
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
            ast::MacroElement::Subtree(matcher_subtree) => {
                let input_token = input_iter.next()?;
                if let ast::TokenTree::Subtree(input_subtree) = input_token {
                    let inner_elements = get_macro_elements(db, matcher_subtree.subtree(db));
                    let inner_input_elements = match input_subtree.subtree(db) {
                        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db),
                        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db),
                        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db),
                        ast::WrappedTokenTree::Missing(_) => unreachable!(),
                    }
                    .elements_vec(db);
                    let mut inner_input_iter = inner_input_elements.iter().peekable();
                    is_macro_rule_match_ex(db, inner_elements, &mut inner_input_iter, ctx, true)?;
                    continue;
                } else {
                    return None;
                }
            }
            ast::MacroElement::Repetition(repetition) => {
                let rep_id = RepetitionId(ctx.next_repetition_id);
                ctx.next_repetition_id += 1;
                ctx.current_repetition_stack.push(rep_id);
                let elements = repetition.elements(db);
                let operator = repetition.operator(db);
                let separator_token = repetition.separator(db);
                let expected_separator = match separator_token {
                    ast::OptionTerminalComma::TerminalComma(sep) => {
                        Some(sep.as_syntax_node().get_text_without_trivia(db))
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
                            let actual = token_leaf.as_syntax_node().get_text_without_trivia(db);
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
                for placeholder_name in ctx.captures.keys() {
                    ctx.placeholder_to_rep_id.insert(*placeholder_name, rep_id);
                }

                for i in 0..match_count {
                    ctx.repetition_indices.insert(rep_id, i);
                }
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

fn validate_repetition_operator_constraints(ctx: &MatcherContext<'_>) -> bool {
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
    pub text: Arc<str>,
    /// Information about placeholder expansions in this macro expansion.
    pub code_mappings: Arc<[CodeMapping]>,
}

/// Traverse the macro expansion and replace the placeholders with the provided values, creates a
/// string representation of the expanded macro.
///
/// Returns an error if any used placeholder in the expansion is not found in the captures.
/// When an error is returned, appropriate diagnostics will already have been reported.
pub fn expand_macro_rule(
    db: &dyn Database,
    rule: &MacroRuleData<'_>,
    matcher_ctx: &mut MatcherContext<'_>,
) -> Maybe<MacroExpansionResult> {
    let node = rule.expansion.as_syntax_node();
    let mut res_buffer = String::new();
    let mut code_mappings = Vec::new();
    expand_macro_rule_ex(db, node, matcher_ctx, &mut res_buffer, &mut code_mappings)?;
    Ok(MacroExpansionResult { text: res_buffer.into(), code_mappings: code_mappings.into() })
}

/// Helper function for [expand_macro_rule]. Traverses the macro expansion and replaces the
/// placeholders with the provided values while collecting the result in res_buffer.
///
/// Returns an error if a placeholder is not found in captures.
/// When an error is returned, appropriate diagnostics will already have been reported.
fn expand_macro_rule_ex(
    db: &dyn Database,
    node: SyntaxNode<'_>,
    matcher_ctx: &mut MatcherContext<'_>,
    res_buffer: &mut String,
    code_mappings: &mut Vec<CodeMapping>,
) -> Maybe<()> {
    match node.kind(db) {
        SyntaxKind::MacroParam => {
            let path_node = MacroParam::from_syntax_node(db, node);
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
                let start = TextWidth::from_str(res_buffer).as_offset();
                let span = TextSpan::new_with_width(start, TextWidth::from_str(&value.text));
                res_buffer.push_str(&value.text);
                code_mappings.push(CodeMapping {
                    span,
                    origin: CodeOrigin::Span(value.stable_ptr.lookup(db).span_without_trivia(db)),
                });
                return Ok(());
            }
        }
        SyntaxKind::MacroRepetition => {
            let repetition = ast::MacroRepetition::from_syntax_node(db, node);
            let elements = repetition.elements(db).elements_vec(db);
            let repetition_params = get_repetition_params(db, elements.iter().cloned());
            let first_param = repetition_params.first().ok_or_else(skip_diagnostic)?;
            let placeholder_name = first_param.name(db).text(db);
            let rep_id = *matcher_ctx
                .placeholder_to_rep_id
                .get(&placeholder_name)
                .ok_or_else(skip_diagnostic)?;
            let repetition_len =
                matcher_ctx.captures.get(&placeholder_name).map(|v| v.len()).unwrap_or(0);
            for i in 0..repetition_len {
                matcher_ctx.repetition_indices.insert(rep_id, i);
                for element in &elements {
                    expand_macro_rule_ex(
                        db,
                        element.as_syntax_node(),
                        matcher_ctx,
                        res_buffer,
                        code_mappings,
                    )?;
                }

                if i + 1 < repetition_len
                    && let ast::OptionTerminalComma::TerminalComma(sep) = repetition.separator(db)
                {
                    res_buffer.push_str(sep.as_syntax_node().get_text(db));
                }
            }

            matcher_ctx.repetition_indices.swap_remove(&rep_id);
            return Ok(());
        }
        _ => {
            if node.kind(db).is_terminal() {
                res_buffer.push_str(node.get_text(db));
                return Ok(());
            }

            for child in node.get_children(db).iter() {
                expand_macro_rule_ex(db, *child, matcher_ctx, res_buffer, code_mappings)?;
            }
            return Ok(());
        }
    }
    if node.kind(db).is_terminal() {
        res_buffer.push_str(node.get_text(db));
        return Ok(());
    }
    for child in node.get_children(db).iter() {
        expand_macro_rule_ex(db, *child, matcher_ctx, res_buffer, code_mappings)?;
    }
    Ok(())
}

/// Gets a Vec of MacroElement, and returns a vec of the params within it.
fn get_repetition_params<'db>(
    db: &'db dyn Database,
    elements: impl IntoIterator<Item = MacroElement<'db>>,
) -> Vec<MacroParam<'db>> {
    let mut params = vec![];
    repetition_params_extend(db, elements, &mut params);
    params
}

/// Recursively extends the provided params vector with all params within the given macro elements.
fn repetition_params_extend<'db>(
    db: &'db dyn Database,
    elements: impl IntoIterator<Item = MacroElement<'db>>,
    params: &mut Vec<MacroParam<'db>>,
) {
    for element in elements {
        match element {
            ast::MacroElement::Param(param) => {
                params.push(param);
            }
            ast::MacroElement::Subtree(subtree) => {
                let inner_elements = get_macro_elements(db, subtree.subtree(db)).elements(db);
                repetition_params_extend(db, inner_elements, params);
            }
            ast::MacroElement::Repetition(repetition) => {
                let inner_elements = repetition.elements(db).elements(db);
                repetition_params_extend(db, inner_elements, params);
            }
            ast::MacroElement::Token(_) => {}
        }
    }
}

/// Implementation of [MacroDeclarationSemantic::macro_declaration_diagnostics].
fn macro_declaration_diagnostics<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    priv_macro_declaration_data(db, macro_declaration_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [MacroDeclarationSemantic::macro_declaration_diagnostics].
#[salsa::tracked]
fn macro_declaration_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    macro_declaration_diagnostics(db, macro_declaration_id)
}

/// Implementation of [MacroDeclarationSemantic::macro_declaration_attributes].
fn macro_declaration_attributes<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    priv_macro_declaration_data(db, macro_declaration_id).map(|data| data.attributes)
}

/// Query implementation of [MacroDeclarationSemantic::macro_declaration_attributes].
#[salsa::tracked]
fn macro_declaration_attributes_tracked<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    macro_declaration_attributes(db, macro_declaration_id)
}

/// Implementation of [MacroDeclarationSemantic::macro_declaration_resolver_data].
fn macro_declaration_resolver_data<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    priv_macro_declaration_data(db, macro_declaration_id).map(|data| data.resolver_data)
}

/// Query implementation of [MacroDeclarationSemantic::macro_declaration_resolver_data].
#[salsa::tracked]
fn macro_declaration_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    macro_declaration_resolver_data(db, macro_declaration_id)
}

/// Implementation of [MacroDeclarationSemantic::macro_declaration_rules].
fn macro_declaration_rules<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<Vec<MacroRuleData<'db>>> {
    priv_macro_declaration_data(db, macro_declaration_id).map(|data| data.rules)
}

/// Query implementation of [MacroDeclarationSemantic::macro_declaration_rules].
#[salsa::tracked]
fn macro_declaration_rules_tracked<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<Vec<MacroRuleData<'db>>> {
    macro_declaration_rules(db, macro_declaration_id)
}

/// Returns true if user defined user macros are enabled for the given module.
fn are_user_defined_inline_macros_enabled<'db>(
    db: &dyn Database,
    module_id: ModuleId<'db>,
) -> bool {
    let owning_crate = module_id.owning_crate(db);
    let Some(config) = db.crate_config(owning_crate) else { return false };
    config.settings.experimental_features.user_defined_inline_macros
}

/// Trait for macro declaration-related semantic queries.
pub trait MacroDeclarationSemantic<'db>: Database {
    /// Private query to compute data about a macro declaration.
    fn priv_macro_declaration_data(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<MacroDeclarationData<'db>> {
        priv_macro_declaration_data_tracked(self.as_dyn_database(), macro_id)
    }
    /// Returns the semantic diagnostics of a macro declaration.
    fn macro_declaration_diagnostics(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        macro_declaration_diagnostics_tracked(self.as_dyn_database(), macro_id)
    }
    /// Returns the resolver data of a macro declaration.
    fn macro_declaration_resolver_data(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        macro_declaration_resolver_data_tracked(self.as_dyn_database(), macro_id)
    }
    /// Returns the attributes of a macro declaration.
    fn macro_declaration_attributes(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>> {
        macro_declaration_attributes_tracked(self.as_dyn_database(), macro_id)
    }
    /// Returns the rules semantic data of a macro declaration.
    fn macro_declaration_rules(
        &'db self,
        macro_id: MacroDeclarationId<'db>,
    ) -> Maybe<Vec<MacroRuleData<'db>>> {
        macro_declaration_rules_tracked(self.as_dyn_database(), macro_id)
    }
}
impl<'db, T: Database + ?Sized> MacroDeclarationSemantic<'db> for T {}
