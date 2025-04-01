use std::collections::HashMap;
use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, MacroDeclarationId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_parser::macro_helpers::as_expr_macro_token_tree;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::ExprPath;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::InferenceId;
use crate::resolve::{MACRO_DEF_SITE, Resolver, ResolverData};

/// A unique identifier for a repetition block inside a macro rule.
/// Each `$( ... )` group in the macro pattern gets a new `RepetitionId`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RepetitionId(usize);

/// The captures collected during macro pattern matching.
/// Each macro parameter name maps to a flat list of matched strings.
/// For repetition parameters, the list will contain all values flattened.
type Captures = HashMap<String, Vec<String>>;

/// Context used during macro pattern matching.
/// Tracks captured values, active repetition scopes, and repetition ownership per placeholder.
#[derive(Default, Clone)]
struct MatcherContext {
    /// The captured values per macro parameter name.
    /// These are flat lists, even for repeated placeholders.
    captures: Captures,

    /// Maps each placeholder to the `RepetitionId` of the repetition block
    /// they are part of. This helps the expansion phase know which iterators to advance together.
    placeholder_to_rep_id: HashMap<String, RepetitionId>,

    /// Stack of currently active repetition blocks. Used to assign placeholders
    /// to their correct `RepetitionId` while recursing into nested repetitions.
    current_repetition_stack: Vec<RepetitionId>,

    /// Counter for generating unique `RepetitionId`s.
    next_repetition_id: usize,
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
    pattern: ast::MacroMatcher,
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

/// Query implementation of [crate::db::SemanticGroup::priv_macro_declaration_data].
pub fn priv_macro_declaration_data(
    db: &dyn SemanticGroup,
    macro_declaration_id: MacroDeclarationId,
) -> Maybe<MacroDeclarationData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let diagnostics = SemanticDiagnostics::default();

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
    let rules = macro_declaration_syntax
        .rules(syntax_db)
        .elements(syntax_db)
        .into_iter()
        .map(|rule_syntax| MacroRuleData {
            pattern: rule_syntax.lhs(db.upcast()),
            expansion: rule_syntax.rhs(db.upcast()),
        })
        .collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(MacroDeclarationData { diagnostics: diagnostics.build(), attributes, resolver_data, rules })
}

/// Given a macro declaration and an input token tree, checks if the input the given rule, and
/// returns the captured params if it does.
pub fn is_macro_rule_match(
    db: &dyn SemanticGroup,
    rule: &MacroRuleData,
    input: &ast::TokenTreeNode,
) -> Option<(Captures, HashMap<String, RepetitionId>)> {
    let mut ctx = MatcherContext::default();

    let matcher_elements = match &rule.pattern {
        ast::MacroMatcher::Parenthesized(inner) => inner.elements(db.upcast()),
        ast::MacroMatcher::Braced(inner) => inner.elements(db.upcast()),
        ast::MacroMatcher::Bracketed(inner) => inner.elements(db.upcast()),
    };
    let input_elements = match input.subtree(db.upcast()) {
        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db.upcast()),
        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db.upcast()),
        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db.upcast()),
        ast::WrappedTokenTree::Missing(_) => unreachable!(),
    }
    .elements(db.upcast());
    let mut input_iter = input_elements.iter().peekable();
    is_macro_rule_match_ex(db, matcher_elements, &mut input_iter, &mut ctx)?;
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
                        return Some(());
                    }
                    _ => return None,
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
                        ctx.captures
                            .entry(placeholder_name.clone())
                            .or_default()
                            .push(captured_text);
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name.clone(), *rep_id);
                        }
                        return Some(());
                    }
                    PlaceholderKind::Expr => {
                        let peek_token = input_iter.peek()?;
                        let file_id = peek_token.as_syntax_node().stable_ptr().file_id(db.upcast());
                        let expr_node = as_expr_macro_token_tree(
                            input_iter.clone().cloned(),
                            file_id,
                            db.upcast(),
                        )?;
                        let expr_text = expr_node.as_syntax_node().get_text(db.upcast());
                        ctx.captures
                            .entry(placeholder_name.clone())
                            .or_default()
                            .push(expr_text.to_string());
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name.clone(), *rep_id);
                        }
                        let expr_length = expr_text.len();
                        let mut current_length = 0;

                        // TODO(Dean): Use the iterator directly in the parser and advance it while
                        // parsing the expression, instead of manually tracking the length and
                        // iterating separately.
                        for token_tree_leaf in input_iter.by_ref() {
                            let token_text = token_tree_leaf.as_syntax_node().get_text(db.upcast());
                            current_length += token_text.len();
                            if current_length >= expr_length {
                                break;
                            }
                        }
                        return Some(());
                    }
                }
            }
            ast::MacroRuleElement::Subtree(matcher_subtree) => {
                let input_token = input_iter.next()?;
                if let ast::TokenTree::Subtree(input_subtree) = input_token {
                    let inner_matcher = matcher_subtree.subtree(db.upcast());
                    let inner_elements = match inner_matcher {
                        ast::MacroMatcher::Parenthesized(p) => p.elements(db.upcast()),
                        ast::MacroMatcher::Braced(b) => b.elements(db.upcast()),
                        ast::MacroMatcher::Bracketed(bk) => bk.elements(db.upcast()),
                    };
                    let inner_input_elements = match input_subtree.subtree(db.upcast()) {
                        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db.upcast()),
                        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db.upcast()),
                        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db.upcast()),
                        ast::WrappedTokenTree::Missing(_) => unreachable!(),
                    }
                    .elements(db.upcast());
                    let mut inner_input_iter = inner_input_elements.iter().peekable();
                    is_macro_rule_match_ex(db, inner_elements, &mut inner_input_iter, ctx)?;
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
                    if is_macro_rule_match_ex(db, elements.clone(), &mut temp_iter, &mut inner_ctx)
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
                let is_valid = match operator {
                    ast::MacroRepetitionOperator::ZeroOrOne(_) => match_count <= 1,
                    ast::MacroRepetitionOperator::OneOrMore(_) => match_count >= 1,
                    ast::MacroRepetitionOperator::ZeroOrMore(_) => true,
                };
                if !is_valid {
                    return None;
                }
                ctx.current_repetition_stack.pop();
            }
        }
    }
    if input_iter.next().is_some() {
        return None;
    }
    Some(())
}

/// Traverse the macro expansion and replace the placeholders with the provided values, creates a
/// string representation of the expanded macro.
pub fn expand_macro_rule(
    db: &dyn SyntaxGroup,
    rule: &MacroRuleData,
    captures: &OrderedHashMap<String, String>,
) -> String {
    let node = rule.expansion.as_syntax_node();
    let mut res_buffer = String::new();
    expand_macro_rule_ex(db, node, captures, &mut res_buffer);
    res_buffer
}

/// Helper function for [expand_macro_rule]. Traverses the macro expansion and replaces the
/// placeholders with the provided values while collecting the result in res_buffer.
fn expand_macro_rule_ex(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
    captures: &OrderedHashMap<String, String>,
    res_buffer: &mut String,
) {
    if node.kind(db) == SyntaxKind::ExprPath {
        let path_node = ExprPath::from_syntax_node(db, node.clone());

        if let ast::OptionTerminalDollar::TerminalDollar(_) = path_node.dollar(db) {
            let placeholder_name = path_node.identifier(db).to_string();
            if path_node.segments(db).elements(db).len() == 1 && placeholder_name != MACRO_DEF_SITE
            {
                if let Some(value) = captures.get(&placeholder_name) {
                    res_buffer.push_str(value);
                } else {
                    // TODO(Gil): verify in the declaration that all the used placeholders in the
                    // expansion are present in the captures.
                    panic!("Placeholder not found in captures.");
                }
                return;
            }
        }
    }
    if node.kind(db).is_terminal() {
        res_buffer.push_str(&node.get_text(db));
        return;
    }
    for child in db.get_children(node).iter() {
        expand_macro_rule_ex(db, child.clone(), captures, res_buffer);
    }
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
