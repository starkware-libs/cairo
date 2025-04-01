use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, MacroDeclarationId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe, skip_diagnostic};
use cairo_lang_parser::macro_helpers::as_expr_macro_token_tree;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::ExprPath;
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
) -> Option<OrderedHashMap<String, String>> {
    let mut captures = OrderedHashMap::default();
    is_macro_rule_match_ex(db, rule.pattern.clone(), input, &mut captures)?;
    Some(captures)
}

/// Helper function for [expand_macro_rule].
/// Traverses the macro expansion and replaces the placeholders with the provided values,
/// while collecting the result in `res_buffer`.
fn is_macro_rule_match_ex(
    db: &dyn SemanticGroup,
    pattern: ast::MacroMatcher,
    input: &ast::TokenTreeNode,
    captures: &mut OrderedHashMap<String, String>,
) -> Option<()> {
    let matcher_elements = get_pattern_elements(db.upcast(), pattern);

    let input_elements = {
        let db = db.upcast();
        match input.subtree(db) {
            ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db),
            ast::WrappedTokenTree::Braced(tt) => tt.tokens(db),
            ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db),
            ast::WrappedTokenTree::Missing(_) => unreachable!(),
        }
    }
    .elements(db.upcast());
    let mut input_iter = input_elements.iter();
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
                    }
                    ast::TokenTree::Subtree(_) => return None,
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
            ast::MacroRuleElement::Param(param) => {
                let placeholder_kind: PlaceholderKind = param.kind(db.upcast()).into();
                let placeholder_name =
                    param.name(db.upcast()).as_syntax_node().get_text_without_trivia(db.upcast());
                match placeholder_kind {
                    PlaceholderKind::Identifier => {
                        let token_tree_leaf: &ast::TokenTree = input_iter.next()?;
                        match token_tree_leaf {
                            ast::TokenTree::Token(token_tree_leaf) => {
                                match token_tree_leaf.leaf(db.upcast()) {
                                    ast::TokenNode::TerminalIdentifier(terminal_identifier) => {
                                        captures.insert(
                                            placeholder_name,
                                            terminal_identifier.text(db.upcast()).to_string(),
                                        );
                                    }
                                    _ => return None,
                                }
                            }
                            ast::TokenTree::Subtree(_) => return None,
                            ast::TokenTree::Missing(_) => unreachable!(),
                        }
                    }
                    PlaceholderKind::Expr => {
                        let expr_node = as_expr_macro_token_tree(
                            input_iter.clone().cloned(),
                            input.stable_ptr().0.file_id(db.upcast()),
                            db.upcast(),
                        )?;
                        let expr_text = expr_node.as_syntax_node().get_text(db.upcast());
                        captures.insert(placeholder_name, expr_text.to_string());
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
                                ast::TokenTree::Missing(_) => unreachable!(),
                            };
                            current_length += token_text.len();
                            if current_length >= expr_length {
                                break;
                            }
                        }
                    }
                }
            }
            ast::MacroRuleElement::Subtree(matcher_subtree) => {
                let input_token = input_iter.next()?;
                match input_token {
                    ast::TokenTree::Subtree(input_subtree) => {
                        is_macro_rule_match_ex(
                            db,
                            matcher_subtree.subtree(db.upcast()),
                            input_subtree,
                            captures,
                        )?;
                    }
                    ast::TokenTree::Token(_) => return None,
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
            ast::MacroRuleElement::Repetition(_) => todo!(),
        }
    }
    if input_iter.next().is_some() {
        return None;
    }
    Some(())
}

/// Traverse the macro expansion and replace the placeholders with the provided values, creates a
/// string representation of the expanded macro.
///
/// Returns an error if any used placeholder in the expansion is not found in the captures.
/// When an error is returned, appropriate diagnostics will already have been reported.
pub fn expand_macro_rule(
    db: &dyn SyntaxGroup,
    rule: &MacroRuleData,
    captures: &OrderedHashMap<String, String>,
) -> Maybe<String> {
    let node = rule.expansion.as_syntax_node();
    let mut res_buffer = String::new();
    expand_macro_rule_ex(db, node, captures, &mut res_buffer)?;
    Ok(res_buffer)
}

/// Helper function for [expand_macro_rule]. Traverses the macro expansion and replaces the
/// placeholders with the provided values while collecting the result in res_buffer.
///
/// Returns an error if a placeholder is not found in captures.
/// When an error is returned, appropriate diagnostics will already have been reported.
fn expand_macro_rule_ex(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
    captures: &OrderedHashMap<String, String>,
    res_buffer: &mut String,
) -> Maybe<()> {
    if node.kind(db) == SyntaxKind::ExprPath {
        let path_node = ExprPath::from_syntax_node(db, node.clone());
        if let Some(placeholder_name) = extract_placeholder(db, &path_node) {
            match captures.get(&placeholder_name) {
                Some(value) => {
                    res_buffer.push_str(value);
                    return Ok(());
                }
                None => return Err(skip_diagnostic()),
            }
        }
    }
    if node.kind(db).is_terminal() {
        res_buffer.push_str(&node.get_text(db));
        return Ok(());
    }
    for child in db.get_children(node).iter() {
        expand_macro_rule_ex(db, child.clone(), captures, res_buffer)?;
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
