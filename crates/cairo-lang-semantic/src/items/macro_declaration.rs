use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, MacroDeclarationId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::ExprPlaceholder;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::InferenceId;
use crate::resolve::{Resolver, ResolverData};

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
}

impl From<ast::MacroRuleParamKind> for PlaceholderKind {
    fn from(kind: ast::MacroRuleParamKind) -> Self {
        match kind {
            ast::MacroRuleParamKind::Identifier(_) => PlaceholderKind::Identifier,
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
) -> Option<OrderedHashMap<String, String>> {
    let mut captures = OrderedHashMap::default();
    let matcher_elements = match &rule.pattern {
        ast::MacroMatcher::Parenthesized(inner) => {
            inner.elements(db.upcast()).elements(db.upcast())
        }
        ast::MacroMatcher::Braced(inner) => inner.elements(db.upcast()).elements(db.upcast()),
        ast::MacroMatcher::Bracketed(inner) => inner.elements(db.upcast()).elements(db.upcast()),
    };
    let input_elements = {
        let db = db.upcast();
        match input.subtree(db) {
            ast::WrappedTokenTree::Parenthesized(parenthesized_token_tree) => {
                parenthesized_token_tree.tokens(db)
            }
            ast::WrappedTokenTree::Braced(braced_token_tree) => braced_token_tree.tokens(db),
            ast::WrappedTokenTree::Bracketed(bracketed_token_tree) => {
                bracketed_token_tree.tokens(db)
            }
            ast::WrappedTokenTree::Missing(_) => unreachable!(),
        }
    }
    .elements(db.upcast());
    let mut input_iter = input_elements.iter();
    for matcher_element in matcher_elements {
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
                    ast::TokenTree::Subtree(_) => {
                        return None;
                    }
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
            ast::MacroRuleElement::Param(param) => {
                let placeholder_kind: PlaceholderKind = param.kind(db.upcast()).into();
                let placeholder_name =
                    param.name(db.upcast()).as_syntax_node().get_text_without_trivia(db.upcast());
                match placeholder_kind {
                    PlaceholderKind::Identifier => {
                        let token_tree_leaf = input_iter.next()?;
                        match token_tree_leaf {
                            ast::TokenTree::Token(token_tree_leaf) => {
                                match token_tree_leaf.leaf(db.upcast()) {
                                    ast::TokenNode::TerminalIdentifier(terminal_identifier) => {
                                        captures.insert(
                                            placeholder_name,
                                            terminal_identifier.text(db.upcast()).to_string(),
                                        );
                                    }
                                    _ => {
                                        return None;
                                    }
                                }
                            }
                            ast::TokenTree::Subtree(_) => {
                                return None;
                            }
                            ast::TokenTree::Missing(_) => unreachable!(),
                        }
                    }
                }
            }
            ast::MacroRuleElement::Subtree(matcher_subtree) => {
                let input_token = input_iter.next()?;
                match input_token {
                    ast::TokenTree::Subtree(input_subtree) => {
                        let matcher_inner_elements = match matcher_subtree.subtree(db.upcast()) {
                            ast::MacroMatcher::Parenthesized(inner) => inner.elements(db.upcast()),
                            ast::MacroMatcher::Braced(inner) => inner.elements(db.upcast()),
                            ast::MacroMatcher::Bracketed(inner) => inner.elements(db.upcast()),
                        };

                        let input_inner_elements = match input_subtree.subtree(db.upcast()) {
                            ast::WrappedTokenTree::Parenthesized(tt) => {
                                tt.tokens(db.upcast()).elements(db.upcast())
                            }
                            ast::WrappedTokenTree::Braced(tt) => {
                                tt.tokens(db.upcast()).elements(db.upcast())
                            }
                            ast::WrappedTokenTree::Bracketed(tt) => {
                                tt.tokens(db.upcast()).elements(db.upcast())
                            }
                            ast::WrappedTokenTree::Missing(_) => unreachable!(),
                        };

                        let inner_captures = match_macro_rule_elements(
                            db.upcast(),
                            &matcher_inner_elements.elements(db.upcast()),
                            &input_inner_elements,
                        )?;

                        for (placeholder_name, captured_value) in inner_captures {
                            if let Some(existing_value) = captures.get(&placeholder_name) {
                                if existing_value != &captured_value {
                                    return None;
                                }
                            } else {
                                captures.insert(placeholder_name, captured_value);
                            }
                        }
                    }
                    ast::TokenTree::Token(_) => return None,
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
        }
    }
    if input_iter.next().is_some() {
        return None;
    }
    Some(captures)
}

/// Recursively matches a sequence of macro rule elements against a sequence of input tokens.
/// If the input matches the pattern, it returns a map of captured identifiers.
fn match_macro_rule_elements(
    db: &dyn SyntaxGroup,
    matcher_elements: &[ast::MacroRuleElement],
    input_elements: &[ast::TokenTree],
) -> Option<OrderedHashMap<String, String>> {
    let mut captures = OrderedHashMap::default();
    let mut input_iter = input_elements.iter();

    for matcher_element in matcher_elements {
        match matcher_element {
            ast::MacroRuleElement::Token(matcher_token) => {
                let input_token = input_iter.next()?;
                match input_token {
                    ast::TokenTree::Token(token_tree_leaf) => {
                        if matcher_token.as_syntax_node().get_text_without_trivia(db)
                            != token_tree_leaf.as_syntax_node().get_text_without_trivia(db)
                        {
                            return None;
                        }
                    }
                    ast::TokenTree::Subtree(_) => return None,
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
            ast::MacroRuleElement::Param(param) => {
                let placeholder_kind: PlaceholderKind = param.kind(db).into();
                let placeholder_name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                let input_token = input_iter.next()?;
                match placeholder_kind {
                    PlaceholderKind::Identifier => match input_token {
                        ast::TokenTree::Token(token_tree_leaf) => match token_tree_leaf.leaf(db) {
                            ast::TokenNode::TerminalIdentifier(terminal_identifier) => {
                                captures.insert(
                                    placeholder_name,
                                    terminal_identifier.text(db).to_string(),
                                );
                            }
                            _ => return None,
                        },
                        ast::TokenTree::Subtree(_) => return None,
                        ast::TokenTree::Missing(_) => unreachable!(),
                    },
                }
            }
            ast::MacroRuleElement::Subtree(matcher_subtree) => {
                let input_token = input_iter.next()?;
                match input_token {
                    ast::TokenTree::Subtree(input_subtree) => {
                        let matcher_inner_elements = match matcher_subtree.subtree(db) {
                            ast::MacroMatcher::Parenthesized(inner) => inner.elements(db),
                            ast::MacroMatcher::Braced(inner) => inner.elements(db),
                            ast::MacroMatcher::Bracketed(inner) => inner.elements(db),
                        };

                        let input_inner_elements = match input_subtree.subtree(db) {
                            ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db).elements(db),
                            ast::WrappedTokenTree::Braced(tt) => tt.tokens(db).elements(db),
                            ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db).elements(db),
                            ast::WrappedTokenTree::Missing(_) => unreachable!(),
                        };

                        let inner_captures = match_macro_rule_elements(
                            db,
                            &matcher_inner_elements.elements(db),
                            &input_inner_elements,
                        )?;
                        for (placeholder_name, captured_value) in inner_captures {
                            if let Some(existing_value) = captures.get(&placeholder_name) {
                                if existing_value != &captured_value {
                                    return None;
                                }
                            } else {
                                captures.insert(placeholder_name, captured_value);
                            }
                        }
                    }
                    ast::TokenTree::Token(_) => return None,
                    ast::TokenTree::Missing(_) => unreachable!(),
                }
            }
        }
    }
    if input_iter.next().is_some() {
        return None;
    }
    Some(captures)
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
    if node.kind(db) == SyntaxKind::ExprPlaceholder {
        let placeholder_node = ExprPlaceholder::from_syntax_node(db, node.clone());
        let placeholder_name =
            placeholder_node.name(db).as_syntax_node().get_text_without_trivia(db);
        if let Some(value) = captures.get(&placeholder_name) {
            res_buffer.push_str(value);
        } else {
            // TODO(Gil): verify in the declaration that all the used placeholders in the expansion
            // are present in the captures.
            panic!("Placeholder not found in captures.");
        }
        return;
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
