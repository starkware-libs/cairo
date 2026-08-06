use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MacroDeclarationId, ModuleId, ModuleItemId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Diagnostics, Maybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin, SmolStrId};
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_parser::macro_helpers::as_expr_macro_token_tree;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::MacroParam;
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
struct RepetitionId(usize);

/// The values a macro rule's pattern captured, per placeholder name - see [`CaptureTree`].
pub type CaptureTrees<'db> = OrderedHashMap<SmolStrId<'db>, CaptureTree<'db>>;

/// The values captured for a single placeholder, in a tree mirroring the repetition nesting of the
/// pattern that matched them.
///
/// A placeholder nested in `d` `$()` repetitions of the pattern has each of its values in a
/// [`CaptureTree::Leaf`] nested under exactly `d` [`CaptureTree::Seq`] levels. The `Seq` at level
/// `j` (the root being level 0) holds one element per group of the pattern repetition at nesting
/// depth `j`, where a group is a single iteration of that repetition: group `k` of a repetition is
/// element `k` of its `Seq`.
///
/// A repetition that matched zero times is therefore an empty `Seq`, in the position its groups
/// would have taken.
///
/// For example, matching the pattern `$( $a:ident $( $b:ident )* );*` against
/// `x y z ; w ; v u` captures `a` at depth 1 and `b` at depth 2 as:
/// * `a`: `Seq([Leaf(x), Leaf(w), Leaf(v)])`.
/// * `b`: `Seq([Seq([Leaf(y), Leaf(z)]), Seq([]), Seq([Leaf(u)])])`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CaptureTree<'db> {
    /// A single captured value.
    Leaf(CapturedValue<'db>),
    /// The groups of a pattern repetition, in the order they matched in.
    Seq(Vec<CaptureTree<'db>>),
}

impl<'db> CaptureTree<'db> {
    /// The subtree holding the values of the group being expanded, given the index of that group in
    /// every `$()` expansion block currently entered, outermost first - one `Seq` level is
    /// descended per index.
    ///
    /// Reaching a [`Self::Leaf`] while indices remain ends the descent and returns the leaf: the
    /// placeholder repeats less deeply than the block it is used in, so its single value is
    /// broadcast to every group of the remaining blocks.
    ///
    /// `None` if an index is out of the range of the `Seq` it indexes, which means the placeholder
    /// was captured by a different pattern repetition than the block is iterating over - rejected
    /// at declaration time by [`SemanticDiagnosticKind::MacroPlaceholderRepDriverMismatch`].
    fn at(&self, group_indices: &[usize]) -> Option<&Self> {
        let mut node = self;
        for &index in group_indices {
            let Self::Seq(groups) = node else { return Some(node) };
            node = groups.get(index)?;
        }
        Some(node)
    }

    /// The groups of the `Seq` at nesting level `level` that is open for new content - reached by
    /// descending into the last group of every level above it.
    ///
    /// `None` if the tree has no such `Seq` - a `Leaf` is in the way, or a level has no group yet.
    /// Callers treat that as the name being used at conflicting nesting positions by the pattern,
    /// as a well formed tree has a `Seq` open at every level up to its leaves.
    fn open_groups_mut(&mut self, level: usize) -> Option<&mut Vec<Self>> {
        let mut node = self;
        for _ in 0..level {
            let Self::Seq(groups) = node else { return None };
            node = groups.last_mut()?;
        }
        let Self::Seq(groups) = node else { return None };
        Some(groups)
    }
}

/// Context used during macro pattern matching.
/// Tracks captured values and the active repetition scopes they were captured in.
#[derive(Default, Clone, Debug)]
struct MatcherContext<'db> {
    /// The captured values per macro parameter name, nested by the repetition structure of the
    /// pattern - see [`CaptureTree`]. Holds a tree for every placeholder of the matched pattern,
    /// including ones whose repetition matched zero times.
    capture_trees: CaptureTrees<'db>,

    /// The number of pattern repetitions currently being matched inside, which is the nesting
    /// depth of the values captured now.
    repetition_depth: usize,

    /// Counter for generating unique `RepetitionId`s.
    next_repetition_id: usize,

    /// Count how many times each repetition matched.
    repetition_match_counts: OrderedHashMap<RepetitionId, usize>,

    /// Store the repetition operator for each repetition.
    repetition_operators: OrderedHashMap<RepetitionId, ast::MacroRepetitionOperator<'db>>,
}

impl<'db> MatcherContext<'db> {
    /// Records `value` as the next capture of `name` in [`Self::capture_trees`].
    ///
    /// A name used at conflicting nesting positions cannot have all of its values in one tree; the
    /// value is dropped. Such a pattern is rejected at declaration time by
    /// [`SemanticDiagnosticKind::DuplicateMacroPlaceholder`], but its rule is still matched against
    /// every call before the error is honored, so the drop is reachable.
    fn record_capture(&mut self, name: SmolStrId<'db>, value: CapturedValue<'db>) {
        // The placeholder is nested in one pattern repetition per level of the current depth, so
        // its leaves are the elements of the `Seq` one level above its own nesting depth.
        let Some(level) = self.repetition_depth.checked_sub(1) else {
            // Outside any repetition the placeholder's tree is the leaf itself, so a pattern using
            // the name a second time there has nowhere to put its value.
            self.capture_trees.entry(name).or_insert(CaptureTree::Leaf(value));
            return;
        };
        // Finding the groups of an inner repetition where the leaf belongs means the pattern uses
        // the name at a deeper nesting position as well.
        let Some(groups) = self
            .capture_trees
            .get_mut(&name)
            .and_then(|tree| tree.open_groups_mut(level))
            .filter(|groups| !matches!(groups.last(), Some(CaptureTree::Seq(_))))
        else {
            return;
        };
        groups.push(CaptureTree::Leaf(value));
    }

    /// Opens a group in the [`Self::capture_trees`] of every placeholder nested in `repetition`,
    /// whose pattern elements are about to be matched.
    ///
    /// Called once per encounter of `repetition` while matching, that is once per group of the
    /// repetitions enclosing it, so that the groups of a placeholder line up with the groups of the
    /// repetition they were matched in - including when `repetition` matches zero times, in which
    /// case the group it opened is left as an empty `Seq`.
    ///
    /// Must be called before incrementing [`Self::repetition_depth`], as the group is opened at the
    /// nesting depth of the repetitions enclosing `repetition`.
    ///
    /// Finding a leaf of the placeholder where the group belongs means the pattern uses the name at
    /// a shallower nesting position as well - rejected at declaration time, but still matched; as
    /// in [`Self::record_capture`], the group is then not opened and the name's values under
    /// `repetition` are dropped.
    fn open_repetition_groups(
        &mut self,
        db: &'db dyn Database,
        repetition: &ast::MacroRepetition<'db>,
    ) {
        let depth = self.repetition_depth;
        let mut names = OrderedHashSet::default();
        collect_pattern_placeholder_names(db, repetition.elements(db).elements(db), &mut names);
        for name in names {
            let Some(level) = depth.checked_sub(1) else {
                // The outermost repetitions of a placeholder are the groups of its tree's root.
                self.capture_trees.entry(name).or_insert(CaptureTree::Seq(vec![]));
                continue;
            };
            let Some(groups) = self
                .capture_trees
                .get_mut(&name)
                .and_then(|tree| tree.open_groups_mut(level))
                .filter(|groups| !matches!(groups.last(), Some(CaptureTree::Leaf(_))))
            else {
                continue;
            };
            groups.push(CaptureTree::Seq(vec![]));
        }
    }
}

/// Reports the well-formedness defects of `elements`, the elements of a macro rule's pattern,
/// including the ones nested in its repetitions and subtrees.
///
/// `Err` if any was reported, for the caller to mark the rule with - a defective element makes the
/// rule's meaning unclear, so it must not expand.
fn check_pattern_elements<'db>(
    db: &'db dyn Database,
    elements: impl IntoIterator<Item = ast::MacroElement<'db>>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    let mut res = Ok(());
    for element in elements {
        match element {
            ast::MacroElement::Param(param) => {
                res = res.and(check_placeholder_name(db, &param, diagnostics));
            }
            ast::MacroElement::Repetition(repetition) => {
                res = res
                    .and(check_repetition_separator(db, &repetition, diagnostics))
                    .and(check_repetition_body(db, &repetition, diagnostics))
                    .and(check_pattern_elements(
                        db,
                        repetition.elements(db).elements(db),
                        diagnostics,
                    ));
            }
            ast::MacroElement::Subtree(subtree) => {
                res = res.and(check_pattern_elements(
                    db,
                    get_macro_elements(db, subtree.subtree(db)).elements(db),
                    diagnostics,
                ));
            }
            ast::MacroElement::Token(_) => {}
        }
    }
    res
}

/// Reports `param`, a placeholder of a macro rule's pattern, if it is named after one of the
/// `$defsite` / `$callsite` resolver modifiers.
///
/// Such a name is not a placeholder in an expansion - see [`extract_placeholder`] - so nothing can
/// ever read the value it captures.
fn check_placeholder_name<'db>(
    db: &'db dyn Database,
    param: &MacroParam<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    if extract_placeholder(db, param).is_some() {
        return Ok(());
    }
    let name = param.name(db).as_syntax_node().get_text_without_trivia(db);
    Err(diagnostics.report(
        param.stable_ptr(db).untyped(),
        SemanticDiagnosticKind::MacroPlaceholderNamedAfterResolverModifier(name),
    ))
}

/// Returns the separator token of the given repetition, if it has one.
///
/// Any single token may separate the groups of a repetition; it is matched in a call and emitted in
/// an expansion exactly as it is written.
fn repetition_separator<'db>(
    db: &'db dyn Database,
    repetition: &ast::MacroRepetition<'db>,
) -> Option<SyntaxNode<'db>> {
    match repetition.separator(db) {
        ast::OptionMacroRepetitionSeparator::MacroRepetitionSeparator(separator) => {
            Some(separator.token(db).as_syntax_node())
        }
        ast::OptionMacroRepetitionSeparator::Empty(_) => None,
    }
}

/// Reports `repetition`, a `$()` block of a macro rule, if it takes a separator while allowing at
/// most one group.
///
/// A separator only ever appears between two consecutive groups, so a `?` block can never have one.
fn check_repetition_separator<'db>(
    db: &'db dyn Database,
    repetition: &ast::MacroRepetition<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    let Some(separator) = repetition_separator(db, repetition) else {
        return Ok(());
    };
    if !matches!(repetition.operator(db), ast::MacroRepetitionOperator::ZeroOrOne(_)) {
        return Ok(());
    }
    Err(diagnostics.report(
        separator.stable_ptr(db),
        SemanticDiagnosticKind::MacroRepetitionSeparatorWithZeroOrOne,
    ))
}

/// Reports `repetition`, a `$()` block of a macro rule's pattern, if its body is empty.
///
/// Such a block consumes no input, so it matches zero times against every call - whatever its
/// operator, it contributes nothing to the pattern.
fn check_repetition_body<'db>(
    db: &'db dyn Database,
    repetition: &ast::MacroRepetition<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    if repetition.elements(db).elements(db).len() != 0 {
        return Ok(());
    }
    Err(diagnostics.report(
        repetition.stable_ptr(db).untyped(),
        SemanticDiagnosticKind::MacroRepetitionWithEmptyBody,
    ))
}

/// The texts a `$name:expr` placeholder of a macro rule's pattern may be followed by.
///
/// An `expr` placeholder captures by parsing the longest expression the call's tokens start with,
/// so whatever the pattern puts after it is only reachable when the expression grammar cannot
/// extend over it. These three are the tokens that can never continue an expression, which is the
/// same set `rustc` allows after its own `expr` fragment.
const EXPR_FOLLOW_SET: [&str; 3] = [",", ";", "=>"];

/// Something a macro rule's pattern can match right after a given position in it - a token of the
/// pattern, the opening delimiter of one of its subtrees, or a placeholder.
///
/// The pattern or a subtree of it running out is deliberately not one of these: it bounds whatever
/// the placeholder before it consumes, so every follow set allows it.
#[derive(Clone, Debug)]
struct Follower<'db> {
    /// The text of the pattern node, for reporting. A placeholder's text is its whole
    /// `$name:kind`, which no follow set holds, so a follow set is a plain text lookup.
    text: SmolStrId<'db>,
    ptr: SyntaxStablePtrId<'db>,
}

impl<'db> Follower<'db> {
    fn new(db: &'db dyn Database, node: SyntaxNode<'db>) -> Self {
        Self { text: node.get_text_without_trivia(db), ptr: node.stable_ptr(db) }
    }
}

/// The things `elements`, a run of a macro rule's pattern, can match first, where `outer` holds the
/// things the pattern can match right after all of `elements`.
///
/// A repetition contributes what its body can match first, and is matched over when it may match no
/// input at all - because its operator allows zero groups, or because its body can match nothing.
fn first_followers<'db>(
    db: &'db dyn Database,
    elements: &[ast::MacroElement<'db>],
    outer: &[Follower<'db>],
) -> Vec<Follower<'db>> {
    let mut res = vec![];
    for element in elements {
        match element {
            ast::MacroElement::Token(token) => {
                res.push(Follower::new(db, token.as_syntax_node()));
                return res;
            }
            ast::MacroElement::Param(param) => {
                res.push(Follower::new(db, param.as_syntax_node()));
                return res;
            }
            ast::MacroElement::Subtree(subtree) => {
                res.push(Follower::new(db, subtree_open_delimiter(db, &subtree.subtree(db))));
                return res;
            }
            ast::MacroElement::Repetition(repetition) => {
                let body_first =
                    first_followers(db, &repetition.elements(db).elements_vec(db), &[]);
                let may_match_nothing = body_first.is_empty()
                    || matches!(
                        repetition.operator(db),
                        ast::MacroRepetitionOperator::ZeroOrOne(_)
                            | ast::MacroRepetitionOperator::ZeroOrMore(_)
                    );
                res.extend(body_first);
                if !may_match_nothing {
                    return res;
                }
            }
        }
    }
    res.extend(outer.iter().cloned());
    res
}

/// Reports every `$name:expr` placeholder of `elements`, a run of a macro rule's pattern, that the
/// pattern can follow with something outside [`EXPR_FOLLOW_SET`], including the ones nested in its
/// repetitions and subtrees.
///
/// `outer` holds the things the pattern can match right after all of `elements`.
///
/// `Err` if any was reported, for the caller to mark the rule with - the placeholder would swallow
/// the tokens the pattern puts after it, so the rule must not expand.
fn check_expr_follow_set<'db>(
    db: &'db dyn Database,
    elements: &[ast::MacroElement<'db>],
    outer: &[Follower<'db>],
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    let mut res = Ok(());
    for (index, element) in elements.iter().enumerate() {
        // What the pattern can match right after this element - also what it can match right after
        // the last element of this element's body, when it has one.
        let after = || first_followers(db, &elements[index + 1..], outer);
        match element {
            ast::MacroElement::Param(param) => {
                if !is_expr_param(db, param) {
                    continue;
                }
                let name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                for follower in after() {
                    if EXPR_FOLLOW_SET.contains(&follower.text.long(db).as_str()) {
                        continue;
                    }
                    res = Err(diagnostics.report(
                        follower.ptr,
                        SemanticDiagnosticKind::MacroExprPlaceholderFollower {
                            name,
                            follower: follower.text,
                        },
                    ));
                }
            }
            ast::MacroElement::Repetition(repetition) => {
                // The end of a group is followed either by the separator, when another group comes
                // after it, or by whatever comes after the repetition itself.
                let mut body_outer = after();
                if let Some(separator) = repetition_separator(db, repetition) {
                    body_outer.push(Follower::new(db, separator));
                }
                res = res.and(check_expr_follow_set(
                    db,
                    &repetition.elements(db).elements_vec(db),
                    &body_outer,
                    diagnostics,
                ));
            }
            ast::MacroElement::Subtree(subtree) => {
                // The subtree's closing delimiter bounds its last element, so nothing of the
                // pattern outside the subtree can follow it.
                res = res.and(check_expr_follow_set(
                    db,
                    &get_macro_elements(db, subtree.subtree(db)).elements_vec(db),
                    &[],
                    diagnostics,
                ));
            }
            ast::MacroElement::Token(_) => {}
        }
    }
    res
}

/// Whether `param`, a placeholder of a macro rule's pattern, captures an expression.
fn is_expr_param<'db>(db: &'db dyn Database, param: &MacroParam<'db>) -> bool {
    let ast::OptionParamKind::ParamKind(kind) = param.kind(db) else { return false };
    matches!(kind.kind(db), ast::MacroParamKind::Expr(_))
}

/// The opening delimiter of a macro rule pattern's subtree.
fn subtree_open_delimiter<'db>(
    db: &'db dyn Database,
    subtree: &ast::WrappedMacro<'db>,
) -> SyntaxNode<'db> {
    match subtree {
        ast::WrappedMacro::Parenthesized(inner) => inner.lparen(db).as_syntax_node(),
        ast::WrappedMacro::Braced(inner) => inner.lbrace(db).as_syntax_node(),
        ast::WrappedMacro::Bracketed(inner) => inner.lbrack(db).as_syntax_node(),
    }
}

/// Collects the names of all the placeholders in the given pattern elements, including the ones
/// nested in inner repetitions and subtrees.
fn collect_pattern_placeholder_names<'db>(
    db: &'db dyn Database,
    elements: impl IntoIterator<Item = ast::MacroElement<'db>>,
    names: &mut OrderedHashSet<SmolStrId<'db>>,
) {
    for element in elements {
        match element {
            ast::MacroElement::Param(param) => {
                names.insert(param.name(db).as_syntax_node().get_text_without_trivia(db));
            }
            ast::MacroElement::Repetition(repetition) => {
                collect_pattern_placeholder_names(db, repetition.elements(db).elements(db), names);
            }
            ast::MacroElement::Subtree(subtree) => {
                collect_pattern_placeholder_names(
                    db,
                    get_macro_elements(db, subtree.subtree(db)).elements(db),
                    names,
                );
            }
            ast::MacroElement::Token(_) => {}
        }
    }
}

/// The semantic data for a macro declaration.
#[derive(Debug, Clone, PartialEq, Eq, salsa::SalsaValue)]
pub struct MacroDeclarationData<'db> {
    rules: Vec<MacroRuleData<'db>>,
    attributes: Vec<Attribute<'db>>,
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// The semantic data for a single macro rule in a macro declaration.
#[derive(Debug, Clone, PartialEq, Eq, salsa::SalsaValue)]
pub struct MacroRuleData<'db> {
    pub pattern: ast::WrappedMacro<'db>,
    pub expansion: ast::MacroElements<'db>,
    /// Set to `Err` when this rule has semantic errors (e.g., undefined placeholders).
    /// Callers must skip expansion when this is `Err`.
    pub err: Maybe<()>,
}

/// The possible kinds of placeholders in a macro rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceholderKind {
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
    /// Whether the expansion must parenthesize the value to keep it a single operand - see
    /// `ExpansionContext::expand_placeholder`. True when the value's top level is an operator
    /// (e.g. `1 + 2`, `-x`) or an operator-adjacent form (a closure, a block, `if`/`match`/loop
    /// expressions); false for atoms (paths, literals, tuples, calls), which parse the same either
    /// way and may legally reach non-expression splice positions where parentheses do not parse.
    pub needs_parens: bool,
}

/// Implementation of [MacroDeclarationSemantic::priv_macro_declaration_data].
fn priv_macro_declaration_data<'db>(
    db: &'db dyn Database,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<MacroDeclarationData<'db>> {
    let module_id = macro_declaration_id.parent_module(db);
    let mut diagnostics = SemanticDiagnostics::new(module_id);

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

    let mut rules = vec![];
    for rule_syntax in macro_declaration_syntax.rules(db).elements(db) {
        let pattern = rule_syntax.lhs(db);
        let expansion = rule_syntax.rhs(db).elements(db);
        let pattern_elements = get_macro_elements(db, pattern.clone());
        let mut placeholders = PatternPlaceholders::default();
        placeholders.collect(db, pattern_elements.elements(db));
        // A pattern with a parse error may hold several nameless placeholders, which are not
        // actually reusing a name - such a rule is dropped below anyway.
        let pattern_is_parsed =
            !pattern.as_syntax_node().descendants(db).any(|node| node.kind(db).is_missing());
        let mut rule_err = Ok(());
        if pattern_is_parsed {
            for (name, ptr) in placeholders.reused_names.iter() {
                rule_err = Err(diagnostics
                    .report(*ptr, SemanticDiagnosticKind::DuplicateMacroPlaceholder(*name)));
            }
            rule_err = rule_err
                .and(check_pattern_elements(db, pattern_elements.elements(db), &mut diagnostics))
                .and(check_expr_follow_set(
                    db,
                    &pattern_elements.elements_vec(db),
                    &[],
                    &mut diagnostics,
                ));
        }
        // The expansion is checked against the nesting depth every placeholder is captured at,
        // which a reused name leaves ambiguous - checking it then reports the ambiguity as
        // a defect of the expansion, which it is not.
        if placeholders.reused_names.is_empty() {
            let mut ctx = ExpansionCheckCtx {
                db,
                known_path: &[],
                curr_rep_depth: 0,
                placeholder_paths: &placeholders.paths,
                diagnostics: &mut diagnostics,
                rule_err: Ok(()),
                in_non_repeating_block: false,
            };
            ctx.check_node(expansion.as_syntax_node());
            rule_err = rule_err.and(ctx.rule_err);
        }
        if !pattern_is_parsed {
            // The pattern cannot be matched as written, so the rule is dropped - keeping it would
            // let it match on the strength of the nodes that did parse, shadowing the rules after
            // it. It is reported so that a call written for it points at the parse error rather
            // than failing with a bare "no matching rule".
            diagnostics.report(
                pattern.stable_ptr(db).untyped(),
                SemanticDiagnosticKind::MacroRuleWithUnparsablePattern,
            );
            continue;
        }
        rules.push(MacroRuleData { pattern, expansion, err: rule_err });
    }
    let resolver_data = Arc::new(resolver.data);
    Ok(MacroDeclarationData { diagnostics: diagnostics.build(), attributes, resolver_data, rules })
}

/// Query implementation of [MacroDeclarationSemantic::priv_macro_declaration_data].
#[salsa::tracked(returns(clone))]
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

/// The placeholders a macro rule's pattern defines, collected by [`Self::collect`].
#[derive(Default)]
struct PatternPlaceholders<'db> {
    /// The path of every placeholder the pattern defines: the IDs of the `$()` repetitions it is
    /// nested in, outermost first. A unique ID is assigned to every repetition of the pattern, in
    /// left-to-right DFS order. Used by [`ExpansionCheckCtx`] to validate the expansion.
    ///
    /// A name the pattern reuses is mapped to the path of its last use, so its path is meaningless
    /// once the name is in [`Self::reused_names`].
    paths: OrderedHashMap<SmolStrId<'db>, Vec<usize>>,
    /// Every name the pattern uses for more than one placeholder, pointing at its second use - the
    /// one making the name ambiguous.
    reused_names: OrderedHashMap<SmolStrId<'db>, SyntaxStablePtrId<'db>>,
    /// The path of the pattern elements being traversed.
    current_path: Vec<usize>,
    /// Counter for assigning repetition IDs.
    next_rep_id: usize,
}

impl<'db> PatternPlaceholders<'db> {
    /// Collects the placeholders of `elements`, the elements of a macro rule's pattern, including
    /// the ones nested in its repetitions and subtrees.
    fn collect(
        &mut self,
        db: &'db dyn Database,
        elements: impl IntoIterator<Item = ast::MacroElement<'db>>,
    ) {
        for element in elements {
            match element {
                ast::MacroElement::Param(param) => {
                    let name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                    if self.paths.insert(name, self.current_path.clone()).is_some() {
                        self.reused_names.entry(name).or_insert(param.stable_ptr(db).untyped());
                    }
                }
                ast::MacroElement::Repetition(rep) => {
                    let rep_id = self.next_rep_id;
                    self.next_rep_id += 1;
                    self.current_path.push(rep_id);
                    self.collect(db, rep.elements(db).elements(db));
                    assert_eq!(self.current_path.pop(), Some(rep_id));
                }
                ast::MacroElement::Subtree(subtree) => {
                    self.collect(db, get_macro_elements(db, subtree.subtree(db)).elements(db));
                }
                ast::MacroElement::Token(_) => {}
            }
        }
    }
}

/// Whether `block`, a `$()` block of a macro expansion nested in `enclosing_depth` other such
/// blocks, holds a placeholder that can drive its repetitions - one whose pattern repetition depth
/// is greater than `enclosing_depth`. Placeholders nested deeper in `block` count, as they are
/// consumed by the enclosing repetitions as well.
///
/// A placeholder undefined in the pattern counts as driving: it is reported on its own, and
/// reporting the block too would double up on a single defect.
fn has_driving_placeholder<'db>(
    db: &'db dyn Database,
    block: SyntaxNode<'db>,
    placeholder_paths: &OrderedHashMap<SmolStrId<'db>, Vec<usize>>,
    enclosing_depth: usize,
) -> bool {
    block
        .descendants(db)
        .filter_map(|node| MacroParam::cast(db, node))
        .filter_map(|param| extract_placeholder(db, &param))
        .any(|name| placeholder_paths.get(&name).is_none_or(|path| path.len() > enclosing_depth))
}

/// Context for validating placeholder usage in a macro rule's expansion.
struct ExpansionCheckCtx<'db, 'a> {
    db: &'db dyn Database,
    /// Maps each placeholder name to its pattern path: the sequence of repetition IDs
    /// (outermost to innermost) of the `$()` blocks it is nested in within the pattern.
    placeholder_paths: &'a OrderedHashMap<SmolStrId<'db>, Vec<usize>>,
    /// Number of `$()` expansion blocks currently entered. Used for depth checks
    /// and to trim `known_path` when exiting a block.
    curr_rep_depth: usize,
    /// The deepest placeholder path seen so far within the current expansion scope.
    /// New placeholders at the same depth are validated against this prefix.
    /// Invariant: `known_path.len() <= curr_rep_depth`.
    /// Trimmed to `curr_rep_depth` on `$()` exit so sibling blocks start fresh.
    known_path: &'a [usize],
    diagnostics: &'a mut SemanticDiagnostics<'db>,
    /// `Err` if any diagnostic has been emitted; callers skip expansion when set.
    rule_err: Maybe<()>,
    /// Whether an enclosing `$()` block was already reported as non-repeating. A block
    /// nested in such a block fails for the same reason, so only the outermost one is reported.
    in_non_repeating_block: bool,
}

impl<'db> ExpansionCheckCtx<'db, '_> {
    /// Validates placeholder usage by recursively traversing `node`.
    ///
    /// Four kinds of errors are reported:
    /// * Depth mismatch: placeholder used at fewer expansion levels than its pattern depth.
    /// * Context mismatch: placeholder from a different repetition than the driving one.
    /// * Non-repeating block: a `$()` block holding no placeholder that repeats at its depth, so
    ///   there is nothing to drive it.
    /// * Separator on a `?` block: see [`check_repetition_separator`].
    fn check_node(&mut self, node: SyntaxNode<'db>) {
        let db = self.db;
        if let Some(param) = MacroParam::cast(db, node) {
            if let Some(name) = extract_placeholder(db, &param) {
                let ptr = param.stable_ptr(db).untyped();
                match self.placeholder_paths.get(&name) {
                    None => {
                        self.rule_err = Err(self
                            .diagnostics
                            .report(ptr, SemanticDiagnosticKind::UndefinedMacroPlaceholder(name)));
                    }
                    Some(path) => {
                        if path.len() > self.curr_rep_depth {
                            self.rule_err = Err(self.diagnostics.report(
                                ptr,
                                SemanticDiagnosticKind::MacroPlaceholderRepDepthMismatch {
                                    name,
                                    required: path.len(),
                                    actual: self.curr_rep_depth,
                                },
                            ));
                        } else {
                            let cmp_size = path.len().min(self.known_path.len());
                            if path[..cmp_size] != self.known_path[..cmp_size] {
                                self.rule_err = Err(self.diagnostics.report(
                                    ptr,
                                    SemanticDiagnosticKind::MacroPlaceholderRepDriverMismatch(name),
                                ));
                            } else if path.len() > self.known_path.len() {
                                self.known_path = path;
                            }
                        }
                    }
                }
            }
            return;
        }

        if let Some(repetition) = ast::MacroRepetition::cast(db, node) {
            self.rule_err =
                self.rule_err.and(check_repetition_separator(db, &repetition, self.diagnostics));
            let outer_in_non_repeating_block = self.in_non_repeating_block;
            if !outer_in_non_repeating_block
                && !has_driving_placeholder(db, node, self.placeholder_paths, self.curr_rep_depth)
            {
                self.rule_err = Err(self.diagnostics.report(
                    repetition.stable_ptr(db).untyped(),
                    SemanticDiagnosticKind::MacroRepetitionWithoutRepeatingPlaceholder,
                ));
                self.in_non_repeating_block = true;
            }
            self.curr_rep_depth += 1;
            for element in repetition.elements(db).elements(db) {
                self.check_node(element.as_syntax_node());
            }
            self.curr_rep_depth -= 1;
            self.in_non_repeating_block = outer_in_non_repeating_block;
            if self.curr_rep_depth < self.known_path.len() {
                // Trimming `self.known_path` so it won't leak between different repetitions.
                self.known_path = &self.known_path[..self.curr_rep_depth];
            }
        } else if !node.kind(db).is_terminal() {
            for child in node.get_children(db).iter() {
                self.check_node(*child);
            }
        }
    }
}

/// Given a macro declaration and an input token tree, checks if the input the given rule, and
/// returns the captured params if it does.
pub fn is_macro_rule_match<'db>(
    db: &'db dyn Database,
    rule: &MacroRuleData<'db>,
    input: &ast::TokenTreeNode<'db>,
) -> Option<CaptureTrees<'db>> {
    let mut ctx = MatcherContext::default();

    let matcher_elements = get_macro_elements(db, rule.pattern.clone());
    let mut input_iter = match input.subtree(db) {
        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db),
        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db),
        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db),
        ast::WrappedTokenTree::Missing(_) => return None,
    }
    .elements(db)
    .peekable();
    is_macro_rule_match_ex(db, matcher_elements, &mut input_iter, &mut ctx, true)?;
    if !validate_repetition_operator_constraints(&ctx) {
        return None;
    }
    Some(ctx.capture_trees)
}

/// Helper function for [expand_macro_rule].
/// Traverses the macro expansion and replaces the placeholders with the provided values,
/// while collecting the result in `res_buffer`.
/// Returns `Some(true)` if the match succeeded and some input was consumed,
/// `Some(false)` if the match succeeded but no input was consumed (empty match),
/// and `None` if the match failed.
fn is_macro_rule_match_ex<'db>(
    db: &'db dyn Database,
    matcher_elements: ast::MacroElements<'db>,
    input_iter: &mut std::iter::Peekable<
        impl DoubleEndedIterator<Item = ast::TokenTree<'db>> + Clone,
    >,
    ctx: &mut MatcherContext<'db>,
    consume_all_input: bool,
) -> Option<bool> {
    let mut advanced = false;
    for matcher_element in matcher_elements.elements(db) {
        match matcher_element {
            ast::MacroElement::Token(matcher_token) => {
                advanced = true;
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
                advanced = true;
                let ast::OptionParamKind::ParamKind(param_kind) = param.kind(db) else {
                    return None;
                };
                let placeholder_kind: PlaceholderKind = param_kind.kind(db).into();
                let placeholder_name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                match placeholder_kind {
                    PlaceholderKind::Identifier => {
                        let input_token = input_iter.next()?;
                        let captured_text = match &input_token {
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
                        ctx.record_capture(
                            placeholder_name,
                            CapturedValue {
                                text: captured_text,
                                stable_ptr: input_token.stable_ptr(db).untyped(),
                                needs_parens: false,
                            },
                        );
                        continue;
                    }
                    PlaceholderKind::Expr => {
                        let peek_token = input_iter.peek().cloned()?;
                        let file_id = peek_token.as_syntax_node().stable_ptr(db).file_id(db);
                        // Advances `input_iter` past the tokens of the captured expression, and
                        // rejects the rule when the input does not start with one.
                        let expr_node = as_expr_macro_token_tree(input_iter, file_id, db)?;
                        ctx.record_capture(
                            placeholder_name,
                            CapturedValue {
                                // The trivia around the expression belongs to the call, not to the
                                // expression - the expansion spaces the value by its own trivia.
                                text: expr_node
                                    .as_syntax_node()
                                    .get_text_without_trivia(db)
                                    .to_string(db),
                                stable_ptr: peek_token.stable_ptr(db).untyped(),
                                needs_parens: match &expr_node {
                                    // `@` forms types as well as expressions, and a snapshot
                                    // type reaches type-position splices, where parentheses
                                    // do not parse. It binds tighter than every binary
                                    // operator, so a bare splice is also safe in expression
                                    // positions - up to a postfix operator the expansion
                                    // writes right after the placeholder, which the author
                                    // can parenthesize explicitly.
                                    ast::Expr::Unary(unary) => {
                                        !matches!(unary.op(db), ast::UnaryOperator::At(_))
                                    }
                                    ast::Expr::Binary(_)
                                    | ast::Expr::Closure(_)
                                    | ast::Expr::Block(_)
                                    | ast::Expr::Match(_)
                                    | ast::Expr::If(_)
                                    | ast::Expr::Loop(_)
                                    | ast::Expr::While(_)
                                    | ast::Expr::For(_) => true,
                                    _ => false,
                                },
                            },
                        );
                        continue;
                    }
                }
            }
            ast::MacroElement::Subtree(matcher_subtree) => {
                advanced = true;
                let ast::TokenTree::Subtree(input_subtree) = input_iter.next()? else {
                    return None;
                };
                // The delimiters of a subtree are part of the pattern: a subtree of the input
                // whose delimiters are of another kind does not match, so the rule does not - it
                // is not an error, as another rule may match the call.
                let (inner_elements, inner_input_tokens) =
                    match (matcher_subtree.subtree(db), input_subtree.subtree(db)) {
                        (
                            ast::WrappedMacro::Parenthesized(matcher),
                            ast::WrappedTokenTree::Parenthesized(input),
                        ) => (matcher.elements(db), input.tokens(db)),
                        (
                            ast::WrappedMacro::Braced(matcher),
                            ast::WrappedTokenTree::Braced(input),
                        ) => (matcher.elements(db), input.tokens(db)),
                        (
                            ast::WrappedMacro::Bracketed(matcher),
                            ast::WrappedTokenTree::Bracketed(input),
                        ) => (matcher.elements(db), input.tokens(db)),
                        _ => return None,
                    };
                let mut inner_input_iter = inner_input_tokens.elements(db).peekable();
                is_macro_rule_match_ex(db, inner_elements, &mut inner_input_iter, ctx, true)?;
                continue;
            }
            ast::MacroElement::Repetition(repetition) => {
                let rep_id = RepetitionId(ctx.next_repetition_id);
                ctx.next_repetition_id += 1;
                ctx.open_repetition_groups(db, &repetition);
                ctx.repetition_depth += 1;
                let elements = repetition.elements(db);
                let operator = repetition.operator(db);
                let expected_separator = repetition_separator(db, &repetition)
                    .map(|sep| sep.get_text_without_trivia(db));
                let mut match_count = 0;
                loop {
                    let mut inner_ctx = ctx.clone();
                    let mut temp_iter = input_iter.clone();
                    // A separator only ever stands *between* two groups, so from the second group
                    // on it has to be consumed before the group - and only together with it. A
                    // separator that no group follows is not part of the repetition, and is left
                    // in the input for whatever comes after it to match, exactly as rustc's
                    // `macro_rules!` does.
                    if match_count > 0
                        && let Some(expected_sep) = &expected_separator
                    {
                        let Some(ast::TokenTree::Token(token_leaf)) = temp_iter.peek() else {
                            break;
                        };
                        if token_leaf.as_syntax_node().get_text_without_trivia(db) != *expected_sep
                        {
                            break;
                        }
                        temp_iter.next();
                    }
                    let Some(true) = is_macro_rule_match_ex(
                        db,
                        elements.clone(),
                        &mut temp_iter,
                        &mut inner_ctx,
                        false,
                    ) else {
                        break;
                    };
                    advanced = true;
                    *ctx = inner_ctx;
                    *input_iter = temp_iter;
                    match_count += 1;
                }
                ctx.repetition_match_counts.insert(rep_id, match_count);
                ctx.repetition_operators.insert(rep_id, operator.clone());
                ctx.repetition_depth -= 1;
                continue;
            }
        }
    }

    if consume_all_input && input_iter.next().is_some() {
        return None;
    }
    Some(advanced)
}

fn validate_repetition_operator_constraints(ctx: &MatcherContext<'_>) -> bool {
    for (&rep_id, &count) in ctx.repetition_match_counts.iter() {
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

/// The reason the expansion of a macro rule could not be performed.
///
/// No user-writable program currently reaches [`MacroExpansionFailure::MissingRepetitionDriver`],
/// [`MacroExpansionFailure::ConflictingRepetitionDrivers`] or
/// [`MacroExpansionFailure::MissingCapture`]: expansion only runs on rules that have passed the
/// declaration-time checks, and each of the three is foreclosed by one of them -
/// [`SemanticDiagnosticKind::UndefinedMacroPlaceholder`],
/// [`SemanticDiagnosticKind::MacroPlaceholderRepDepthMismatch`],
/// [`SemanticDiagnosticKind::MacroPlaceholderRepDriverMismatch`],
/// [`SemanticDiagnosticKind::MacroRepetitionWithoutRepeatingPlaceholder`] and
/// [`SemanticDiagnosticKind::DuplicateMacroPlaceholder`] (see the notes at the construction
/// sites). They are kept as a backstop, so that a rule a future matcher or checker change lets
/// through fails with a diagnostic instead of expanding to something arbitrary.
/// [`MacroExpansionFailure::EmptyPlusRepetition`] is reachable by design: the number of groups is
/// only known once a call is matched.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
pub enum MacroExpansionFailure<'db> {
    /// No placeholder of a `$( ... )` block in the expansion repeats at the block's depth, so the
    /// number of groups to expand it over is unknown.
    MissingRepetitionDriver,
    /// The placeholders of a `$( ... )` block in the expansion disagree on the number of groups to
    /// expand it over.
    ConflictingRepetitionDrivers,
    /// A placeholder in the expansion has no captured value for the group being expanded.
    MissingCapture(SmolStrId<'db>),
    /// A `$( ... )+` block in the expansion expands over zero groups, violating the at-least-once
    /// promise of its `+` operator.
    EmptyPlusRepetition,
}

/// An error preventing the expansion of a macro rule, to be reported by the caller performing the
/// expansion.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MacroExpansionError<'db> {
    /// The node in the rule's expansion that could not be expanded.
    stable_ptr: SyntaxStablePtrId<'db>,
    /// The reason the expansion failed.
    failure: MacroExpansionFailure<'db>,
}
impl<'db> MacroExpansionError<'db> {
    /// Reports the error as a semantic diagnostic on the node that could not be expanded.
    pub fn report(self, diagnostics: &mut SemanticDiagnostics<'db>) -> DiagnosticAdded {
        diagnostics
            .report(self.stable_ptr, SemanticDiagnosticKind::MacroExpansionFailed(self.failure))
    }
}

/// Traverse the macro expansion and replace the placeholders with the provided values, creates a
/// string representation of the expanded macro.
///
/// Returns an error if the expansion cannot be performed, for the caller to report.
pub fn expand_macro_rule<'db>(
    db: &'db dyn Database,
    rule: &MacroRuleData<'db>,
    capture_trees: &CaptureTrees<'db>,
) -> Result<MacroExpansionResult, MacroExpansionError<'db>> {
    let mut ctx = ExpansionContext {
        db,
        capture_trees,
        group_indices: vec![],
        res_buffer: String::new(),
        code_mappings: vec![],
    };
    ctx.expand_node(rule.expansion.as_syntax_node())?;
    Ok(MacroExpansionResult {
        text: ctx.res_buffer.into(),
        code_mappings: ctx.code_mappings.into(),
    })
}

/// The state of an in-progress expansion of a macro rule, performed by [`expand_macro_rule`].
struct ExpansionContext<'db, 'a> {
    db: &'db dyn Database,
    /// The values the rule's pattern captured from the call.
    capture_trees: &'a CaptureTrees<'db>,
    /// The index of the group being expanded, one per `$()` expansion block currently entered,
    /// outermost first. Selects the captures every placeholder is expanded to - see
    /// [`CaptureTree::at`].
    group_indices: Vec<usize>,
    /// The expansion so far.
    res_buffer: String,
    /// The origin of every placeholder expanded into [`Self::res_buffer`].
    code_mappings: Vec<CodeMapping>,
}

impl<'db> ExpansionContext<'db, '_> {
    /// Expands `node` of a macro rule's expansion, appending it to [`Self::res_buffer`].
    fn expand_node(&mut self, node: SyntaxNode<'db>) -> Result<(), MacroExpansionError<'db>> {
        let db = self.db;
        match node.kind(db) {
            SyntaxKind::MacroParam => {
                let param = MacroParam::from_syntax_node(db, node);
                // `$defsite` / `$callsite` are not placeholders; they are emitted as written, by
                // the fallthrough below.
                if let Some(name) = extract_placeholder(db, &param) {
                    self.expand_placeholder(&param, name)?;
                    self.push_trailing_trivia(node);
                    return Ok(());
                }
            }
            SyntaxKind::MacroRepetition => {
                let repetition = ast::MacroRepetition::from_syntax_node(db, node);
                self.expand_repetition(&repetition)?;
                self.push_trailing_trivia(node);
                return Ok(());
            }
            _ => {}
        }
        if node.kind(db).is_terminal() {
            self.res_buffer.push_str(node.get_text(db));
            return Ok(());
        }
        for child in node.get_children(db).iter() {
            self.expand_node(*child)?;
        }
        Ok(())
    }

    /// Appends the trivia `node` carries after its last token to [`Self::res_buffer`].
    ///
    /// A node whose text is replaced by the expansion - a placeholder, a `$()` block - is not
    /// emitted by [`Self::expand_node`]'s terminal case, so the trivia it carries has to be
    /// emitted separately. It is the whitespace the rule's author wrote between that node and the
    /// one after it, and dropping it glues their tokens together.
    ///
    /// The leading trivia of such a node is deliberately not emitted. The resolver maps a path of
    /// the expanded code back to the call by the offset of its syntax node, which includes that
    /// node's leading trivia, and it needs that offset to land inside the [`CodeMapping`] of the
    /// value the path came from. Trivia emitted before a value puts the offset before its mapping,
    /// so the value resolves at the definition site instead. Nothing is glued by dropping it:
    /// whitespace before a node is the trailing trivia of the token before it unless a newline
    /// separates them, and the lexer puts that newline in the same trailing trivia, so only
    /// indentation is lost.
    fn push_trailing_trivia(&mut self, node: SyntaxNode<'db>) {
        let db = self.db;
        let span = TextSpan::new(node.span_without_trivia(db).end, node.span(db).end);
        self.res_buffer.push_str(node.get_text_of_span(db, span));
    }

    /// Expands the placeholder `name`, used by `param`, to the value it captured in the group being
    /// expanded.
    ///
    /// The value is emitted without the trivia surrounding it in the call - the spacing of the
    /// expansion is the one the rule's author wrote, emitted by [`Self::push_trailing_trivia`].
    ///
    /// A value whose top level is an operator - see [`CapturedValue::needs_parens`] - is wrapped
    /// in parentheses, as it is spliced as text into an expansion that was written around a single
    /// operand. Without them the operators of the expansion bind into the value:
    /// `macro neg { ($x:expr) => { 0 - $x }; }` on `neg!(1 + 2)` would produce `0 - 1 + 2`, which
    /// is `1` rather than `-3`. `rustc` has no such hazard because an `expr` fragment is one AST
    /// node there, and the parentheses are how the same atomicity is spelled in text.
    ///
    /// An atomic value - a path, a literal, a tuple, a call - is spliced as written. It has no
    /// top-level operator for the expansion's operators to bind into, so the parentheses would
    /// change nothing about how it parses - and atoms are the expression shapes that reach the
    /// non-expression positions a rule can splice a capture into, where parentheses do not parse:
    /// `use $path;` on a path capture, `let x: $t = ...;` on a tuple capture. Cairo has no `path`
    /// or `ty` placeholder kinds, so `expr` is how a rule captures these.
    fn expand_placeholder(
        &mut self,
        param: &MacroParam<'db>,
        name: SmolStrId<'db>,
    ) -> Result<(), MacroExpansionError<'db>> {
        let db = self.db;
        // The trees are borrowed out of `self` so that appending to the buffers below may borrow it
        // mutably.
        let capture_trees = self.capture_trees;
        let Some(CaptureTree::Leaf(value)) =
            capture_trees.get(&name).and_then(|tree| tree.at(&self.group_indices))
        else {
            // Either the placeholder is not one of the pattern's - rejected at declaration time by
            // `UndefinedMacroPlaceholder` - or its captures are nested deeper than the blocks it is
            // used in, rejected there by `MacroPlaceholderRepDepthMismatch`. An out-of-range group
            // index cannot land here: every index was produced by `group_count` on an enclosing
            // block, which scanned this placeholder's tree (the placeholder is a descendant of
            // that block) before descending into it.
            return Err(MacroExpansionError {
                stable_ptr: param.stable_ptr(db).untyped(),
                failure: MacroExpansionFailure::MissingCapture(name),
            });
        };
        // The mapping spans the parentheses along with the value. Everything the expanded code
        // makes of the value - a path resolved through it, another macro call capturing it whole -
        // is looked up by an offset that must land inside the mapping, and the offset of a node
        // starting at the value includes the parenthesis in front of it.
        let start = TextWidth::from_str(&self.res_buffer).as_offset();
        let parenthesize = value.needs_parens;
        if parenthesize {
            self.res_buffer.push('(');
        }
        self.res_buffer.push_str(&value.text);
        if parenthesize {
            self.res_buffer.push(')');
        }
        let end = TextWidth::from_str(&self.res_buffer).as_offset();
        self.code_mappings.push(CodeMapping {
            span: TextSpan::new(start, end),
            origin: CodeOrigin::Span(value.stable_ptr.lookup(db).span_without_trivia(db)),
        });
        Ok(())
    }

    /// Expands `repetition`, a `$()` block of a macro rule's expansion, once per group of the
    /// captures driving it, emitting its separator between consecutive groups.
    fn expand_repetition(
        &mut self,
        repetition: &ast::MacroRepetition<'db>,
    ) -> Result<(), MacroExpansionError<'db>> {
        let db = self.db;
        let group_count = self.group_count(repetition)?;
        if group_count == 0
            && matches!(repetition.operator(db), ast::MacroRepetitionOperator::OneOrMore(_))
        {
            // `rustc` errors here as well ("this must repeat at least once"): a `+` block promises
            // at least one repetition, and only the matched call determines the group count.
            return Err(MacroExpansionError {
                stable_ptr: repetition.stable_ptr(db).untyped(),
                failure: MacroExpansionFailure::EmptyPlusRepetition,
            });
        }
        let elements = repetition.elements(db);
        for index in 0..group_count {
            self.group_indices.push(index);
            let expanded = elements
                .elements(db)
                .try_for_each(|element| self.expand_node(element.as_syntax_node()));
            self.group_indices.pop();
            expanded?;
            if index + 1 < group_count
                && let Some(sep) = repetition_separator(db, repetition)
            {
                // The separator's text carries its trailing trivia but not its leading trivia -
                // that sits on the repetition's closing `)`, which is not emitted. An identifier
                // or keyword separator would then lex together with the text before or after it,
                // so a space is inserted wherever the adjacent characters would form one token.
                let glues = |c: Option<char>| c.is_some_and(|c| c.is_alphanumeric() || c == '_');
                let sep_text = sep.get_text(db);
                if glues(self.res_buffer.chars().next_back()) && glues(sep_text.chars().next()) {
                    self.res_buffer.push(' ');
                }
                self.res_buffer.push_str(sep_text);
                if glues(self.res_buffer.chars().next_back()) {
                    self.res_buffer.push(' ');
                }
            }
        }
        Ok(())
    }

    /// The number of groups `repetition` is expanded over.
    ///
    /// A placeholder drives `repetition` if its captures still repeat at its depth -
    /// [`CaptureTree::at`] reaches a [`CaptureTree::Seq`]; one reaching a [`CaptureTree::Leaf`] is
    /// broadcast to every group instead. The declaration-time checks guarantee the drivers exist
    /// and agree, but both are verified here rather than assumed, so that a pattern the checks do
    /// not cover fails with a diagnostic instead of expanding to something arbitrary.
    fn group_count(
        &self,
        repetition: &ast::MacroRepetition<'db>,
    ) -> Result<usize, MacroExpansionError<'db>> {
        let db = self.db;
        let mut group_count: Option<usize> = None;
        let names = repetition
            .as_syntax_node()
            .descendants(db)
            .filter_map(|node| MacroParam::cast(db, node))
            .filter_map(|param| extract_placeholder(db, &param));
        let error = |failure| MacroExpansionError {
            stable_ptr: repetition.stable_ptr(db).untyped(),
            failure,
        };
        for name in names {
            let Some(CaptureTree::Seq(groups)) =
                self.capture_trees.get(&name).and_then(|tree| tree.at(&self.group_indices))
            else {
                continue;
            };
            if group_count.is_some_and(|count| count != groups.len()) {
                return Err(error(MacroExpansionFailure::ConflictingRepetitionDrivers));
            }
            group_count = Some(groups.len());
        }
        group_count.ok_or_else(|| error(MacroExpansionFailure::MissingRepetitionDriver))
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
#[salsa::tracked(returns(clone))]
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
#[salsa::tracked(returns(clone))]
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
#[salsa::tracked(returns(clone))]
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
#[salsa::tracked(returns(clone))]
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
