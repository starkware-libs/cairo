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

/// The values captured for a single placeholder, nested by the repetition structure of the pattern
/// that matched them: a placeholder nested in `d` `$()` repetitions has each value as a `Leaf`
/// under `d` `Seq` levels, the `Seq` at level `j` holding one element per group of the repetition
/// at depth `j`. A repetition that matched zero times is an empty `Seq`.
///
/// For example, matching `$( $a:ident $( $b:ident )* ),*` against `x y z, w, v u` captures:
/// * `a`: `Seq([Leaf(x), Leaf(w), Leaf(v)])`.
/// * `b`: `Seq([Seq([Leaf(y), Leaf(z)]), Seq([]), Seq([Leaf(u)])])`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CaptureTree<'db> {
    /// A single captured value.
    Leaf(CapturedValue<'db>),
    /// The groups of a pattern repetition, in match order.
    Seq(Vec<CaptureTree<'db>>),
}

impl<'db> CaptureTree<'db> {
    /// The subtree for the group being expanded, given its index in every entered `$()` expansion
    /// block, outermost first. A `Leaf` reached while indices remain is returned as is: the
    /// placeholder repeats less deeply than the block, so its value is broadcast to every group.
    /// `None` if an index is out of range, which
    /// [`SemanticDiagnosticKind::MacroPlaceholderRepDriverMismatch`] rejects at declaration time.
    fn at(&self, group_indices: &[usize]) -> Option<&Self> {
        let mut node = self;
        for &index in group_indices {
            let Self::Seq(groups) = node else { return Some(node) };
            node = groups.get(index)?;
        }
        Some(node)
    }

    /// The groups of the `Seq` at nesting `level` that is open for new content - the last group of
    /// every level above it. `None` if a `Leaf` is in the way or a level has no group yet, which
    /// means the pattern uses the name at conflicting nesting positions.
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
    /// pattern - see [`CaptureTree`]. Includes placeholders whose repetition matched zero times.
    /// A reused name is rejected at declaration time, but every rule is matched before its error
    /// is honored; the tree then only holds the values of the name's first use.
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
    fn record_capture(&mut self, name: SmolStrId<'db>, value: CapturedValue<'db>) {
        // The leaves of a placeholder are the elements of the `Seq` one level above its own depth.
        let Some(level) = self.repetition_depth.checked_sub(1) else {
            self.capture_trees.entry(name).or_insert(CaptureTree::Leaf(value));
            return;
        };
        // A `Seq` as the last group means the name is also used at a deeper nesting position.
        if let Some(groups) = self
            .capture_trees
            .get_mut(&name)
            .and_then(|tree| tree.open_groups_mut(level))
            .filter(|groups| !matches!(groups.last(), Some(CaptureTree::Seq(_))))
        {
            groups.push(CaptureTree::Leaf(value));
        }
    }

    /// Opens a group in the tree of every placeholder nested in `repetition`, which is about to be
    /// matched. Called once per encounter of the repetition, before [`Self::repetition_depth`] is
    /// incremented, so a zero-match repetition still leaves an empty group.
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
                self.capture_trees.entry(name).or_insert(CaptureTree::Seq(vec![]));
                continue;
            };
            // A `Leaf` as the last group means the name is also used at a shallower position.
            if let Some(groups) = self
                .capture_trees
                .get_mut(&name)
                .and_then(|tree| tree.open_groups_mut(level))
                .filter(|groups| !matches!(groups.last(), Some(CaptureTree::Leaf(_))))
            {
                groups.push(CaptureTree::Seq(vec![]));
            }
        }
    }
}

/// Reports the well-formedness defects of `elements`, the elements of a macro rule's pattern,
/// including the ones nested in its repetitions and subtrees. `Err` if any was reported.
fn check_pattern_elements<'db>(
    db: &'db dyn Database,
    elements: &[ast::MacroElement<'db>],
    outer: &[SyntaxNode<'db>],
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    let mut res = Ok(());
    for (index, element) in elements.iter().enumerate() {
        // What the pattern can match right after this element - also what it can match right
        // after the last element of this element's body, when it has one.
        let after = || first_followers(db, &elements[index + 1..], outer).0;
        match element {
            ast::MacroElement::Param(param) => {
                let ast::OptionParamKind::ParamKind(kind) = param.kind(db) else { continue };
                if !matches!(kind.kind(db), ast::MacroParamKind::Expr(_)) {
                    continue;
                }
                let name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                for follower in after() {
                    let text = follower.get_text_without_trivia(db);
                    if EXPR_FOLLOW_SET.contains(&text.long(db).as_str()) {
                        continue;
                    }
                    res = Err(diagnostics.report(
                        follower.stable_ptr(db),
                        SemanticDiagnosticKind::MacroExprPlaceholderFollower {
                            name,
                            follower: text,
                        },
                    ));
                }
            }
            ast::MacroElement::Repetition(repetition) => {
                // The end of a group is followed either by the separator, when another group
                // comes after it, or by whatever comes after the repetition itself.
                let mut body_outer = after();
                if let Some(separator) = repetition_separator(db, repetition) {
                    body_outer.push(separator);
                }
                res = res
                    .and(check_repetition_separator(db, repetition, diagnostics))
                    .and(check_repetition_body(db, repetition, diagnostics))
                    .and(check_pattern_elements(
                        db,
                        &repetition.elements(db).elements_vec(db),
                        &body_outer,
                        diagnostics,
                    ));
            }
            ast::MacroElement::Subtree(subtree) => {
                // The subtree's closing delimiter bounds its last element, so nothing of the
                // pattern outside the subtree can follow it.
                res = res.and(check_pattern_elements(
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

/// The separator token of `repetition`, if it declares one - matched and emitted as written.
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
/// most one group - a separator only ever stands between two groups.
fn check_repetition_separator<'db>(
    db: &'db dyn Database,
    repetition: &ast::MacroRepetition<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    if let Some(separator) = repetition_separator(db, repetition)
        && matches!(repetition.operator(db), ast::MacroRepetitionOperator::ZeroOrOne(_))
    {
        return Err(diagnostics.report(
            separator.stable_ptr(db),
            SemanticDiagnosticKind::MacroRepetitionSeparatorWithZeroOrOne,
        ));
    }
    Ok(())
}

/// Reports `repetition`, a `$()` block of a macro rule's pattern, if its body is empty - such a
/// block consumes no input, so it matches zero times against every call.
fn check_repetition_body<'db>(
    db: &'db dyn Database,
    repetition: &ast::MacroRepetition<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> Maybe<()> {
    if repetition.elements(db).elements(db).len() == 0 {
        return Err(diagnostics.report(
            repetition.stable_ptr(db).untyped(),
            SemanticDiagnosticKind::MacroRepetitionWithEmptyBody,
        ));
    }
    Ok(())
}

/// The texts a `$name:expr` placeholder of a macro rule's pattern may be followed by: the tokens
/// that can never continue an expression, so the placeholder's greedy capture cannot swallow them.
/// The same set `rustc` allows after its `expr` fragment.
const EXPR_FOLLOW_SET: [&str; 3] = [",", ";", "=>"];

/// The nodes `elements`, a run of a macro rule's pattern, can match first, and whether it can match
/// no input at all. `outer` is what the pattern can match right after all of `elements`; the
/// pattern running out is not a follower, as it bounds any capture before it.
fn first_followers<'db>(
    db: &'db dyn Database,
    elements: &[ast::MacroElement<'db>],
    outer: &[SyntaxNode<'db>],
) -> (Vec<SyntaxNode<'db>>, bool) {
    let mut res = vec![];
    for element in elements {
        match element {
            ast::MacroElement::Token(token) => {
                res.push(token.as_syntax_node());
                return (res, false);
            }
            ast::MacroElement::Param(param) => {
                res.push(param.as_syntax_node());
                return (res, false);
            }
            ast::MacroElement::Subtree(subtree) => {
                res.push(match subtree.subtree(db) {
                    ast::WrappedMacro::Parenthesized(inner) => inner.lparen(db).as_syntax_node(),
                    ast::WrappedMacro::Braced(inner) => inner.lbrace(db).as_syntax_node(),
                    ast::WrappedMacro::Bracketed(inner) => inner.lbrack(db).as_syntax_node(),
                });
                return (res, false);
            }
            ast::MacroElement::Repetition(repetition) => {
                let (body_first, body_maybe_empty) =
                    first_followers(db, &repetition.elements(db).elements_vec(db), &[]);
                let may_match_nothing = body_maybe_empty
                    || matches!(
                        repetition.operator(db),
                        ast::MacroRepetitionOperator::ZeroOrOne(_)
                            | ast::MacroRepetitionOperator::ZeroOrMore(_)
                    );
                res.extend(body_first);
                if !may_match_nothing {
                    return (res, false);
                }
            }
        }
    }
    res.extend(outer.iter().copied());
    (res, true)
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
        if pattern.as_syntax_node().contains_missing(db) {
            // The pattern cannot be matched as written, so the rule is dropped - keeping it would
            // let it match on the strength of the nodes that did parse, shadowing the rules
            // after it.
            diagnostics.report(
                pattern.stable_ptr(db).untyped(),
                SemanticDiagnosticKind::MacroRuleWithUnparsablePattern,
            );
            continue;
        }
        let expansion = rule_syntax.rhs(db).elements(db);
        let pattern_elements = get_macro_elements(db, pattern.clone());
        let placeholders = PatternPlaceholders::collect(db, pattern_elements.elements(db));
        let mut rule_err = Ok(());
        for (name, ptr) in placeholders.reused_names.iter() {
            rule_err =
                Err(diagnostics
                    .report(*ptr, SemanticDiagnosticKind::DuplicateMacroPlaceholder(*name)));
        }
        for (name, ptr) in placeholders.modifier_named.iter() {
            rule_err = Err(diagnostics.report(
                *ptr,
                SemanticDiagnosticKind::MacroPlaceholderNamedAfterResolverModifier(*name),
            ));
        }
        rule_err = rule_err.and(check_pattern_elements(
            db,
            &pattern_elements.elements_vec(db),
            &[],
            &mut diagnostics,
        ));
        // A reused or modifier-named placeholder leaves its capture depth unrecorded, so the
        // expansion is only checked against a pattern without one.
        if placeholders.reused_names.is_empty() && placeholders.modifier_named.is_empty() {
            let mut ctx = ExpansionCheckCtx {
                db,
                known_path: &[],
                curr_rep_depth: 0,
                placeholder_paths: &placeholders.paths,
                diagnostics: &mut diagnostics,
                rule_err: Ok(()),
                enclosing_block_reported: false,
            };
            ctx.check_node(expansion.as_syntax_node());
            rule_err = rule_err.and(ctx.rule_err);
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
    /// nested in, outermost first, IDs assigned in left-to-right DFS order. Used by
    /// [`ExpansionCheckCtx`] to validate the expansion. A reused name holds the path of its last
    /// use.
    paths: OrderedHashMap<SmolStrId<'db>, Vec<usize>>,
    /// Every name the pattern uses for more than one placeholder, pointing at its second use.
    reused_names: OrderedHashMap<SmolStrId<'db>, SyntaxStablePtrId<'db>>,
    /// Placeholders named after a `$defsite` / `$callsite` resolver modifier. Such a name is not a
    /// placeholder in an expansion - see [`extract_placeholder`] - so nothing can ever read the
    /// value it captures.
    modifier_named: Vec<(SmolStrId<'db>, SyntaxStablePtrId<'db>)>,
}

impl<'db> PatternPlaceholders<'db> {
    /// Collects the placeholders of `elements`, the elements of a macro rule's pattern, including
    /// the ones nested in its repetitions and subtrees.
    fn collect(
        db: &'db dyn Database,
        elements: impl IntoIterator<Item = ast::MacroElement<'db>>,
    ) -> Self {
        let mut res = Self::default();
        res.collect_elements(db, elements, &mut vec![], &mut 0);
        res
    }

    /// Recursive part of [`Self::collect`]: `current_path` is the path of the elements being
    /// traversed, and `next_rep_id` the counter assigning repetition IDs.
    fn collect_elements(
        &mut self,
        db: &'db dyn Database,
        elements: impl IntoIterator<Item = ast::MacroElement<'db>>,
        current_path: &mut Vec<usize>,
        next_rep_id: &mut usize,
    ) {
        for element in elements {
            match element {
                ast::MacroElement::Param(param) => {
                    let name = param.name(db).as_syntax_node().get_text_without_trivia(db);
                    if extract_placeholder(db, &param).is_none() {
                        self.modifier_named.push((name, param.stable_ptr(db).untyped()));
                        continue;
                    }
                    if self.paths.insert(name, current_path.clone()).is_some() {
                        self.reused_names.entry(name).or_insert(param.stable_ptr(db).untyped());
                    }
                }
                ast::MacroElement::Repetition(rep) => {
                    let rep_id = *next_rep_id;
                    *next_rep_id += 1;
                    current_path.push(rep_id);
                    self.collect_elements(
                        db,
                        rep.elements(db).elements(db),
                        current_path,
                        next_rep_id,
                    );
                    assert_eq!(current_path.pop(), Some(rep_id));
                }
                ast::MacroElement::Subtree(subtree) => {
                    self.collect_elements(
                        db,
                        get_macro_elements(db, subtree.subtree(db)).elements(db),
                        current_path,
                        next_rep_id,
                    );
                }
                ast::MacroElement::Token(_) => {}
            }
        }
    }
}

/// Whether a placeholder in `block`'s subtree drives it - see the model on
/// [`ExpansionCheckCtx`]. `enclosing_depth` is the number of expansion blocks `block` is nested
/// in.
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
///
/// The model: a placeholder nested under `d` pattern repetitions (its *pattern depth*) captures a
/// `d`-layer nested list of matches, and each `$()` expansion block iterates one layer. So:
/// * A placeholder must be spliced under at least `d` expansion blocks.
/// * A block's repetition count is set by a placeholder in its subtree - at any nesting - whose
///   pattern depth exceeds the block's nesting depth. Such a placeholder *drives* the block; a
///   driverless block is rejected.
/// * All placeholders under a block must come from the same pattern repetition, so their counts
///   agree.
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
    /// Whether an enclosing `$()` block was already reported as driverless; a block nested in it
    /// fails for the same reason, so only the outermost one is reported.
    enclosing_block_reported: bool,
}

impl<'db> ExpansionCheckCtx<'db, '_> {
    /// Validates placeholder usage by recursively traversing `node`.
    ///
    /// Reports, in the vocabulary of the model on [`ExpansionCheckCtx`]:
    /// * A placeholder spliced under fewer expansion blocks than its pattern depth.
    /// * A placeholder from a different pattern repetition than the block's driving one.
    /// * A `$()` block whose repetition count is undetermined, as no placeholder drives it.
    /// * A separator on a `?` block: see [`check_repetition_separator`].
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
            let outer_enclosing_block_reported = self.enclosing_block_reported;
            if !outer_enclosing_block_reported
                && !has_driving_placeholder(db, node, self.placeholder_paths, self.curr_rep_depth)
            {
                self.rule_err = Err(self.diagnostics.report(
                    repetition.stable_ptr(db).untyped(),
                    SemanticDiagnosticKind::MacroRepetitionWithoutRepeatingPlaceholder,
                ));
                self.enclosing_block_reported = true;
            }
            self.curr_rep_depth += 1;
            for element in repetition.elements(db).elements(db) {
                self.check_node(element.as_syntax_node());
            }
            self.curr_rep_depth -= 1;
            self.enclosing_block_reported = outer_enclosing_block_reported;
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
                            },
                        );
                        continue;
                    }
                    PlaceholderKind::Expr => {
                        let peek_token = input_iter.peek().cloned()?;
                        let file_id = peek_token.as_syntax_node().stable_ptr(db).file_id(db);
                        let expr_node = as_expr_macro_token_tree(input_iter, file_id, db)?;
                        let syntax_node = expr_node.as_syntax_node();
                        // The trivia around the expression belongs to the call; the expansion
                        // spaces the value by its own trivia.
                        let text =
                            syntax_node.get_text_of_span(db, syntax_node.span_without_trivia(db));
                        ctx.record_capture(
                            placeholder_name,
                            CapturedValue {
                                text: if capture_needs_parens(db, expr_node) {
                                    format!("({text})")
                                } else {
                                    text.to_string()
                                },
                                stable_ptr: peek_token.stable_ptr(db).untyped(),
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
                // The delimiters of a subtree are part of the pattern, so another kind of subtree
                // fails the rule.
                let matcher_subtree = matcher_subtree.subtree(db);
                let inner_input_tokens = match (&matcher_subtree, input_subtree.subtree(db)) {
                    (
                        ast::WrappedMacro::Parenthesized(_),
                        ast::WrappedTokenTree::Parenthesized(input),
                    ) => input.tokens(db),
                    (ast::WrappedMacro::Braced(_), ast::WrappedTokenTree::Braced(input)) => {
                        input.tokens(db)
                    }
                    (ast::WrappedMacro::Bracketed(_), ast::WrappedTokenTree::Bracketed(input)) => {
                        input.tokens(db)
                    }
                    _ => return None,
                };
                let inner_elements = get_macro_elements(db, matcher_subtree);
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
                    let mut temp_iter = input_iter.clone();
                    // A separator stands only between groups, so it is consumed with the group
                    // that follows it; a trailing one is left in the input.
                    if match_count > 0
                        && let Some(expected_sep) = &expected_separator
                    {
                        let Some(ast::TokenTree::Token(token_leaf)) = temp_iter.next() else {
                            break;
                        };
                        if token_leaf.as_syntax_node().get_text_without_trivia(db) != *expected_sep
                        {
                            break;
                        }
                    }
                    let mut inner_ctx = ctx.clone();
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

/// Whether an `expr` capture of `expr_node`'s shape must be parenthesized to stay a single operand
/// when spliced as text: its top level is an operator the expansion's own operators would bind
/// into, or a struct constructor, whose `{` does not parse in a match scrutinee or loop condition.
/// `@` and `&` also form types, which must reach a type-position splice bare, so they are exempt
/// unless their operand needs the parentheses itself.
fn capture_needs_parens<'db>(db: &'db dyn Database, expr_node: ast::Expr<'db>) -> bool {
    match expr_node {
        ast::Expr::Unary(unary) => match unary.op(db) {
            ast::UnaryOperator::At(_) | ast::UnaryOperator::Reference(_) => {
                capture_needs_parens(db, unary.expr(db))
            }
            _ => true,
        },
        ast::Expr::Binary(_) | ast::Expr::Closure(_) | ast::Expr::StructCtorCall(_) => true,
        _ => false,
    }
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
                self.expand_repetition(&ast::MacroRepetition::from_syntax_node(db, node))?;
                self.push_trailing_trivia(node);
                return Ok(());
            }
            _ => {}
        }
        if node.kind(db).is_terminal() {
            self.push_text(node.get_text(db));
            return Ok(());
        }
        for child in node.get_children(db).iter() {
            self.expand_node(*child)?;
        }
        Ok(())
    }

    /// Appends `text` to the expansion, spaced from the token before it when the two would
    /// otherwise fuse into one - a replaced node carries no trivia of its own.
    fn push_text(&mut self, text: &str) {
        self.keep_apart(text);
        self.res_buffer.push_str(text);
    }

    /// Appends a space when `next` would fuse with the token the expansion currently ends with.
    fn keep_apart(&mut self, next: &str) {
        let is_word = |c: char| c.is_alphanumeric() || c == '_';
        if self.res_buffer.ends_with(is_word) && next.starts_with(is_word) {
            self.res_buffer.push(' ');
        }
    }

    /// Appends the trailing trivia of `node`, whose text the expansion replaced, to
    /// [`Self::res_buffer`]. Its leading trivia is not emitted: the resolver maps a path back to
    /// the call by the offset of its node, which includes the leading trivia, so emitting it would
    /// put the offset before the value's [`CodeMapping`].
    fn push_trailing_trivia(&mut self, node: SyntaxNode<'db>) {
        let db = self.db;
        let span = TextSpan::new(node.span_end_without_trivia(db), node.span(db).end);
        self.res_buffer.push_str(node.get_text_of_span(db, span));
    }

    /// Expands the placeholder `name`, used by `param`, to the value it captured in the group being
    /// expanded.
    fn expand_placeholder(
        &mut self,
        param: &MacroParam<'db>,
        name: SmolStrId<'db>,
    ) -> Result<(), MacroExpansionError<'db>> {
        let db = self.db;
        let capture_trees = self.capture_trees;
        let Some(CaptureTree::Leaf(value)) =
            capture_trees.get(&name).and_then(|tree| tree.at(&self.group_indices))
        else {
            // Rejected at declaration time by `UndefinedMacroPlaceholder` or
            // `MacroPlaceholderRepDepthMismatch`.
            return Err(MacroExpansionError {
                stable_ptr: param.stable_ptr(db).untyped(),
                failure: MacroExpansionFailure::MissingCapture(name),
            });
        };
        self.keep_apart(&value.text);
        let start = TextWidth::from_str(&self.res_buffer).as_offset();
        let span = TextSpan::new_with_width(start, TextWidth::from_str(&value.text));
        self.res_buffer.push_str(&value.text);
        self.code_mappings.push(CodeMapping {
            span,
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
        let elements = repetition.elements(db);
        let separator = repetition_separator(db, repetition);
        for index in 0..group_count {
            self.group_indices.push(index);
            let expanded = elements
                .elements(db)
                .try_for_each(|element| self.expand_node(element.as_syntax_node()));
            self.group_indices.pop();
            expanded?;
            if index + 1 < group_count
                && let Some(sep) = separator
            {
                // The separator's leading trivia sits on the repetition's closing `)`, which is
                // not emitted - so the spacing written before it is pushed from there.
                self.push_trailing_trivia(repetition.rparen(db).as_syntax_node());
                self.push_text(sep.get_text(db));
            }
        }
        Ok(())
    }

    /// The number of groups `repetition` is expanded over: the group count of the placeholders
    /// whose captures still repeat at its depth. Placeholders reaching a `Leaf` are broadcast.
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
