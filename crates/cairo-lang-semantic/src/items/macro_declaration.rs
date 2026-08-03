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
/// would have taken - which the flat [`MatcherContext::captures`] cannot express, as it cannot tell
/// a placeholder that matched nothing from one that does not exist.
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
    /// The values of the tree's leaves, in the order they were matched in - which is the order of
    /// the matching placeholder's flat capture list.
    pub fn leaves(&self) -> impl Iterator<Item = &CapturedValue<'db>> {
        let mut stack = vec![self];
        std::iter::from_fn(move || {
            while let Some(node) = stack.pop() {
                match node {
                    Self::Leaf(value) => return Some(value),
                    Self::Seq(groups) => stack.extend(groups.iter().rev()),
                }
            }
            None
        })
    }

    /// Whether all the leaves of the tree are nested under the same number of `Seq` levels, which
    /// is the pattern repetition depth of the placeholder the tree belongs to. Leaves at
    /// differing levels mean a value was added at the wrong nesting position.
    fn is_uniformly_nested(&self) -> bool {
        let mut stack = vec![(self, 0)];
        let mut leaves_level = None;
        while let Some((node, level)) = stack.pop() {
            match node {
                Self::Leaf(_) => {
                    if *leaves_level.get_or_insert(level) != level {
                        return false;
                    }
                }
                Self::Seq(groups) => stack.extend(groups.iter().map(|group| (group, level + 1))),
            }
        }
        true
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

/// Context used during macro pattern matching and expansion.
/// Tracks captured values, active repetition scopes, and repetition ownership per placeholder.
#[derive(Default, Clone, Debug)]
pub struct MatcherContext<'db> {
    /// The captured values per macro parameter name.
    /// These are flat lists, even for repeated placeholders.
    pub captures: Captures<'db>,

    /// The captured values per macro parameter name, nested by the repetition structure of the
    /// pattern - see [`CaptureTree`]. Holds a tree for every placeholder of the matched pattern,
    /// including ones whose repetition matched zero times.
    ///
    /// Mirrors [`Self::captures`], which remains the authoritative representation: the leaves of a
    /// name's tree, in order, are exactly the name's flat capture list.
    pub capture_trees: OrderedHashMap<SmolStrId<'db>, CaptureTree<'db>>,

    /// The names whose [`Self::capture_trees`] entry could not mirror the pattern, as the pattern
    /// uses the name at conflicting nesting positions - at two different repetition depths, or
    /// twice outside any repetition. Param name uniqueness is not verified, so such a pattern
    /// is accepted; a single tree cannot hold all of the name's values, so consumers must fall
    /// back to [`Self::captures`] for these names.
    ///
    /// A name used at the same depth by several repetitions is not conflicting: its tree holds the
    /// groups of all of them, exactly as [`Self::captures`] holds their values.
    pub poisoned_capture_trees: OrderedHashSet<SmolStrId<'db>>,

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

impl<'db> MatcherContext<'db> {
    /// Records `value` as the next capture of `name`, in both the flat [`Self::captures`] and the
    /// nested [`Self::capture_trees`].
    fn record_capture(&mut self, name: SmolStrId<'db>, value: CapturedValue<'db>) {
        self.captures.entry(name).or_default().push(value.clone());
        // The placeholder is nested in one pattern repetition per entry of the repetition stack, so
        // its leaves are the elements of the `Seq` one level above its own nesting depth.
        let Some(level) = self.current_repetition_stack.len().checked_sub(1) else {
            // Outside any repetition the placeholder's tree is the leaf itself, so a pattern using
            // the name a second time there has nowhere to put its value.
            if self.capture_trees.contains_key(&name) {
                self.poisoned_capture_trees.insert(name);
            } else {
                self.capture_trees.insert(name, CaptureTree::Leaf(value));
            }
            return;
        };
        // Finding the groups of an inner repetition where the leaf belongs means the pattern uses
        // the name at a deeper nesting position as well, so its tree is poisoned.
        let Some(groups) = self
            .capture_trees
            .get_mut(&name)
            .and_then(|tree| tree.open_groups_mut(level))
            .filter(|groups| !matches!(groups.last(), Some(CaptureTree::Seq(_))))
        else {
            self.poisoned_capture_trees.insert(name);
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
    /// Must be called before pushing `repetition` onto [`Self::current_repetition_stack`], as the
    /// group is opened at the nesting depth of the repetitions enclosing it.
    ///
    /// Finding a leaf of the placeholder where the group belongs means the pattern uses the name at
    /// a shallower nesting position as well, so its tree is poisoned.
    fn open_repetition_groups(
        &mut self,
        db: &'db dyn Database,
        repetition: &ast::MacroRepetition<'db>,
    ) {
        let depth = self.current_repetition_stack.len();
        let mut names = OrderedHashSet::default();
        collect_pattern_placeholder_names(db, repetition.elements(db).elements(db), &mut names);
        for name in names {
            let Some(level) = depth.checked_sub(1) else {
                // The outermost repetitions of a placeholder are the groups of its tree's root.
                let tree = self.capture_trees.entry(name).or_insert(CaptureTree::Seq(vec![]));
                if matches!(tree, CaptureTree::Leaf(_)) {
                    self.poisoned_capture_trees.insert(name);
                }
                continue;
            };
            let Some(groups) = self
                .capture_trees
                .get_mut(&name)
                .and_then(|tree| tree.open_groups_mut(level))
                .filter(|groups| !matches!(groups.last(), Some(CaptureTree::Leaf(_))))
            else {
                self.poisoned_capture_trees.insert(name);
                continue;
            };
            groups.push(CaptureTree::Seq(vec![]));
        }
    }
}

/// Whether the nested capture trees of a completed match are valid, that is:
/// * The leaves of a placeholder's tree, in match order, are exactly the placeholder's flat capture
///   list - the invariant tying the two representations together.
/// * Every captured placeholder has a tree. A placeholder may have a tree with no leaves, as its
///   repetition may have matched zero times.
/// * All the leaves of a tree are at the same nesting level, as they are all captured by the same
///   placeholder, at a single repetition depth of the pattern.
///
/// Poisoned names are exempt, as their tree cannot mirror the pattern in the first place.
fn capture_trees_are_valid(ctx: &MatcherContext<'_>) -> bool {
    ctx.capture_trees.iter().all(|(name, tree)| {
        ctx.poisoned_capture_trees.contains(name)
            || (tree.is_uniformly_nested()
                && tree
                    .leaves()
                    .eq(ctx.captures.get(name).map_or(&[][..], |values| values.as_slice())))
    }) && ctx.captures.keys().all(|name| {
        ctx.capture_trees.contains_key(name) || ctx.poisoned_capture_trees.contains(name)
    })
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

    // TODO(Dean): Verify uniqueness of param names.
    // TODO(Dean): Verify consistency bracket terminals.
    let mut rules = vec![];
    for rule_syntax in macro_declaration_syntax.rules(db).elements(db) {
        let pattern = rule_syntax.lhs(db);
        let expansion = rule_syntax.rhs(db).elements(db);
        let pattern_elements = get_macro_elements(db, pattern.clone());
        // Collect the repetition path (outermost-to-innermost pattern rep IDs) for every
        // placeholder defined in the pattern.
        let mut placeholder_paths: OrderedHashMap<SmolStrId<'db>, Vec<usize>> = Default::default();
        let mut next_rep_id = 0;
        collect_placeholder_paths(
            db,
            pattern_elements.elements(db),
            &mut vec![],
            &mut next_rep_id,
            &mut placeholder_paths,
        );

        let mut ctx = ExpansionCheckCtx {
            db,
            known_path: &[],
            curr_rep_depth: 0,
            placeholder_paths: &placeholder_paths,
            diagnostics: &mut diagnostics,
            rule_err: Ok(()),
            in_non_repeating_block: false,
        };
        ctx.check_node(expansion.as_syntax_node());
        // Skipping expanding an inline macro if it had a parser error.
        if pattern.as_syntax_node().contains_missing(db) {
            continue;
        }
        rules.push(MacroRuleData { pattern, expansion, err: ctx.rule_err });
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

/// Assigns a unique ID to every `$()` repetition block in the pattern (left-to-right DFS order)
/// and records, for each placeholder, its path: the ordered list of ancestor repetition IDs from
/// outermost to innermost. The resulting map is used by [`ExpansionCheckCtx`] to validate the
/// expansion.
fn collect_placeholder_paths<'db>(
    db: &'db dyn Database,
    elements: impl IntoIterator<Item = ast::MacroElement<'db>>,
    current_path: &mut Vec<usize>,
    next_rep_id: &mut usize,
    result: &mut OrderedHashMap<SmolStrId<'db>, Vec<usize>>,
) {
    for element in elements {
        match element {
            ast::MacroElement::Param(param) => {
                result.insert(
                    param.name(db).as_syntax_node().get_text_without_trivia(db),
                    current_path.clone(),
                );
            }
            ast::MacroElement::Repetition(rep) => {
                let rep_id = *next_rep_id;
                *next_rep_id += 1;
                current_path.push(rep_id);
                let inner = rep.elements(db).elements(db);
                collect_placeholder_paths(db, inner, current_path, next_rep_id, result);
                assert_eq!(current_path.pop(), Some(rep_id));
            }
            ast::MacroElement::Subtree(subtree) => {
                let inner = get_macro_elements(db, subtree.subtree(db)).elements(db);
                collect_placeholder_paths(db, inner, current_path, next_rep_id, result);
            }
            ast::MacroElement::Token(_) => {}
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
    /// Three kinds of errors are reported:
    /// * Depth mismatch: placeholder used at fewer expansion levels than its pattern depth.
    /// * Context mismatch: placeholder from a different repetition than the driving one.
    /// * Non-repeating block: a `$()` block holding no placeholder that repeats at its depth, so
    ///   there is nothing to drive it.
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
) -> Option<(Captures<'db>, OrderedHashMap<SmolStrId<'db>, RepetitionId>)> {
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
    debug_assert!(
        capture_trees_are_valid(&ctx),
        "The nested capture trees do not agree with the flat captures. Trees: {:?}. Captures: \
         {:?}.",
        ctx.capture_trees,
        ctx.captures
    );
    Some((ctx.captures, ctx.placeholder_to_rep_id))
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
                        if let Some(rep_id) = ctx.current_repetition_stack.last() {
                            ctx.placeholder_to_rep_id.insert(placeholder_name, *rep_id);
                        }
                        continue;
                    }
                    PlaceholderKind::Expr => {
                        let peek_token = input_iter.peek().cloned()?;
                        let file_id = peek_token.as_syntax_node().stable_ptr(db).file_id(db);
                        let expr_node = as_expr_macro_token_tree(input_iter.clone(), file_id, db)?;
                        let expr_text = expr_node.as_syntax_node().get_text(db);
                        let expr_length = expr_text.len();
                        // An empty expression is parsed successfully. However we don't want to
                        // capture it a valid expr.
                        if expr_length == 0 {
                            return None;
                        }

                        ctx.record_capture(
                            placeholder_name,
                            CapturedValue {
                                text: expr_text.to_string(),
                                stable_ptr: peek_token.stable_ptr(db).untyped(),
                            },
                        );
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
                                ast::TokenTree::Token(leaf) => leaf.as_syntax_node(),
                                ast::TokenTree::Subtree(subtree) => subtree.as_syntax_node(),
                                ast::TokenTree::Repetition(rep) => rep.as_syntax_node(),
                                ast::TokenTree::Param(param) => param.as_syntax_node(),
                                ast::TokenTree::Missing(_) => unreachable!(),
                            }
                            .get_text(db);
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
                advanced = true;
                let input_token = input_iter.next()?;
                if let ast::TokenTree::Subtree(input_subtree) = input_token {
                    let inner_elements = get_macro_elements(db, matcher_subtree.subtree(db));
                    let mut inner_input_iter = match input_subtree.subtree(db) {
                        ast::WrappedTokenTree::Parenthesized(tt) => tt.tokens(db),
                        ast::WrappedTokenTree::Braced(tt) => tt.tokens(db),
                        ast::WrappedTokenTree::Bracketed(tt) => tt.tokens(db),
                        ast::WrappedTokenTree::Missing(_) => unreachable!(),
                    }
                    .elements(db)
                    .peekable();
                    is_macro_rule_match_ex(db, inner_elements, &mut inner_input_iter, ctx, true)?;
                    continue;
                } else {
                    return None;
                }
            }
            ast::MacroElement::Repetition(repetition) => {
                let rep_id = RepetitionId(ctx.next_repetition_id);
                ctx.next_repetition_id += 1;
                ctx.open_repetition_groups(db, &repetition);
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
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
pub enum MacroExpansionFailure<'db> {
    /// A `$( ... )` block in the expansion holds no placeholder, so there is nothing to determine
    /// how many times it should be repeated.
    RepetitionWithoutPlaceholder,
    /// A placeholder in the expansion has no captured value at the current repetition indices.
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
    matcher_ctx: &mut MatcherContext<'db>,
) -> Result<MacroExpansionResult, MacroExpansionError<'db>> {
    let node = rule.expansion.as_syntax_node();
    let mut res_buffer = String::new();
    let mut code_mappings = Vec::new();
    expand_macro_rule_ex(db, node, matcher_ctx, &mut res_buffer, &mut code_mappings)?;
    Ok(MacroExpansionResult { text: res_buffer.into(), code_mappings: code_mappings.into() })
}

/// Helper function for [expand_macro_rule]. Traverses the macro expansion and replaces the
/// placeholders with the provided values while collecting the result in res_buffer.
fn expand_macro_rule_ex<'db>(
    db: &'db dyn Database,
    node: SyntaxNode<'db>,
    matcher_ctx: &mut MatcherContext<'db>,
    res_buffer: &mut String,
    code_mappings: &mut Vec<CodeMapping>,
) -> Result<(), MacroExpansionError<'db>> {
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
                    .ok_or(MacroExpansionError {
                        stable_ptr: path_node.stable_ptr(db).untyped(),
                        failure: MacroExpansionFailure::MissingCapture(name),
                    })?;
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
            let elements = repetition.elements(db);
            let first_param = find_first_repetition_param(db, elements.elements(db)).ok_or(
                MacroExpansionError {
                    stable_ptr: repetition.stable_ptr(db).untyped(),
                    failure: MacroExpansionFailure::RepetitionWithoutPlaceholder,
                },
            )?;
            let placeholder_name = first_param.name(db).text(db);
            // If the placeholder isn't mapped to any repetition, it means it doesn't belong to any
            // consumed repetition.
            let Some(rep_id) = matcher_ctx.placeholder_to_rep_id.get(&placeholder_name).copied()
            else {
                return Ok(());
            };
            let repetition_len =
                matcher_ctx.captures.get(&placeholder_name).map(|v| v.len()).unwrap_or(0);
            for i in 0..repetition_len {
                matcher_ctx.repetition_indices.insert(rep_id, i);
                for element in elements.elements(db) {
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

/// Returns the first param within the given macro elements.
fn find_first_repetition_param<'db>(
    db: &'db dyn Database,
    elements: impl IntoIterator<Item = MacroElement<'db>>,
) -> Option<MacroParam<'db>> {
    for element in elements {
        match element {
            ast::MacroElement::Param(param) => return Some(param),
            ast::MacroElement::Subtree(subtree) => {
                let inner_elements = get_macro_elements(db, subtree.subtree(db)).elements(db);
                if let Some(param) = find_first_repetition_param(db, inner_elements) {
                    return Some(param);
                }
            }
            ast::MacroElement::Repetition(repetition) => {
                let inner_elements = repetition.elements(db).elements(db);
                if let Some(param) = find_first_repetition_param(db, inner_elements) {
                    return Some(param);
                }
            }
            ast::MacroElement::Token(_) => {}
        }
    }
    None
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
