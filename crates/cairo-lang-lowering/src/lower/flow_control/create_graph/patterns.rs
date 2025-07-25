use std::cell::RefCell;

use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::types::peel_snapshots;
use cairo_lang_semantic::{
    self as semantic, ConcreteEnumId, ConcreteTypeId, PatternEnumVariant, PatternTuple,
    PatternVariable, TypeId, TypeLongId,
};
use cairo_lang_utils::iterators::zip_eq3;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;

use super::super::graph::{
    Capture, Deconstruct, EnumMatch, FlowControlGraphBuilder, FlowControlNode, FlowControlVar,
    NodeId,
};
use crate::ids::LocationId;
use crate::lower::context::LoweringContext;

#[derive(Clone)]
pub enum Pattern<'a> {
    Semantic(&'a semantic::Pattern),
    Otherwise,
}
impl<'a> Pattern<'a> {
    /// Constructs a [Pattern] from a [semantic::PatternId].
    pub fn from_semantic(
        ctx: &'a LoweringContext<'_, '_>,
        semantic_pattern: semantic::PatternId,
    ) -> Self {
        Self::Semantic(&ctx.function_body.arenas.patterns[semantic_pattern])
    }
}

/// Any pattern matching function below takes a list of patterns and depending on the item at
/// question constructs a filtered list of patterns that are relevant to the item.
/// This struct represents the indices of those filtered patterns.
///
/// For example, consider the following match:
/// ```plain
/// match (x, y) {
///     (A | _, C) => { ... }
///     (B, _) => { ... }
/// }
/// ```
/// When we look at the first item, `x`, we have two patterns: `A | _` and `B`.
/// If `x=A` then the filtered list is `[0]`.
/// If `x=B` then the filtered list is `[0, 1]`.
/// For the latter, it is important to return both `0` and `1` because which arm is chosen depends
/// on the value of `y` (which will be handled by the calling pattern matching function).
#[derive(Clone, Hash, Eq, PartialEq)]
pub struct FilteredPatterns {
    /// The indices of the patterns that are accepted by the filter, together with capture
    /// information.
    filter: Vec<IndexAndCaptures>,
}

impl FilteredPatterns {
    fn empty() -> Self {
        Self { filter: vec![] }
    }

    /// Returns a [FilteredPatterns] that accepts all patterns (no filtering).
    fn all(n_patterns: usize) -> Self {
        Self {
            filter: (0..n_patterns)
                .map(|idx| IndexAndCaptures { index: idx, captures: Captures::default() })
                .collect_vec(),
        }
    }

    fn all_with_captures(captures_vec: impl Iterator<Item = Captures>) -> Self {
        Self {
            filter: captures_vec
                .enumerate()
                .map(|(index, captures)| IndexAndCaptures { index, captures })
                .collect_vec(),
        }
    }

    fn add(&mut self, idx: usize, captures: Captures) {
        self.filter.push(IndexAndCaptures { index: idx, captures });
    }

    /// Returns a lifted [FilteredPatterns] after a filtering a list of patterns.
    ///
    /// For example, assume that `foo` gets 3 patterns, and it calls `bar` with the last two
    /// patterns at indices `[1, 2]`. Suppose that `bar` filters it to only the last pattern.
    /// `bar` returns `[1]` since it uses its own indexing.
    /// `foo` needs to lift it to `[2]` to return to its caller using `foo`'s indexing.
    fn lift(self, outer_filter: &FilteredPatterns) -> Self {
        Self {
            filter: self.filter.into_iter().map(|capture| capture.lift(outer_filter)).collect_vec(),
        }
    }

    /// Returns the first index of the filtered patterns.
    // TODO: rename. Fix doc.
    pub fn first(self) -> Option<IndexAndCaptures> {
        self.filter.into_iter().next()
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct IndexAndCaptures {
    /// The index of the pattern in the list of patterns.
    pub index: usize,
    /// The captures that should be applied if the pattern is chosen.
    captures: Captures,
}
impl IndexAndCaptures {
    /// Lifts the index of the pattern to the outer level.
    /// See [FilteredPatterns::lift] for more details.
    fn lift(self, outer_filter: &FilteredPatterns) -> IndexAndCaptures {
        IndexAndCaptures {
            index: outer_filter.filter[self.index].index,
            captures: self.captures.union(&outer_filter.filter[self.index].captures),
        }
    }

    // TODO: doc.
    pub fn wrap_node(self, graph: &mut FlowControlGraphBuilder, mut node: NodeId) -> NodeId {
        for (input, output) in self.captures.captures {
            node = graph.add_node(FlowControlNode::Capture(Capture { input, output, next: node }));
        }
        node
    }
}

#[derive(Clone, Default, Hash, Eq, PartialEq)]
struct Captures {
    /// The captures that should be applied if the pattern is chosen.
    captures: Vec<(FlowControlVar, PatternVariable)>,
}
impl Captures {
    fn single(input: FlowControlVar, output: PatternVariable) -> Self {
        Self { captures: vec![(input, output)] }
    }

    fn union(&self, captures: &Self) -> Self {
        Self {
            captures: self.captures.iter().chain(captures.captures.iter()).cloned().collect_vec(),
        }
    }
}

pub struct Cache<Input> {
    cache: RefCell<UnorderedHashMap<Input, NodeId>>,
}
impl<Input: std::hash::Hash + Eq + Clone> Cache<Input> {
    pub fn get_or_compute(
        &self,
        callback: &dyn Fn(&mut FlowControlGraphBuilder, Input) -> NodeId,
        graph: &mut FlowControlGraphBuilder,
        input: Input,
    ) -> NodeId {
        if let Some(node_id) = self.cache.borrow().get(&input) {
            return *node_id;
        }

        let node_id = callback(graph, input.clone());
        assert!(!self.cache.borrow().contains_key(&input));
        self.cache.borrow_mut().insert(input, node_id);
        node_id
    }
}

impl<Input: std::hash::Hash + Eq + Clone> std::default::Default for Cache<Input> {
    fn default() -> Self {
        Self { cache: Default::default() }
    }
}

/// Given a list of patterns and the nodes to go to if the pattern matches,
/// returns a new graph node to handle the patterns.
///
/// `location` is the location of the expression initiating the match.
///
/// If `default` is provided, the patterns do not need to be exhaustive, and the default node
/// will be used if no pattern matches.
///
/// TODO: doc build_node_callback.
pub fn create_node_for_patterns(
    ctx: &LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    location: LocationId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(&mut FlowControlGraphBuilder, FilteredPatterns) -> NodeId,
) -> NodeId {
    // TODO: Check for semantic::Pattern::Otherwise in addition to Pattern::Otherwise.
    if patterns.iter().all(|pattern| pattern_is_any(pattern)) {
        return build_node_callback(
            graph,
            FilteredPatterns::all_with_captures(patterns.iter().map(|pattern| {
                if let Pattern::Semantic(semantic::Pattern::Variable(pattern_variable)) = pattern {
                    Captures::single(input_var, pattern_variable.clone())
                } else {
                    Captures::default()
                }
            })),
        );
        // TODO: Each variable should be registered for the appropriate node.
        // for pattern in patterns {
        //     if let Pattern::Semantic(semantic::Pattern::Variable(pattern_variable)) = pattern {
        //         return graph.add_node(FlowControlNode::Capture(Capture {
        //             input: input_var,
        //             output: pattern_variable.clone(),
        //             next: node,
        //         }));
        //     }
        // }
        // return node;
    }

    let cache = Cache::default();

    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, input_var.ty());
    // TODO: Handle n_snapshots.
    match long_ty {
        TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) => create_node_for_enum(
            ctx,
            graph,
            input_var,
            concrete_enum_id,
            location,
            patterns,
            &|graph, pattern_indices| {
                cache.get_or_compute(build_node_callback, graph, pattern_indices)
            },
        ),
        TypeLongId::Tuple(types) => create_node_for_tuple(
            ctx,
            graph,
            input_var,
            &types,
            location,
            patterns,
            &|graph, pattern_indices| {
                cache.get_or_compute(build_node_callback, graph, pattern_indices)
            },
        ),
        _ => todo!("{:?}", long_ty),
    }
}

fn create_node_for_enum(
    ctx: &LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    concrete_enum_id: ConcreteEnumId,
    location: LocationId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(&mut FlowControlGraphBuilder, FilteredPatterns) -> NodeId,
) -> NodeId {
    let concrete_variants = ctx.db.concrete_enum_variants(concrete_enum_id).unwrap(); // TODO: Fix unwrap.

    // Maps variant index to the list of the indices of the patterns that match it.
    let mut variant_to_pattern_indices: Vec<FilteredPatterns> = (0..concrete_variants.len()).map(|_|
        FilteredPatterns::empty()).collect_vec();
    // TODO: doc.
    let mut variant_to_inner_patterns: Vec<Vec<Pattern>> = vec![vec![]; concrete_variants.len()];
    for (idx, pattern) in patterns.iter().enumerate() {
        match pattern {
            Pattern::Semantic(semantic::Pattern::EnumVariant(PatternEnumVariant {
                variant,
                inner_pattern,
                ..
            })) => {
                variant_to_pattern_indices[variant.idx].add(idx, Captures::default());
                variant_to_inner_patterns[variant.idx].push(
                    inner_pattern
                        .map(|inner_pattern| Pattern::from_semantic(ctx, inner_pattern))
                        .unwrap_or(Pattern::Otherwise),
                );
            }
            Pattern::Semantic(semantic::Pattern::Otherwise(..)) | Pattern::Otherwise => {
                for pattern_indices in variant_to_pattern_indices.iter_mut() {
                    pattern_indices.add(idx, Captures::default());
                }
                for inner_patterns in variant_to_inner_patterns.iter_mut() {
                    inner_patterns.push(Pattern::Otherwise);
                }
            }
            Pattern::Semantic(semantic::Pattern::Variable(pattern_variable)) => {
                for pattern_indices in variant_to_pattern_indices.iter_mut() {
                    pattern_indices.add(idx, Captures::single(input_var, pattern_variable.clone()));
                }
                for inner_patterns in variant_to_inner_patterns.iter_mut() {
                    inner_patterns.push(Pattern::Otherwise);
                }
            }
            Pattern::Semantic(_) => todo!(),
        }
    }

    // Create the inner nodes.
    let variants =
        zip_eq3(concrete_variants, variant_to_pattern_indices, variant_to_inner_patterns)
            .map(|(concrete_variant, pattern_indices, inner_patterns)| {
                let inner_var = graph.new_var(concrete_variant.ty, location);
                let node = create_node_for_patterns(
                    ctx,
                    graph,
                    inner_var,
                    location, // TODO: Check.
                    &inner_patterns,
                    &|graph, pattern_indices_inner| {
                        build_node_callback(
                            graph,
                            pattern_indices_inner.lift(&pattern_indices),
                        )
                    },
                );
                (concrete_variant, node, inner_var)
            })
            .collect_vec();

    // TODO: support zero variants.
    let first_variant_node = variants[0].1;
    if variants.iter().all(|(_, node_id, _)| *node_id == first_variant_node) {
        // All the variants lead to the same node. No need to do the match.
        // TODO: handle the different inner_var that were already allocated.
        return first_variant_node;
    }

    // Create a node for the match.
    graph.add_node(FlowControlNode::EnumMatch(EnumMatch {
        matched_var: input_var,
        concrete_enum_id,
        variants,
    }))
}

fn create_node_for_tuple(
    ctx: &LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    types: &Vec<TypeId>,
    location: LocationId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(&mut FlowControlGraphBuilder, FilteredPatterns) -> NodeId,
) -> NodeId {
    let inner_vars = types.iter().map(|ty| graph.new_var(*ty, location)).collect_vec();

    let node = create_node_for_tuple_inner(
        ctx,
        graph,
        &inner_vars,
        types,
        location,
        patterns,
        build_node_callback,
        0,
    );

    // Deconstruct the input variable.
    graph.add_node(FlowControlNode::Deconstruct(Deconstruct {
        input: input_var,
        outputs: inner_vars,
        next: node,
    }))
}

// TODO: doc item_idx.
fn create_node_for_tuple_inner(
    ctx: &LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    inner_vars: &Vec<FlowControlVar>,
    types: &Vec<TypeId>,
    location: LocationId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(&mut FlowControlGraphBuilder, FilteredPatterns) -> NodeId,
    item_idx: usize,
) -> NodeId {
    if item_idx == types.len() {
        return build_node_callback(graph, FilteredPatterns::all(patterns.len()));
    }

    // TODO: Zero length tuple.
    let patterns_on_current_item = patterns
        .iter()
        .map(|pattern| match pattern {
            Pattern::Semantic(semantic::Pattern::Tuple(PatternTuple {
                field_patterns, ..
            })) => Pattern::from_semantic(ctx, field_patterns[item_idx]),
            Pattern::Semantic(semantic::Pattern::Otherwise(..)) | Pattern::Otherwise => {
                Pattern::Otherwise
            }
            Pattern::Semantic(_) => todo!(),
        })
        .collect_vec();

    create_node_for_patterns(
        ctx,
        graph,
        inner_vars[item_idx],
        location, // TODO: Check,
        &patterns_on_current_item,
        &|graph, pattern_indices| {
            create_node_for_tuple_inner(
                ctx,
                graph,
                inner_vars,
                types,
                location,
                &pattern_indices.filter.iter().map(|idx| patterns[idx.index].clone()).collect_vec(),
                &|graph, pattern_indices_inner| {
                    build_node_callback(graph, pattern_indices_inner.lift(&pattern_indices))
                },
                item_idx + 1,
            )
        },
    )
}

/// Returns true if all patterns in the slice are `Pattern::Otherwise`.
fn pattern_is_any(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Semantic(semantic::Pattern::Otherwise(..))
        | Pattern::Semantic(semantic::Pattern::Variable(..))
        | Pattern::Otherwise => true,
        Pattern::Semantic(_) => false,
    }
}

// TODO: Change build_node_callback to get `&Vec<usize>` instead of `Vec<usize>`.
