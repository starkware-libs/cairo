use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::types::{peel_snapshots, wrap_in_snapshots};
use cairo_lang_semantic::{
    self as semantic, ConcreteEnumId, ConcreteTypeId, PatternEnumVariant, PatternTuple, TypeId,
    TypeLongId, corelib,
};
use cairo_lang_syntax::node::ast::ExprPtr;
use cairo_lang_utils::iterators::zip_eq3;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;
use num_traits::ToPrimitive;

use super::super::graph::{
    Deconstruct, EnumMatch, FlowControlGraphBuilder, FlowControlNode, FlowControlVar, NodeId,
};
use super::cache::Cache;
use super::filtered_patterns::{Bindings, FilteredPatterns};
use crate::diagnostic::{LoweringDiagnosticKind, MatchDiagnostic, MatchError};
use crate::ids::LocationId;
use crate::lower::context::LoweringContext;
use crate::lower::flow_control::graph::EqualsLiteral;

/// A callback that gets a [FilteredPatterns] and constructs a node that continues the pattern
/// matching restricted to the filtered patterns.
///
/// For example, consider the following match:
/// ```plain
/// match (x, y) {
///     (_, C) => 0
///     (B, _) => 1
///     _ => 2
/// }
/// ```
///
/// The pattern-matching function that handles the tuple will call the pattern-matching function
/// for `x` with the following three patterns: (0) `_`, (1) `B`, (2) `_`.
///
/// The inner function will compute the filtered patterns for each variant of `x`.
/// For `x=A`, the filtered patterns are `[0, 2]`.
/// Then, the callback will be called with the filter, and it will return a node that
/// checks if `y=C` or not, and returns `0` or `2` respectively.
/// For `x=B`, the filtered patterns are `[0, 1, 2]`, and the callback will return a node that
/// returns `0` if `y=C` and `1` otherwise.
/// Finally, the inner pattern-matching function (for `x`) will construct a [EnumMatch] node
/// that leads to the two nodes returned by the callback.
type BuildNodeCallback<'db, 'a> =
    &'a mut dyn FnMut(&mut FlowControlGraphBuilder<'db>, FilteredPatterns) -> NodeId;

/// A thin wrapper around [semantic::Pattern], where `None` represents the `_` pattern.
type PatternOption<'a, 'db> = Option<&'a semantic::Pattern<'db>>;

/// Common parameters for the `create_node_*` functions.
pub struct CreateNodeParams<'db, 'mt, 'a> {
    /// The lowering context.
    pub ctx: &'a LoweringContext<'db, 'mt>,
    /// The graph builder.
    pub graph: &'a mut FlowControlGraphBuilder<'db>,
    /// The patterns to match.
    pub patterns: &'a [PatternOption<'a, 'db>],
    /// A callback that gets a [FilteredPatterns] and constructs a node that continues the pattern
    /// matching.
    pub build_node_callback: BuildNodeCallback<'db, 'a>,
    /// The location of the expression initiating the match.
    pub location: LocationId<'db>,
}

/// Given `input_var` and a list of patterns, returns a new graph node to handle the patterns.
pub fn create_node_for_patterns<'db>(
    params: CreateNodeParams<'db, '_, '_>,
    input_var: FlowControlVar,
) -> NodeId {
    let CreateNodeParams { ctx, graph, patterns, build_node_callback, location } = params;
    // Handle `Pattern::Variable` patterns. Replace them with `Pattern::Otherwise` and collect the
    // bindings.
    let mut bindings: Vec<Bindings> = vec![];
    let patterns: Vec<PatternOption<'_, 'db>> = patterns
        .iter()
        .map(|pattern| {
            if let Some(semantic::Pattern::Variable(pattern_variable)) = pattern {
                let pattern_var = graph.register_pattern_var(pattern_variable.clone());
                bindings.push(Bindings::single(input_var, pattern_var));
                None
            } else {
                bindings.push(Bindings::default());
                *pattern
            }
        })
        .collect_vec();

    let mut cache = Cache::default();

    // Wrap `build_node_callback` to add the bindings to the patterns and cache the result.
    let mut build_node_callback =
        |graph: &mut FlowControlGraphBuilder<'db>, pattern_indices: FilteredPatterns| {
            cache.get_or_compute(
                build_node_callback,
                graph,
                pattern_indices.add_bindings(bindings.clone()),
            )
        };

    // Find the first non-any pattern, if exists.
    let Some(first_non_any_pattern_stable_ptr) = patterns.iter().find_map(|pattern| {
        try_extract_matches!(pattern_is_any(pattern), IsAny::Concrete)
            .map(|pattern| pattern.stable_ptr())
    }) else {
        // If all the patterns are catch-all, we do not need to look into `input_var`.
        // Call the callback with all patterns accepted.
        return build_node_callback(graph, FilteredPatterns::all(patterns.len()));
    };

    let var_ty = graph.var_ty(input_var);

    let params = CreateNodeParams {
        ctx,
        graph,
        patterns: &patterns,
        build_node_callback: &mut build_node_callback,
        location,
    };

    // Check if this is a numeric type that should use value matching.
    if corelib::numeric_upcastable_to_felt252(ctx.db, var_ty) {
        return create_node_for_value(params, input_var);
    }

    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, var_ty);
    match long_ty {
        TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) => {
            create_node_for_enum(params, input_var, concrete_enum_id, n_snapshots)
        }
        TypeLongId::Tuple(types) => create_node_for_tuple(params, input_var, &types, n_snapshots),
        _ => graph.report_with_missing_node(
            first_non_any_pattern_stable_ptr,
            LoweringDiagnosticKind::MatchError(MatchError {
                kind: graph.kind(),
                error: MatchDiagnostic::UnsupportedMatchedType(long_ty.format(ctx.db)),
            }),
        ),
    }
}

/// Creates an [EnumMatch] node for the given `input_var` and `patterns`.
fn create_node_for_enum<'db>(
    params: CreateNodeParams<'db, '_, '_>,
    input_var: FlowControlVar,
    concrete_enum_id: ConcreteEnumId<'db>,
    n_snapshots: usize,
) -> NodeId {
    let CreateNodeParams { ctx, graph, patterns, build_node_callback, location } = params;
    let concrete_variants = ctx.db.concrete_enum_variants(concrete_enum_id).unwrap();

    // Maps variant index to the list of the indices of the patterns that match it.
    let mut variant_to_pattern_indices = vec![FilteredPatterns::default(); concrete_variants.len()];

    // Maps variant index to the list of the inner patterns.
    // For example, a pattern `A(B(x))` will add the (inner) pattern `B(x)` to the vector at the
    // index of the variant `A`.
    let mut variant_to_inner_patterns: Vec<Vec<PatternOption<'_, 'db>>> =
        vec![vec![]; concrete_variants.len()];

    for (idx, pattern) in patterns.iter().enumerate() {
        match pattern {
            Some(semantic::Pattern::EnumVariant(PatternEnumVariant {
                variant,
                inner_pattern,
                ..
            })) => {
                variant_to_pattern_indices[variant.idx].add(idx);
                variant_to_inner_patterns[variant.idx]
                    .push(inner_pattern.map(|inner_pattern| get_pattern(ctx, inner_pattern)));
            }
            Some(semantic::Pattern::Otherwise(..)) | None => {
                // Add `idx` to all the variants.
                for pattern_indices in variant_to_pattern_indices.iter_mut() {
                    pattern_indices.add(idx);
                }
                // Add the `_` pattern (represented by `None`) to all the variants.
                for inner_patterns in variant_to_inner_patterns.iter_mut() {
                    inner_patterns.push(None);
                }
            }
            Some(semantic::Pattern::Variable(..)) => unreachable!(),
            _ => todo!("Pattern {:?} is not supported yet.", pattern),
        }
    }

    // Create a node in the graph for each variant.
    let variants =
        zip_eq3(concrete_variants, variant_to_pattern_indices, variant_to_inner_patterns)
            .map(|(concrete_variant, pattern_indices, inner_patterns)| {
                let inner_var = graph
                    .new_var(wrap_in_snapshots(ctx.db, concrete_variant.ty, n_snapshots), location);
                let node = create_node_for_patterns(
                    CreateNodeParams {
                        ctx,
                        graph,
                        patterns: &inner_patterns,
                        build_node_callback: &mut |graph, pattern_indices_inner| {
                            build_node_callback(graph, pattern_indices_inner.lift(&pattern_indices))
                        },
                        location,
                    },
                    inner_var,
                );
                (concrete_variant, node, inner_var)
            })
            .collect_vec();

    // Optimization: If all the variants lead to the same node, and the inner variables are not
    // used, there is no need to do the match.
    if let Some(first_variant) = variants.first() {
        let first_variant_node = first_variant.1;
        if variants.iter().all(|(_, node_id, inner_var)| {
            *node_id == first_variant_node && !graph.is_var_used(*inner_var)
        }) {
            return first_variant_node;
        }
    }

    // Create a node for the match.
    graph.add_node(FlowControlNode::EnumMatch(EnumMatch {
        matched_var: input_var,
        concrete_enum_id,
        variants,
    }))
}

/// Creates a [Deconstruct] node for the given `input_var` and `patterns`.
#[allow(clippy::too_many_arguments)]
fn create_node_for_tuple<'db>(
    params: CreateNodeParams<'db, '_, '_>,
    input_var: FlowControlVar,
    types: &Vec<TypeId<'db>>,
    n_snapshots: usize,
) -> NodeId {
    let CreateNodeParams { ctx, graph, patterns, build_node_callback, location } = params;
    let inner_vars = types
        .iter()
        .map(|ty| graph.new_var(wrap_in_snapshots(ctx.db, *ty, n_snapshots), location))
        .collect_vec();

    let node = create_node_for_tuple_inner(
        CreateNodeParams { ctx, graph, patterns, build_node_callback, location },
        &inner_vars,
        types,
        0,
    );

    // Deconstruct the input variable.
    graph.add_node(FlowControlNode::Deconstruct(Deconstruct {
        input: input_var,
        outputs: inner_vars,
        next: node,
    }))
}

/// Helper function for [create_node_for_tuple].
///
/// `item_idx` is the index of the current member that is being processed in the tuple.
#[allow(clippy::too_many_arguments)]
fn create_node_for_tuple_inner<'db>(
    params: CreateNodeParams<'db, '_, '_>,
    inner_vars: &Vec<FlowControlVar>,
    types: &Vec<TypeId<'db>>,
    item_idx: usize,
) -> NodeId {
    let CreateNodeParams { ctx, graph, patterns, build_node_callback, location } = params;

    if item_idx == types.len() {
        return build_node_callback(graph, FilteredPatterns::all(patterns.len()));
    }

    // Collect the patterns on the current item.
    let patterns_on_current_item = patterns
        .iter()
        .map(|pattern| match pattern {
            Some(semantic::Pattern::Tuple(PatternTuple { field_patterns, .. })) => {
                Some(get_pattern(ctx, field_patterns[item_idx]))
            }
            Some(semantic::Pattern::Otherwise(..)) | None => None,
            Some(_) => todo!(),
        })
        .collect_vec();

    // Create a node to handle the current item. The callback will handle the rest of the tuple.
    create_node_for_patterns(
        CreateNodeParams {
            ctx,
            graph,
            patterns: &patterns_on_current_item,
            build_node_callback: &mut |graph, pattern_indices| {
                // Call `create_node_for_tuple_inner` recursively to handle the rest of the tuple.
                create_node_for_tuple_inner(
                    CreateNodeParams {
                        ctx,
                        graph,
                        patterns: &pattern_indices.indices().map(|idx| patterns[idx]).collect_vec(),
                        build_node_callback: &mut |graph, pattern_indices_inner| {
                            build_node_callback(graph, pattern_indices_inner.lift(&pattern_indices))
                        },
                        location,
                    },
                    inner_vars,
                    types,
                    item_idx + 1,
                )
            },
            location,
        },
        inner_vars[item_idx],
    )
}

/// Creates a node for matching over numeric values, using [EqualsLiteral] nodes.
fn create_node_for_value<'db>(
    params: CreateNodeParams<'db, '_, '_>,
    input_var: FlowControlVar,
) -> NodeId {
    let CreateNodeParams { ctx: _, graph, patterns, build_node_callback, location: _ } = params;

    // A map from literals to their corresponding filter and the location of the first pattern with
    // this literal.
    let mut literals_map = OrderedHashMap::<usize, (FilteredPatterns, ExprPtr<'db>)>::default();

    // The filter of patterns that correspond to the otherwise patterns.
    let mut otherwise_filter = FilteredPatterns::default();

    // Go over the patterns, and update the maps.
    for (pattern_index, pattern) in patterns.iter().enumerate() {
        match pattern {
            Some(semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. })) => {
                // Extract the literal value as `usize`.
                let Some(literal_value) = literal.value.to_usize() else {
                    // TODO(lior): Report diagnostics.
                    todo!();
                };

                literals_map
                    .entry(literal_value)
                    .or_insert((otherwise_filter.clone(), literal.stable_ptr))
                    .0
                    .add(pattern_index);
            }
            Some(semantic::Pattern::Otherwise(_)) | None => {
                otherwise_filter.add(pattern_index);
                for (_, (filter, _)) in literals_map.iter_mut() {
                    filter.add(pattern_index);
                }
            }
            Some(semantic::Pattern::Variable(..)) => unreachable!(),
            _ => {
                // TODO(lior): Report `UnsupportedPattern` diagnostics.
                todo!();
            }
        }
    }

    // First, construct a node that handles the otherwise patterns.
    let mut current_node = build_node_callback(graph, otherwise_filter.clone());

    // Go over the literals (in reverse order), and construct a chain of [BooleanIf] nodes that
    // handle each literal.
    for (literal, (filter, stable_ptr)) in literals_map.into_iter().rev() {
        let node_if_literal = build_node_callback(graph, filter);

        // Don't add an [EqualsLiteral] node if both branches lead to the same node.
        if node_if_literal == current_node {
            continue;
        }

        current_node = graph.add_node(FlowControlNode::EqualsLiteral(EqualsLiteral {
            input: input_var,
            literal,
            stable_ptr,
            true_branch: node_if_literal,
            false_branch: current_node,
        }));
    }

    current_node
}

/// Whether a pattern accepts any value.
enum IsAny<'a, 'db> {
    /// The pattern accepts any value (`_` or a variable name).
    Any,
    /// Otherwise.
    Concrete(&'a semantic::Pattern<'db>),
}

/// Returns [IsAny::Any] if the pattern accepts any value (`_` or a variable name).
fn pattern_is_any<'a, 'db>(pattern: &PatternOption<'a, 'db>) -> IsAny<'a, 'db> {
    match pattern {
        Some(semantic_pattern) => match semantic_pattern {
            semantic::Pattern::Otherwise(..) | semantic::Pattern::Variable(..) => IsAny::Any,
            semantic::Pattern::Literal(..)
            | semantic::Pattern::StringLiteral(..)
            | semantic::Pattern::Struct(..)
            | semantic::Pattern::Tuple(..)
            | semantic::Pattern::FixedSizeArray(..)
            | semantic::Pattern::EnumVariant(..)
            | semantic::Pattern::Missing(..) => IsAny::Concrete(*semantic_pattern),
        },
        None => IsAny::Any,
    }
}

/// Returns a reference to a [semantic::Pattern] from a [semantic::PatternId].
pub fn get_pattern<'db, 'a>(
    ctx: &'a LoweringContext<'db, '_>,
    semantic_pattern: semantic::PatternId,
) -> &'a semantic::Pattern<'db> {
    &ctx.function_body.arenas.patterns[semantic_pattern]
}
