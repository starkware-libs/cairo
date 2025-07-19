use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::test_utils::{TestFunction, setup_test_function};
use cairo_lang_semantic::{self as semantic, ExprVarMemberPath};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use super::create_graph::{create_graph_expr_if, create_graph_expr_match};
use super::graph::FlowControlGraph;
use super::lower_graph::lower_graph;
use super::test_utils::format_graph;
use crate::Lowered;
use crate::db::LoweringGroup;
use crate::ids::{FunctionWithBodyLongId, Signature};
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{EncapsulatingLoweringContext, LoweringContext, VarRequest};
use crate::lower::{
    alloc_empty_block, lowered_expr_to_block_scope_end, wrap_sealed_block_as_function,
};
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    create_graph,
    "src/lower/flow_control/test_data",
    {
        if_: "if",
        match_: "match",
    },
    test_create_graph
);

fn test_create_graph(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function_code"].as_str(),
        "foo",
        inputs["module_code"].as_str(),
    )
    .split();

    println!("{}", semantic_diagnostics); // TODO: Remove.

    let semantic::Expr::Block(semantic::ExprBlock { tail: Some(expr_id), .. }) =
        db.expr_semantic(test_function.function_id, test_function.body)
    else {
        panic!("Expected a block expression.");
    };

    let expr = db.expr_semantic(test_function.function_id, expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };

    let mut encapsulating_ctx =
        EncapsulatingLoweringContext::new(db, test_function.function_id).unwrap();
    let mut ctx = lowering_context(db, &test_function, &mut encapsulating_ctx);

    let graph = match &expr {
        semantic::Expr::If(expr) => create_graph_expr_if(&mut ctx, expr),
        semantic::Expr::Match(expr) => create_graph_expr_match(&mut ctx, expr),
        _ => {
            panic!("Unsupported expression: {:?}", expr.debug(&expr_formatter));
        }
    };

    let error = verify_diagnostics_expectation(args, &semantic_diagnostics);

    // Lower the graph.
    let lowered_str = if args["skip_lowering"] == "true" {
        "".into()
    } else {
        let lowered = lower_graph_as_function(ctx, expr_id, &graph);
        formatted_lowered(db, Some(&lowered))
    };

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("graph".into(), format_graph(&graph)),
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowered".into(), lowered_str),
        ]),
        error,
    }
}

fn lowering_context<'a, 'db>(
    db: &'db LoweringDatabaseForTesting,
    test_function: &TestFunction,
    encapsulating_ctx: &'a mut EncapsulatingLoweringContext<'db>,
) -> LoweringContext<'a, 'db> {
    let lowering_function_id = db.intern_lowering_function_with_body(
        FunctionWithBodyLongId::Semantic(test_function.function_id),
    );

    let lowering_signature = Signature::from_semantic(db, test_function.signature.clone());
    let return_type = lowering_signature.return_type;

    for semantic_var in &test_function.signature.params {
        encapsulating_ctx.semantic_defs.insert(
            semantic::VarId::Param(semantic_var.id),
            semantic::Binding::Param(semantic_var.clone()),
        );
    }

    LoweringContext::new(encapsulating_ctx, lowering_function_id, lowering_signature, return_type)
        .unwrap()
}

fn lower_graph_as_function(
    mut ctx: LoweringContext<'_, '_>,
    // db: &LoweringDatabaseForTesting,
    // test_function: &TestFunction,
    expr_id: semantic::ExprId,
    graph: &FlowControlGraph,
) -> Lowered {
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(root_block_id);

    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            // TODO(spapini): Introduce member paths, not just base variables.
            let param_var = extract_matches!(param, ExprVarMemberPath::Var);
            builder.put_semantic(param_var.var, var);
            var
        })
        .collect_vec();

    let block_expr = lower_graph(&mut ctx, &mut builder, &graph);

    let block_sealed = lowered_expr_to_block_scope_end(&mut ctx, builder, block_expr).unwrap();

    let expr = ctx.function_body.arenas.exprs[expr_id].clone();

    wrap_sealed_block_as_function(&mut ctx, block_sealed, expr.stable_ptr().untyped()).unwrap();

    let blocks = ctx.blocks.build().expect("Root block must exist.");
    Lowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.clone(),
        parameters,
    }
}
