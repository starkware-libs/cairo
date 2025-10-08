use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::corelib::unit_ty;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::test_utils::{TestFunction, setup_test_function};
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{self as semantic, Expr, Statement, StatementId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::Itertools;

use super::block_builder::{BlockBuilder, merge_block_builders};
use super::context::{LoweringContext, VarRequest};
use super::test_utils::{create_encapsulating_ctx, create_lowering_context};
use crate::VariableId;
use crate::fmt::LoweredFormatter;
use crate::test_utils::LoweringDatabaseForTesting;

const N_LOWERING_VARS: usize = 100;

cairo_lang_test_utils::test_file_test!(
    test_merge_block_builders,
    "src/lower/test_data",
    {
        merge_block_builders: "merge_block_builders",
    },
    test_merge_block_builders
);

/// Tests the [merge_block_builders] function.
///
/// Each test case has the following input sections:
/// - `variables`: A comma-separated list of "var_name: type".
/// - `block_definitions`: Defines the semantic to lowering map of each input block.
///
///   For example:
///   ```ignore
///      ((x, 0),);
///      ((x.a, 1), (y, 2));
///   ```
///   represents two blocks, where:
///   * The first maps `x` to lowered variable 0.
///   * The second maps `x.a` to lowered variable 1, and `y` to lowered variable 2.
///
///   Note that `x` and `x.*` should not be specified together in one block.
///
/// - `module_code`: Additional code for defining structs and helper functions.
fn test_merge_block_builders(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = LoweringDatabaseForTesting::default();
    // Create a function with the given variables as parameters and the given block definitions
    // as a dummy function body.
    // Note that the function body is not lowered at any point, and it is only used to parse the
    // semantic to lowering map.
    let test_function = setup_test_function(
        &db,
        &format!("fn foo ({}) {{ {} }}", &inputs["variables"], &inputs["block_definitions"]),
        "foo",
        inputs.get("module_code").unwrap_or(&"".into()),
    )
    .unwrap();

    let mut encapsulating_ctx =
        create_encapsulating_ctx(&db, test_function.function_id, &test_function.signature);

    let mut ctx = create_lowering_context(
        &db,
        test_function.function_id,
        &test_function.signature,
        &mut encapsulating_ctx,
    );

    // Create dummy lowering variables.
    let dummy_location = ctx.get_location(test_function.signature.stable_ptr.untyped());
    let lowering_vars: Vec<VariableId> = (0..N_LOWERING_VARS)
        .map(|_| ctx.new_var(VarRequest { ty: unit_ty(ctx.db), location: dummy_location }))
        .collect();

    let expr_formatter = ExprFormatter { db: &db, function_id: test_function.function_id };

    let input_blocks = create_block_builders(&mut ctx, &test_function, &lowering_vars);
    let input_blocks_str =
        input_blocks.iter().map(|b| format!("{:?}", b.debug(&expr_formatter))).join("\n");

    // Invoke [merge_block_builders] on the input blocks.
    let merged_block = merge_block_builders(&mut ctx, input_blocks, dummy_location);

    let lowered_formatter = LoweredFormatter::new(&db, &ctx.variables.variables);
    let lowered_blocks = ctx.blocks.build().unwrap();
    let lowered_str = lowered_blocks
        .iter()
        .map(|(block_id, block)| format!("{block_id:?}:\n{:?}\n", block.debug(&lowered_formatter)))
        .join("");

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("input_blocks".into(), input_blocks_str),
            ("merged_block_builder".into(), format!("{:?}", merged_block.debug(&expr_formatter))),
            ("lowered".into(), lowered_str),
        ]),
        error: None,
    }
}

/// Creates a block builder for each semantic "statement" in the function body.
///
/// See [create_block_builder] for more details.
fn create_block_builders<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    test_function: &TestFunction<'db>,
    lowering_vars: &[VariableId],
) -> Vec<BlockBuilder<'db>> {
    let expr = ctx.function_body.arenas.exprs[test_function.body].clone();
    let block_expr = extract_matches!(expr, Expr::Block);

    block_expr
        .statements
        .iter()
        .map(|statement_id| create_block_builder(ctx, *statement_id, lowering_vars))
        .collect()
}

/// Given a semantic "statement" of the form:
///    `((member_path, lower_var_idx), ...)`
/// creates a block builder with a semantic mapping that maps each member path to the corresponding
/// given lowered variable.
///
/// Assumption: if a certain semantic variable is mapped, all its children should not be mapped.
///
/// Note that the statement is not a real statement - it is not lowered, and it is only used to
/// define the semantic mapping.
fn create_block_builder<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    statement_id: StatementId,
    lowering_vars: &[VariableId],
) -> BlockBuilder<'db> {
    let block_id = ctx.blocks.alloc_empty();
    let mut block_builder = BlockBuilder::root(block_id);
    let mut visited_vars: UnorderedHashSet<semantic::VarId<'_>> = Default::default();

    let statement_expr =
        extract_matches!(&ctx.function_body.arenas.statements[statement_id], Statement::Expr);
    let external_tuple =
        extract_matches!(&ctx.function_body.arenas.exprs[statement_expr.expr], Expr::Tuple);

    let expr_ids = external_tuple.items.clone();
    for expr_id in expr_ids {
        let inner_tuple = extract_matches!(&ctx.function_body.arenas.exprs[expr_id], Expr::Tuple);
        let lower_var_idx: usize = (&extract_matches!(
            &ctx.function_body.arenas.exprs[inner_tuple.items[1]],
            Expr::Literal
        )
        .value)
            .try_into()
            .unwrap();

        match &ctx.function_body.arenas.exprs[inner_tuple.items[0]] {
            Expr::MemberAccess(member_access) => {
                let member_path: MemberPath<'_> =
                    (member_access.member_path.as_ref().unwrap()).into();
                let mut var = &member_path;
                while let MemberPath::Member { parent: v, .. } = var {
                    var = v;
                }
                let var_id = extract_matches!(var, MemberPath::Var);

                if visited_vars.insert(*var_id) {
                    block_builder.put_semantic(*var_id, lowering_vars[lower_var_idx]);
                }

                let location = ctx.get_location(member_access.stable_ptr.untyped());
                block_builder.update_ref_raw(
                    ctx,
                    member_path,
                    lowering_vars[lower_var_idx],
                    location,
                );
                // Remove the statements that were created as part of the `update_ref_raw` call.
                block_builder.statements.statements.clear();
            }
            Expr::Var(var) => {
                if visited_vars.insert(var.var) {
                    block_builder.put_semantic(var.var, lowering_vars[lower_var_idx]);
                }
            }
            expr => {
                panic!("Unexpected expression: {expr:?}");
            }
        }
    }

    block_builder
}
