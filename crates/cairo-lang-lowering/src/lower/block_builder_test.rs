use crate::{db::LoweringGroup, fmt::LoweredFormatter, ids::{FunctionWithBodyLongId, Signature}, test_utils::LoweringDatabaseForTesting, BlockId, VariableId};
use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::{self as semantic, corelib::unit_ty, test_utils::{setup_test_function, TestFunction}, ExprVarMemberPath};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::{extract_matches, ordered_hash_map::OrderedHashMap, Upcast};
use itertools::Itertools;
use salsa::{InternId, InternKey};

use super::{block_builder::{merge_block_builders, BlockBuilder}, context::{EncapsulatingLoweringContext, LoweringContext, VarRequest}};

cairo_lang_test_utils::test_file_test!(
    merge_block_builders,
    "src/lower/test_data",
    {
        merge_block_builders: "merge_block_builders",
    },
    test_merge_block_builders
);

fn semantic_param(id: u32) -> semantic::VarId {
    semantic::VarId::Param(semantic::ParamId::from_intern_id(id.into()))
}

fn create_context<'a, 'db>(db: &'db LoweringDatabaseForTesting, test_function: &TestFunction,
    encapsulating_ctx: &'a mut EncapsulatingLoweringContext<'db>) -> LoweringContext<'a, 'db> {

    let lowering_signature = Signature::from_semantic(db, test_function.signature.clone());
    let return_type = lowering_signature.return_type;

    let lowering_function_id = db.intern_lowering_function_with_body(
        FunctionWithBodyLongId::Semantic(test_function.function_id),
    );
    LoweringContext::new(
        encapsulating_ctx,
        lowering_function_id,
        lowering_signature,
        return_type,
    )
    .unwrap()
}

fn test_merge_block_builders(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = LoweringDatabaseForTesting::default();
    let test_function = setup_test_function(
        &db,
        format!("{} {{}}", inputs["function_signature"]).as_str(),
        "foo",
        inputs.get("module_code").unwrap_or(&"".into())
    ).unwrap();
    let mut encapsulating_ctx =
         EncapsulatingLoweringContext::new(&db, test_function.function_id).unwrap();

         for semantic_var in &test_function.signature.params {
            encapsulating_ctx.semantic_defs.insert(
                semantic::VarId::Param(semantic_var.id),
                semantic::Binding::Param(semantic_var.clone()),
            );
        }

    let mut ctx = create_context(&db, &test_function, &mut encapsulating_ctx);

    let parameters: Vec<VariableId> = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            var
        })
        .collect();

    println!("semantic_defs: {:?}", ctx.semantic_defs);

    let dummy_location = ctx.get_location(test_function.signature.stable_ptr.untyped());

    let input_blocks = parse_block_builders(&mut ctx, &inputs, &parameters);
    let input_blocks_str = input_blocks.iter().map(|b| b.to_string()).join("\n");

    let merged_block = merge_block_builders(&mut ctx, input_blocks, dummy_location);

    let lowered_formatter = LoweredFormatter::new(db.upcast(), &ctx.variables.variables);
    let lowered_str = ctx.blocks.build().unwrap().iter().map(|(block_id, block)|
        format!("{:?}:\n{:?}\n", block_id.debug(&lowered_formatter),  block.debug(&lowered_formatter))
    ).join("");

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("input_blocks".into(), input_blocks_str),
            ("lowered".into(), lowered_str),
            ("merged_block_builder".into(), merged_block.to_string()),
        ]),
        error: None,
    }
}

fn parse_block_builders(ctx: &mut LoweringContext, inputs: &OrderedHashMap<String, String>, parameters: &Vec<VariableId>) -> Vec<BlockBuilder> {
    let n_blocks = inputs["n_blocks"].parse::<usize>().unwrap_or(0);
    (0..n_blocks).map(|idx| parse_block_builder(ctx, inputs, idx, parameters)).collect()
}

fn parse_block_builder(ctx: &mut LoweringContext, inputs: &OrderedHashMap<String, String>, idx: usize, parameters: &Vec<VariableId>) -> BlockBuilder {
    let block_id = ctx.blocks.alloc_empty();
    let mut block_builder = BlockBuilder::root(block_id);

    for line in inputs[&format!("block_{idx}_semantic_vars")].lines() {
        let line = line.split_once(" //").map(|(x, _)| x).unwrap_or(line);
        let (var_id_str, param_idx_str) = line.split_once(": ").unwrap();
        let var_id = var_id_str.parse::<u32>().unwrap_or(0);
        let param_idx = param_idx_str.parse::<usize>().unwrap_or(0);

        block_builder.put_semantic(semantic_param(var_id), parameters[param_idx]);
    }

    block_builder
}
