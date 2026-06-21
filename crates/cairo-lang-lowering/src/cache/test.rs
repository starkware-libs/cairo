use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_filesystem::db::{FilesGroup, files_group_input, set_crate_configs_input};
use cairo_lang_filesystem::ids::{BlobLongId, FileLongId};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::corelib::CorelibSemantic;
use cairo_lang_semantic::test_utils::setup_test_function_ex;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::generate_crate_cache;
use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    cache,
    "src/cache/test_data",
    {
        cache: "cache",
    },
    test_cache_check
);

fn test_cache_check(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let function = &inputs["function_code"];
    let function_name = &inputs["function_name"];
    let module_code = inputs.get("module_code").map_or("", String::as_str);

    let (new_db, artifact) = generate_cached_db(function, function_name, module_code);
    let cached_file = BlobLongId::Virtual(artifact).intern(&new_db);
    let (test_function, semantic_diagnostics) = setup_test_function_ex(
        &new_db,
        function,
        function_name,
        module_code,
        None,
        Some(cached_file),
    )
    .split();

    let function_id: ConcreteFunctionWithBodyId<'_> =
        ConcreteFunctionWithBodyId::from_semantic(&new_db, test_function.concrete_function_id);

    let lowered = new_db.lowered_body(function_id, LoweringStage::Final);
    if let Ok(lowered) = &lowered {
        assert!(
            lowered.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset flat blocks"
        );
    }
    let diagnostics =
        new_db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();
    let formatted_lowering_diagnostics = diagnostics.format(&new_db);
    let combined_diagnostics = format!("{semantic_diagnostics}\n{formatted_lowering_diagnostics}");
    let error = verify_diagnostics_expectation(args, &combined_diagnostics);
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), formatted_lowering_diagnostics),
            ("lowering_flat".into(), formatted_lowered(&new_db, lowered.ok())),
        ]),
        error,
    }
}

/// Compiles `function`/`module_code` to generate the crate cache (and the corelib cache), then
/// returns a fresh db with the corelib cache loaded plus the crate-cache artifact. Callers wire the
/// artifact in as the test crate's `cache_file` via
/// `setup_test_function_ex(.., Some(BlobLongId::Virtual(artifact).intern(&db)))`.
fn generate_cached_db(
    function: &str,
    function_name: &str,
    module_code: &str,
) -> (LoweringDatabaseForTesting, Vec<u8>) {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, _) =
        setup_test_function_ex(db, function, function_name, module_code, None, None).split();

    let artifact = generate_crate_cache(db, test_function.module_id.owning_crate(db)).unwrap();
    let core_artifact = generate_crate_cache(db, db.core_crate()).unwrap();

    let mut new_db = LoweringDatabaseForTesting::new();
    let crt = new_db.crate_input(new_db.core_crate());
    let mut crate_configs = files_group_input(&new_db).crate_configs(&new_db).clone().unwrap();
    crate_configs.get_mut(crt).unwrap().cache_file = Some(BlobLongId::Virtual(core_artifact));
    set_crate_configs_input(&mut new_db, Some(crate_configs));
    (new_db, artifact)
}

/// File syntax roots are canonical (file-keyed via `SyntaxNode::new_canonical_root`). For an
/// external (plugin-generated) file restored from a crate cache, the root reached from a cached
/// stable ptr must therefore be the very node `db.file_syntax` mints — not a detached duplicate.
#[test]
fn cached_external_file_root_is_canonical() {
    let function = "fn foo() {}";
    let module_code = "\
#[derive(Drop)]
struct MyStruct {
    x: felt252,
}";
    let (db, artifact) = generate_cached_db(function, "foo", module_code);
    let cached_file = BlobLongId::Virtual(artifact).intern(&db);
    let (cached_function, _) =
        setup_test_function_ex(&db, function, "foo", module_code, None, Some(cached_file)).split();

    // The `#[derive(Drop)]` impl lives in an external file; its cached stable ptr must resolve to
    // the canonical `file_syntax` root.
    let mut checked = 0;
    for impl_id in db.module_impls_ids(cached_function.module_id).unwrap() {
        let stable_ptr = impl_id.untyped_stable_ptr(&db);
        let ext_file = stable_ptr.file_id(&db);
        if !matches!(ext_file.long(&db), FileLongId::External(_)) {
            continue;
        }
        let root = stable_ptr.0.ancestors_with_self(&db).last().unwrap();
        assert_eq!(
            root,
            db.file_syntax(ext_file).unwrap(),
            "external root differs from file_syntax (detached node minted on load?)"
        );
        checked += 1;
    }
    assert!(checked > 0, "expected at least one external (derive-generated) file");
}
