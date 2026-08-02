use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_filesystem::db::{FilesGroup, files_group_input, set_crate_configs_input};
use cairo_lang_filesystem::ids::{BlobLongId, FileLongId};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::ConcreteFunctionWithBodyId as SemanticConcreteFunctionWithBodyId;
use cairo_lang_semantic::corelib::CorelibSemantic;
use cairo_lang_semantic::test_utils::{
    resolve_target_functions, setup_test_module_ex, test_module_code,
};
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
    // The very same module content must be used for generating the cache and for loading it, so
    // that the cached stable pointers match the module they are loaded into.
    let content = test_module_code(inputs);

    let (new_db, artifact) = generate_cached_db(&content);
    let cached_file = BlobLongId::Virtual(artifact).intern(&new_db);
    let (test_module, semantic_diagnostics) =
        setup_test_module_ex(&new_db, &content, None, Some(cached_file)).split();

    let function_id = ConcreteFunctionWithBodyId::from_semantic(
        &new_db,
        target_concrete_function(&new_db, test_module.module_id),
    );

    let lowered = new_db.lowered_body(function_id, LoweringStage::Final);
    if let Ok(lowered) = &lowered {
        assert!(
            lowered.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset flat blocks"
        );
    }
    let diagnostics = new_db.module_lowering_diagnostics(test_module.module_id).unwrap_or_default();
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

/// Returns the single function marked with `#[target_function]` in the given module, as a semantic
/// concrete function.
///
/// This mirrors what `setup_test_function` does, which can't be used here as it doesn't support
/// loading a crate cache.
fn target_concrete_function<'db>(
    db: &'db LoweringDatabaseForTesting,
    module_id: ModuleId<'db>,
) -> SemanticConcreteFunctionWithBodyId<'db> {
    let [free_function_id] = resolve_target_functions(db, module_id)[..] else {
        panic!("Expected exactly one function marked with `#[target_function]`.");
    };
    SemanticConcreteFunctionWithBodyId::from_no_generics_free(db, free_function_id).unwrap()
}

/// Compiles `content` to generate the crate cache (and the corelib cache), then returns a fresh db
/// with the corelib cache loaded plus the crate-cache artifact. Callers wire the artifact in as the
/// test crate's `cache_file` via
/// `setup_test_module_ex(.., Some(BlobLongId::Virtual(artifact).intern(&db)))`.
fn generate_cached_db(content: &str) -> (LoweringDatabaseForTesting, Vec<u8>) {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_module, _) = setup_test_module_ex(db, content, None, None).split();

    let artifact = generate_crate_cache(db, test_module.crate_id).unwrap();
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
    let content = "\
#[derive(Drop)]
struct MyStruct {
    x: felt252,
}";
    let (db, artifact) = generate_cached_db(content);
    let cached_file = BlobLongId::Virtual(artifact).intern(&db);
    let (cached_module, _) = setup_test_module_ex(&db, content, None, Some(cached_file)).split();

    // The `#[derive(Drop)]` impl lives in an external file; its cached stable ptr must resolve to
    // the canonical `file_syntax` root.
    let mut checked = 0;
    for impl_id in db.module_impls_ids(cached_module.module_id).unwrap() {
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
