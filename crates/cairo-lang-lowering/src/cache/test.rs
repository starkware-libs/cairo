use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_filesystem::db::{FilesGroup, files_group_input, set_crate_configs_input};
use cairo_lang_filesystem::ids::{BlobLongId, CodeMapping, FileId, FileKind, FileLongId};
use cairo_lang_filesystem::span::TextSpan;
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
    let db = &mut LoweringDatabaseForTesting::default();

    let function = &inputs["function_code"];
    let function_name = &inputs["function_name"];
    let module_code = inputs.get("module_code").map_or("", String::as_str);
    let (test_function, _semantic_diagnostics) =
        setup_test_function_ex(db, function, function_name, module_code, None, None).split();

    let artifact = generate_crate_cache(db, test_function.module_id.owning_crate(db)).unwrap();
    let core_artifact = generate_crate_cache(db, db.core_crate()).unwrap();
    let mut new_db = LoweringDatabaseForTesting::new();
    let crt = new_db.crate_input(new_db.core_crate());
    let mut crate_configs = files_group_input(&new_db).crate_configs(&new_db).clone().unwrap();
    let config = crate_configs.get_mut(crt).unwrap();
    config.cache_file = Some(BlobLongId::Virtual(core_artifact));
    set_crate_configs_input(&mut new_db, Some(crate_configs));

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

/// The db-independent fields of the `VirtualFile` backing an external (plugin-generated) file.
///
/// Compared across two databases, so it holds owned/interning-free values only: `full_path`
/// strings stand in for parent `FileId`s, which are interned per-db and thus not comparable.
#[derive(Clone, Debug, PartialEq)]
struct ExternalFileSnapshot {
    name: String,
    content: String,
    kind: FileKind,
    original_item_removed: bool,
    code_mappings: Vec<CodeMapping>,
    /// `(parent full path, span of this file within its parent)`.
    parent: Option<(String, TextSpan)>,
}

/// Snapshots the `VirtualFile` backing a single external file (as served by `ext_as_virtual`).
fn external_file_snapshot<'db>(
    db: &'db LoweringDatabaseForTesting,
    ext_file: FileId<'db>,
) -> ExternalFileSnapshot {
    let FileLongId::External(id) = ext_file.long(db) else {
        panic!("expected an external file, got {:?}", ext_file.long(db));
    };
    let vf = cairo_lang_filesystem::db::ext_as_virtual(db, *id);
    ExternalFileSnapshot {
        name: vf.name.to_string(db),
        content: vf.content.to_string(db),
        kind: vf.kind,
        original_item_removed: vf.original_item_removed,
        code_mappings: vf.code_mappings.to_vec(),
        parent: vf.parent.map(|p| (p.file_id.long(db).full_path(db), p.span)),
    }
}

/// Collects every external file backing a derive-generated impl in `module_id`, keyed by full
/// path (stable across databases). Also asserts each external root reached from the cache *is* the
/// canonical `file_syntax` root, i.e. no detached duplicate node was minted on load.
fn external_files<'db>(
    db: &'db LoweringDatabaseForTesting,
    module_id: ModuleId<'db>,
) -> OrderedHashMap<String, ExternalFileSnapshot> {
    let mut map = OrderedHashMap::default();
    for impl_id in db.module_impls_ids(module_id).unwrap() {
        let stable_ptr = impl_id.untyped_stable_ptr(db);
        let ext_file = stable_ptr.file_id(db);
        if !matches!(ext_file.long(db), FileLongId::External(_)) {
            continue;
        }
        let root = stable_ptr.0.ancestors_with_self(db).last().unwrap();
        assert_eq!(
            root,
            db.file_syntax(ext_file).unwrap(),
            "external root differs from file_syntax (detached node minted on load?)"
        );
        map.entry(ext_file.long(db).full_path(db))
            .or_insert_with(|| external_file_snapshot(db, ext_file));
    }
    map
}

/// Selector passed to [`assert_external_files_roundtrip`]: the crate-root module itself.
fn same_module<'db>(_db: &'db LoweringDatabaseForTesting, module: ModuleId<'db>) -> ModuleId<'db> {
    module
}

/// Selector passed to [`assert_external_files_roundtrip`]: `module`'s single submodule (the test
/// crates here have exactly one).
fn first_submodule<'db>(
    db: &'db LoweringDatabaseForTesting,
    module: ModuleId<'db>,
) -> ModuleId<'db> {
    let submodule =
        *db.module_submodules_ids(module).unwrap().first().expect("expected one submodule");
    ModuleId::Submodule(submodule)
}

/// Round-trips `module_code` through a crate cache and asserts the external (plugin-generated)
/// files in the module chosen by `select` reconstruct from the cache exactly as live plugin
/// generation produces them — by comparing `ext_as_virtual` on a cache-backed database against a
/// non-cached one.
fn assert_external_files_roundtrip(
    module_code: &str,
    min_files: usize,
    select: for<'db> fn(&'db LoweringDatabaseForTesting, ModuleId<'db>) -> ModuleId<'db>,
) {
    let function = "fn foo() {}";
    let function_name = "foo";

    // `live_db` never loads a cache: its external files are generated by the plugins on demand.
    let live_db = &mut LoweringDatabaseForTesting::default();
    let (live_function, _) =
        setup_test_function_ex(live_db, function, function_name, module_code, None, None).split();
    let artifact =
        generate_crate_cache(live_db, live_function.module_id.owning_crate(live_db)).unwrap();
    let core_artifact = generate_crate_cache(live_db, live_db.core_crate()).unwrap();

    // `cached_db` loads the crate (and corelib) caches generated above.
    let mut cached_db = LoweringDatabaseForTesting::new();
    let crt = cached_db.crate_input(cached_db.core_crate());
    let mut crate_configs =
        files_group_input(&cached_db).crate_configs(&cached_db).clone().unwrap();
    crate_configs.get_mut(crt).unwrap().cache_file = Some(BlobLongId::Virtual(core_artifact));
    set_crate_configs_input(&mut cached_db, Some(crate_configs));
    let cached_file = BlobLongId::Virtual(artifact).intern(&cached_db);
    let (cached_function, _) = setup_test_function_ex(
        &cached_db,
        function,
        function_name,
        module_code,
        None,
        Some(cached_file),
    )
    .split();

    let live = external_files(live_db, select(live_db, live_function.module_id));
    let cached = external_files(&cached_db, select(&cached_db, cached_function.module_id));

    assert!(
        cached.len() >= min_files,
        "expected at least {min_files} external file(s), found {}",
        cached.len()
    );
    assert_eq!(cached.len(), live.len(), "external file count differs from live generation");
    for (path, cached_snapshot) in cached.iter() {
        let live_snapshot = live
            .get(path)
            .unwrap_or_else(|| panic!("external file `{path}` is missing from live generation"));
        assert_eq!(
            cached_snapshot, live_snapshot,
            "external file `{path}` reconstructed from cache differs from live generation"
        );
    }
}

/// Loading external (plugin-generated) files from a crate cache must reconstruct exactly the
/// `VirtualFile` contents that re-running the plugins produces.
#[test]
fn cached_external_files_match_live_generation() {
    // Two derive-bearing items so more than one external file is exercised.
    let module_code = "\
#[derive(Drop)]
struct MyStruct {
    x: felt252,
}

#[derive(Drop)]
struct OtherStruct {
    y: u32,
}";
    assert_external_files_roundtrip(module_code, 2, same_module);
}

/// A derive inside a submodule: the generated external file's `module_id` is a `Submodule`, so
/// serving it from the cache exercises the recursive `owning_crate` walk in
/// `cached_external_virtual_file`.
#[test]
fn cached_submodule_external_files_match_live_generation() {
    let module_code = "\
mod inner {
    #[derive(Drop)]
    struct InnerStruct {
        x: felt252,
    }
}";
    assert_external_files_roundtrip(module_code, 1, first_submodule);
}
