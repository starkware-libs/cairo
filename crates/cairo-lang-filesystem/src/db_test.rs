use cairo_lang_test_utils::test;
use cairo_lang_utils::Intern;

use super::FilesGroup;
use crate::cfg::{Cfg, CfgSet};
use crate::db::{
    CrateConfiguration, FileContentView, override_file_content_for_input,
    set_generated_file_content_for_input,
};
use crate::flag::{Flag, FlagsGroup};
use crate::ids::{
    CrateLongId, Directory, FileId, FileKind, FileLongId, FlagId, FlagLongId, SmolStrId,
    VirtualFile,
};
use crate::set_crate_config;
use crate::test_utils::FilesDatabaseForTesting;

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseForTesting::default();

    let directory = Directory::Real("src".into());
    let child_str = "child.cairo";
    let db_ref = &mut db;
    let file_input = {
        let file_id = directory.file(db_ref, child_str);
        db_ref.file_input(file_id).clone()
    };
    override_file_content_for_input(db_ref, file_input, Some("content\n".into()));

    let config = CrateConfiguration::default_for_root(directory.clone());
    let crt = CrateLongId::plain(SmolStrId::from(db_ref, "my_crate")).intern(db_ref);
    set_crate_config!(db_ref, crt, Some(config.clone()));

    let crt = CrateLongId::plain(SmolStrId::from(&db, "my_crate")).intern(&db);
    let crt2 = CrateLongId::plain(SmolStrId::from(&db, "my_crate2")).intern(&db);

    assert_eq!(db.crate_config(crt), Some(&config));
    assert!(db.crate_config(crt2).is_none());

    let file_id = directory.file(&db, child_str);
    assert_eq!(db.file_content(file_id).unwrap(), "content\n");
}

fn vfs_file_id(db: &FilesDatabaseForTesting) -> FileId<'_> {
    make_vfs_file(db, "test.cairo", "original")
}

fn make_vfs_file<'db>(db: &'db FilesDatabaseForTesting, name: &str, content: &str) -> FileId<'db> {
    FileLongId::Virtual(VirtualFile {
        parent: None,
        name: SmolStrId::from(db, name),
        content: SmolStrId::from(db, content),
        code_mappings: [].into(),
        kind: FileKind::Module,
        original_item_removed: false,
    })
    .intern(db)
}

/// Tests that a `file_content` query first evaluated without a handle correctly picks up the
/// override once a handle is registered afterwards (the `report_untracked_read` invalidation path).
#[test]
fn test_file_content_handle_created_after_first_query() {
    let mut db = FilesDatabaseForTesting::default();

    // First query — no handle exists yet, returns the embedded virtual content.
    assert_eq!(db.file_content(vfs_file_id(&db)).unwrap(), "original");

    // Querying does not create handles — storage stays empty.
    assert_eq!(db.file_content_storage().read().unwrap().len(), 0);

    // Without any mutation, the query is memoized and returns the same result.
    // This confirms Salsa is actually caching between calls — if it weren't, the
    // post-handle assertion below would pass trivially regardless of our change.
    assert_eq!(db.file_content(vfs_file_id(&db)).unwrap(), "original");

    // Repeated querying still doesn't allocate handles.
    assert_eq!(db.file_content_storage().read().unwrap().len(), 0);

    // Register a handle with overridden content after the first query.
    let file_input = { db.file_input(vfs_file_id(&db)).clone() };
    set_generated_file_content_for_input(&mut db, file_input, Some("overridden".into()));

    // Exactly one handle exists now — one per file, no extras.
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    // Third query — must re-run via report_untracked_read and return the handle's content.
    assert_eq!(db.file_content(vfs_file_id(&db)).unwrap(), "overridden");

    // Re-execution doesn't allocate additional handles.
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);
}

/// Tests that two VFS files have independent file-content handles — assigning a handle to one
/// file does not affect the other.
#[test]
fn test_two_files_independent_handles() {
    let mut db = FilesDatabaseForTesting::default();

    // Both files start with no handles and return their embedded content.
    assert_eq!(db.file_content(make_vfs_file(&db, "a.cairo", "content_a")).unwrap(), "content_a");
    assert_eq!(db.file_content(make_vfs_file(&db, "b.cairo", "content_b")).unwrap(), "content_b");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 0);

    // Create a handle for file_a only.
    let file_a_input = { db.file_input(make_vfs_file(&db, "a.cairo", "content_a")).clone() };
    set_generated_file_content_for_input(&mut db, file_a_input, Some("override_a".into()));
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    // file_a now returns the overridden content.
    assert_eq!(db.file_content(make_vfs_file(&db, "a.cairo", "content_a")).unwrap(), "override_a");

    // file_b is unaffected — still returns its embedded content, no extra handle created.
    assert_eq!(db.file_content(make_vfs_file(&db, "b.cairo", "content_b")).unwrap(), "content_b");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);
}

/// Tests that multiple files can each hold their own handle simultaneously, and that updating one
/// does not disturb the others.
#[test]
fn test_multiple_handles_independent_content() {
    let mut db = FilesDatabaseForTesting::default();

    let file_a_input = { db.file_input(make_vfs_file(&db, "a.cairo", "raw_a")).clone() };
    let file_b_input = { db.file_input(make_vfs_file(&db, "b.cairo", "raw_b")).clone() };
    let file_c_input = { db.file_input(make_vfs_file(&db, "c.cairo", "raw_c")).clone() };

    set_generated_file_content_for_input(&mut db, file_a_input.clone(), Some("handle_a".into()));
    set_generated_file_content_for_input(&mut db, file_b_input.clone(), Some("handle_b".into()));
    set_generated_file_content_for_input(&mut db, file_c_input.clone(), Some("handle_c".into()));

    // All three handles are registered.
    assert_eq!(db.file_content_storage().read().unwrap().len(), 3);

    // Each file returns its own handle content.
    assert_eq!(db.file_content(make_vfs_file(&db, "a.cairo", "raw_a")).unwrap(), "handle_a");
    assert_eq!(db.file_content(make_vfs_file(&db, "b.cairo", "raw_b")).unwrap(), "handle_b");
    assert_eq!(db.file_content(make_vfs_file(&db, "c.cairo", "raw_c")).unwrap(), "handle_c");

    // Update one file — the other two are unaffected.
    set_generated_file_content_for_input(&mut db, file_b_input, Some("handle_b_v2".into()));
    assert_eq!(db.file_content_storage().read().unwrap().len(), 3);

    assert_eq!(db.file_content(make_vfs_file(&db, "a.cairo", "raw_a")).unwrap(), "handle_a");
    assert_eq!(db.file_content(make_vfs_file(&db, "b.cairo", "raw_b")).unwrap(), "handle_b_v2");
    assert_eq!(db.file_content(make_vfs_file(&db, "c.cairo", "raw_c")).unwrap(), "handle_c");

    // Clear one handle — that file falls back to its embedded content; count stays at 3.
    set_generated_file_content_for_input(&mut db, file_c_input, None);
    assert_eq!(db.file_content_storage().read().unwrap().len(), 3);

    assert_eq!(db.file_content(make_vfs_file(&db, "a.cairo", "raw_a")).unwrap(), "handle_a");
    assert_eq!(db.file_content(make_vfs_file(&db, "b.cairo", "raw_b")).unwrap(), "handle_b_v2");
    assert_eq!(db.file_content(make_vfs_file(&db, "c.cairo", "raw_c")).unwrap(), "raw_c");
}

/// Tests that updating a file's content multiple times reuses the same handle — no additional
/// handles are allocated per update.
#[test]
fn test_multiple_content_updates_single_handle() {
    let mut db = FilesDatabaseForTesting::default();

    let file_input = { db.file_input(make_vfs_file(&db, "f.cairo", "v0")).clone() };

    set_generated_file_content_for_input(&mut db, file_input.clone(), Some("v1".into()));
    assert_eq!(db.file_content(make_vfs_file(&db, "f.cairo", "v0")).unwrap(), "v1");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    set_generated_file_content_for_input(&mut db, file_input.clone(), Some("v2".into()));
    assert_eq!(db.file_content(make_vfs_file(&db, "f.cairo", "v0")).unwrap(), "v2");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    // Switching to the low-durability override path also reuses the handle.
    override_file_content_for_input(&mut db, file_input, Some("v3".into()));
    assert_eq!(db.file_content(make_vfs_file(&db, "f.cairo", "v0")).unwrap(), "v3");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);
}

/// Tests that a handle whose content is `None` does not prevent the query from falling back to
/// the embedded VFS content — and that subsequent updates are picked up correctly.
///
/// This exercises the code path introduced to fix the incorrect `report_untracked_read` call
/// when a handle exists but holds `None`.
#[test]
fn test_handle_with_none_content_falls_back_to_raw() {
    let mut db = FilesDatabaseForTesting::default();

    // Register a handle with no content override.
    let file_input = { db.file_input(make_vfs_file(&db, "g.cairo", "embedded")).clone() };
    set_generated_file_content_for_input(&mut db, file_input.clone(), None);
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    // With a None handle, the query falls back to the embedded VFS content.
    assert_eq!(db.file_content(make_vfs_file(&db, "g.cairo", "embedded")).unwrap(), "embedded");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    // Set actual content — Salsa picks it up via the per-file dependency (not
    // report_untracked_read).
    set_generated_file_content_for_input(&mut db, file_input.clone(), Some("overridden".into()));
    assert_eq!(db.file_content(make_vfs_file(&db, "g.cairo", "embedded")).unwrap(), "overridden");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);

    // Clearing back to None falls back to embedded content again — still exactly one handle.
    set_generated_file_content_for_input(&mut db, file_input, None);
    assert_eq!(db.file_content(make_vfs_file(&db, "g.cairo", "embedded")).unwrap(), "embedded");
    assert_eq!(db.file_content_storage().read().unwrap().len(), 1);
}

#[test]
fn test_flags() {
    let mut db = FilesDatabaseForTesting::default();

    let add_withdraw_gas_flag_id = FlagLongId(Flag::ADD_WITHDRAW_GAS.into());
    db.set_flag(add_withdraw_gas_flag_id.clone(), Some(Flag::AddWithdrawGas(false)));
    let id = add_withdraw_gas_flag_id.intern(&db);

    assert_eq!(db.get_flag(id), Some(Flag::AddWithdrawGas(false)));
    assert!(db.get_flag(FlagId::new(&db, FlagLongId("non_existing_flag".into()))).is_none());
}

#[test]
fn test_cfgs() {
    let mut db = FilesDatabaseForTesting::default();

    db.use_cfg(&CfgSet::from_iter([Cfg::name("test"), Cfg::kv("k", "v1")]));

    db.use_cfg(&CfgSet::from_iter([Cfg::kv("k", "v2")]));

    assert_eq!(
        db.cfg_set(),
        &CfgSet::from_iter([Cfg::name("test"), Cfg::kv("k", "v1"), Cfg::kv("k", "v2")])
    )
}
