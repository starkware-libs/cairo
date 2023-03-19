use std::sync::Arc;

use cairo_lang_utils::Upcast;
use test_log::test;

use super::FilesGroup;
use crate::db::FilesGroupEx;
use crate::flag::Flag;
use crate::ids::{CrateLongId, Directory, FlagId};
use crate::test_utils::FilesDatabaseForTesting;

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseForTesting::default();

    let crt = db.intern_crate(CrateLongId("my_crate".into()));
    let crt2 = db.intern_crate(CrateLongId("my_crate2".into()));
    let directory = Directory("src".into());
    let file_id = directory.file(&db, "child.cairo".into());
    db.override_file_content(file_id, Some(Arc::new("content\n".into())));
    db.set_crate_root(crt, Some(directory.clone()));

    assert_eq!(db.crate_root_dir(crt), Some(directory));
    assert!(db.crate_root_dir(crt2).is_none());

    assert_eq!(*db.file_content(file_id).unwrap(), "content\n");
}

#[test]
fn test_flags() {
    let mut db = FilesDatabaseForTesting::default();

    let add_get_gas_flag_id = FlagId::new(db.upcast(), "add_get_gas");

    db.set_flag(add_get_gas_flag_id, Some(Arc::new(Flag::AddGetGas(true))));

    assert_eq!(*db.get_flag(add_get_gas_flag_id).unwrap(), Flag::AddGetGas(true));
    assert!(db.get_flag(FlagId::new(db.upcast(), "non_existing_flag")).is_none());
}
