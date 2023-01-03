use std::sync::Arc;

use test_log::test;

use super::FilesGroup;
use crate::db::FilesGroupEx;
use crate::ids::{CrateLongId, Directory};
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
    assert_eq!(db.crate_root_dir(crt2), None);

    assert_eq!(*db.file_content(file_id).unwrap(), "content\n");
}
