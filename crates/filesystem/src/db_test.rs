use std::collections::HashMap;
use std::sync::Arc;

use super::{FilesGroup, ProjectConfig};
use crate::db::FilesGroupEx;
use crate::ids::{CrateLongId, FilesDir};
use crate::test_utils::FilesDatabaseForTesting;

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseForTesting::default();

    let crt = db.intern_crate(CrateLongId("my_crate".into()));
    let crt2 = db.intern_crate(CrateLongId("my_crate2".into()));
    let files_dir = FilesDir("src".into());
    let file_id = files_dir.child(&db, "child".into());
    db.override_file_content(file_id, Some(Arc::new("content\n".into())));
    db.set_project_config(ProjectConfig { crate_roots: HashMap::from([(crt, files_dir.clone())]) });

    assert_eq!(db.crate_root_dir(crt), Some(files_dir));
    assert_eq!(db.crate_root_dir(crt2), None);

    assert_eq!(*db.file_content(file_id).unwrap(), "content\n");
}
