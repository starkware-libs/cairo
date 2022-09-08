use std::collections::HashMap;
use std::sync::Arc;

use super::{FilesGroup, ProjectConfig};
use crate::ids::{CrateLongId, FileLongId, VirtualFile};
use crate::test_utils::FilesDatabaseImpl;

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseImpl::default();

    let crt = db.intern_crate(CrateLongId("my_crate".into()));
    let crt2 = db.intern_crate(CrateLongId("my_crate2".into()));
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "root.cairo".into(),
        content: Arc::new("content\n".into()),
    }));

    db.set_project_config(ProjectConfig { crate_roots: HashMap::from([(crt, file)]) });

    assert_eq!(db.crate_root_file(crt), Some(file));
    assert_eq!(db.crate_root_file(crt2), None);

    assert_eq!(*db.file_content(file).unwrap(), "content\n");
}
