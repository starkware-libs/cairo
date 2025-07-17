// Test for macro output: ensure generated code does not use FileId<'a>::new(..)
// but rather FileId::new(..) or FileId::new<>()
mod logger_db;
use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_proc_macros::query_group;
use logger_db::LoggerDb;

#[salsa::interned(no_lifetime)]
pub struct LongFile {
    data: u32,
}

// pub trait B<'db>: salsa::Database {
//     // #[salsa::input]
//     fn file_id_input(&self) -> FileId<'db>;
//     fn inner_val(&'db self) -> u32 {
//         *self.file_id_input().0
//     }
// }

// impl<'db, D> B<'db> for D
// where
//     D: salsa::Database,
// {
//     fn file_id_input(&self) -> FileId<'db> {
//         // This is a placeholder implementation; in a real application, you would retrieve the
// input value from the database.         FileId(&42)
//     }
// }

#[salsa::interned]
pub struct I {
    val: u32,
}

#[query_group]
pub trait B: salsa::Database {
    #[salsa::input]
    fn file_id_input(&self) -> LongFile;

    fn inner_val(&self, id: LongFile) -> u32;

    #[salsa::interned]
    fn create_file_id(&self, val: u32) -> LongFile;

    fn do_something<'db>(&self, i: I<'db>) -> u32;

    fn do_arc<'db>(&'db self, i: I<'db>) -> Arc<u32>;

    fn arc_file_id<'db>(&'db self, x: u32) -> Arc<I<'db>>;
}

fn create_file_id(db: &dyn B, val: u32) -> LongFile {
    LongFile::new(db, val)
}

fn inner_val(db: &dyn B, id: LongFile) -> u32 {
    id.data(db)
}

fn do_something<'db>(db: &dyn B, i: I<'db>) -> u32 {
    i.val(db)
}

fn do_arc<'db>(db: &dyn B, i: I<'db>) -> Arc<u32> {
    Arc::new(i.val(db))
}

fn arc_file_id<'db>(db: &'db dyn B, x: u32) -> Arc<I<'db>> {
    Arc::new(I::new(db, x))
}

struct Other {
    x: Box<dyn Deref<Target = u32>>,
}

#[test]
fn test_fileid_new_call() {
    let mut db = LoggerDb::default();
    let val = 42;
    let arc = db.do_arc(I::new(&db, val));
    let file_id = create_file_id(&db, val);
    let other_obj = Other { x: Box::new(arc.clone()) };
    db.set_file_id_input(file_id);

    assert_eq!(inner_val(&db, file_id), 42);
    println!("FileId created with value: {}", **other_obj.x);
}
