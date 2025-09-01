use expect_test::expect;

mod logger_db;
use logger_db::LoggerDb;

#[salsa::interned(no_lifetime, revisions = usize::MAX)]
pub struct InternedWithDefaults {
    data: String,
    debug: Option<String>,
}

#[cairo_lang_proc_macros::query_group]
pub trait InternedWithDefaultsDB: salsa::Database {
    #[salsa::interned(revisions = usize::MAX)]
    #[default_values(debug=None)]
    fn intern_with_defaults(&self, data: String) -> InternedWithDefaults;

    fn interned_data_len(&self, id: InternedWithDefaults) -> usize;
}

fn interned_data_len(db: &dyn InternedWithDefaultsDB, id: InternedWithDefaults) -> usize {
    let interned = db.lookup_intern_with_defaults(id);
    interned.len()
}

#[test]
fn intern_with_default_values() {
    let db = LoggerDb::default();

    // Should create interned value with debug=None automatically
    let id = db.intern_with_defaults(String::from("Hello, world!"));

    assert_eq!(id.data(&db), "Hello, world!");
    assert_eq!(id.debug(&db), None);

    let interned_str = db.lookup_intern_with_defaults(id);
    assert_eq!(interned_str, "Hello, world!");

    db.assert_logs(expect![[r#"[]"#]]);
}
