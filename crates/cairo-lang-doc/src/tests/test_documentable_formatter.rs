use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::tests::test_utils::{TestDatabase, setup_test_module};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LookupItemId;
use itertools::Itertools;
use std::fs;

#[test]
fn reconstruct_signature_form_signature_elements_vector() {
    let mut db_val = TestDatabase::new().unwrap();
    let input = fs::read_to_string("src/tests/test-data/signature_elements.cairo")
        .expect("Should have been able to read the file");

    let crate_id = setup_test_module(&mut db_val, input.as_str());
    let db = &db_val;

    let modules = db.crate_modules(crate_id);
    modules.iter().for_each(|m| {
        db.module_items(m.clone()).unwrap().iter().for_each(|i| {
            let documentable_id = DocumentableItemId::from(LookupItemId::ModuleItem(*i));
            let (signature, elements) = db.get_item_signature_with_elements(documentable_id);
            let res = elements.into_iter().map(|e| e.signature).join("");
            assert_eq!(signature, res)
        })
    });
}
