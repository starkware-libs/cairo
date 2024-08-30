use crate::{
    db::{DocGroup, Documentation},
    documentable_item::{DocumentableItemId, DocumentableModuleItemId},
};
use cairo_lang_defs::{
    db::DefsGroup,
    ids::{ImplItemId, LookupItemId, ModuleId, ModuleItemId, TraitItemId},
};
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::{extract_matches, LookupIntern};

use super::test_utils::{set_file_content, setup_test_module, TestDatabase};

const PREFIX_AND_INNER_COMMENTS_TEST_CODE: &str =
    include_str!("test-code/prefix_and_inner_comments.cairo");
const CRATE_COMMENTS_TEST_CODE: &str = include_str!("test-code/crate_comments.cairo");
const SUBMODULE_FILE_COMMENTS: &str = include_str!("test-code/submodule_file_comments.cairo");

#[test]
fn test_module_level_documentation() {
    let mut db_val = TestDatabase::default();
    let crate_id = setup_test_module(&mut db_val, CRATE_COMMENTS_TEST_CODE);
    let db = &mut db_val;

    let main_module_id = ModuleId::CrateRoot(crate_id);
    assert_eq!(
        db.module_main_file(main_module_id).unwrap().lookup_intern(db),
        FileLongId::OnDisk("src/lib.cairo".into())
    );

    set_file_content(db, "src/submodule_file_comments.cairo", SUBMODULE_FILE_COMMENTS);

    let submodule_id = *db.module_submodules_ids(main_module_id).unwrap().first().unwrap();

    assert_eq!(
        db.module_main_file(ModuleId::Submodule(submodule_id)).unwrap().lookup_intern(db),
        FileLongId::OnDisk("src/submodule_file_comments.cairo".into())
    );

    let crate_documentation = db.get_item_documentation(DocumentableItemId::Crate(crate_id));

    let submodule_documentation =
        db.get_item_documentation(DocumentableItemId::from(DocumentableModuleItemId::from(
            LookupItemId::ModuleItem(ModuleItemId::Submodule(submodule_id)),
        )));

    assert_eq!(
        crate_documentation,
        Documentation {
          prefix_comments: None,
          inner_comments: None,
          module_level_comments: Some(String::from("This is a testing crate file. It's for the tests purposes only.\nWe don't take responsibility for compiling this file.\nSo don't even try.\n")),
        }
    );

    assert_eq!(
        submodule_documentation,
        Documentation {
            prefix_comments: Some(String::from("This one is just a prefix comment for a module.\n")),
            inner_comments: None,
            module_level_comments: Some(String::from("This is a submodule regarding the module_level_comments.\nIt's used to make sure crate / module level comments are parsed in a correct way.\nTesting purposes only!\n"))
        }
    )
}

#[test]
fn test_prefix_and_inner_comments() {
    let mut db_val = TestDatabase::new().unwrap();
    let crate_id = setup_test_module(&mut db_val, PREFIX_AND_INNER_COMMENTS_TEST_CODE);
    let db = &db_val;

    let main_module_id = ModuleId::CrateRoot(crate_id);
    let module_items = db.module_items(main_module_id).unwrap();

    assert_eq!(
        db.module_main_file(main_module_id).unwrap().lookup_intern(db),
        FileLongId::OnDisk("src/lib.cairo".into())
    );

    let main_function_id = extract_matches!(module_items[0], ModuleItemId::FreeFunction);
    let trait_id = extract_matches!(module_items[1], ModuleItemId::Trait);
    let impl_id = extract_matches!(module_items[2], ModuleItemId::Impl);
    let submodule_id = extract_matches!(module_items[3], ModuleItemId::Submodule);
    let struct_id = extract_matches!(module_items[4], ModuleItemId::Struct);
    let enum_id = extract_matches!(module_items[5], ModuleItemId::Enum);

    let trait_function_id = db.trait_functions(trait_id).unwrap().into_iter().next().unwrap().1;

    let impl_function_id =
        db.impl_function_by_trait_function(impl_id, trait_function_id).unwrap().unwrap();

    let submodule_items = db.module_items(ModuleId::Submodule(submodule_id)).unwrap();

    let submodule_function_id = extract_matches!(submodule_items[0], ModuleItemId::FreeFunction);

    let struct_members = db.struct_members(struct_id).unwrap();

    let struct_member_ids: Vec<_> = struct_members.iter().collect();
    // let member_1_id = struct_member_ids.next().unwrap().1.id;
    // let member_2_id = struct_member_ids.next().unwrap().1.id;
    let member_1_id = struct_member_ids[0].1.id;
    let member_2_id = struct_member_ids[1].1.id;

    let main_function_documentation =
        db.get_item_documentation(DocumentableItemId::from(DocumentableModuleItemId::from(
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(main_function_id)),
        )));

    let trait_documentation = db.get_item_documentation(DocumentableItemId::from(
        DocumentableModuleItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id))),
    ));

    let trait_function_documentation =
        db.get_item_documentation(DocumentableItemId::from(DocumentableModuleItemId::from(
            LookupItemId::TraitItem(TraitItemId::Function(trait_function_id)),
        )));

    let impl_documentation = db.get_item_documentation(DocumentableItemId::from(
        DocumentableModuleItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(impl_id))),
    ));

    let impl_function_documentation =
        db.get_item_documentation(DocumentableItemId::from(DocumentableModuleItemId::from(
            LookupItemId::ImplItem(ImplItemId::Function(impl_function_id)),
        )));

    let submodule_documentation =
        db.get_item_documentation(DocumentableItemId::from(DocumentableModuleItemId::from(
            LookupItemId::ModuleItem(ModuleItemId::Submodule(submodule_id)),
        )));

    let submodule_function_documentation =
        db.get_item_documentation(DocumentableItemId::from(DocumentableModuleItemId::from(
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(submodule_function_id)),
        )));

    let struct_documentation = db.get_item_documentation(DocumentableItemId::from(
        DocumentableModuleItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(struct_id))),
    ));

    let member_1_documentation = db.get_item_documentation(DocumentableItemId::from(
        DocumentableModuleItemId::from(member_1_id),
    ));

    let member_2_documentation = db.get_item_documentation(DocumentableItemId::from(
        DocumentableModuleItemId::from(member_2_id),
    ));

    let enum_documentation = db.get_item_documentation(DocumentableItemId::from(
        DocumentableModuleItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id))),
    ));

    assert_eq!(
        main_function_documentation,
        Documentation {
            prefix_comments: Some(String::from("Main function comment outside.\n")),
            inner_comments: Some(String::from("Main function comment inside.\n")),
            module_level_comments: None,
        }
    );

    assert_eq!(
        trait_documentation,
        Documentation {
            prefix_comments: Some(String::from("Trait containing abc function.\n")),
            inner_comments: None,
            module_level_comments: None
        }
    );

    assert_eq!(
        trait_function_documentation,
        Documentation {
            prefix_comments: Some(String::from(
                "abc function returning u32.\nDefault impl of abc TraitTest function.\n"
            )),
            inner_comments: Some(String::from(
                "Default impl of abc TraitTest function inner comment.\n"
            )),
            module_level_comments: None
        }
    );

    assert_eq!(impl_documentation, Documentation {
      prefix_comments: Some(String::from("Implementation of TraitTest's abc function.\nAdditional comment for TraitTestImpl.\n")),
      inner_comments: None,
      module_level_comments: None
    });

    assert_eq!(
        impl_function_documentation,
        Documentation {
            prefix_comments: Some(String::from("Default impl of abc TraitTest function.\n")),
            inner_comments: Some(String::from(
                "Default impl of abc TraitTest function inner comment.\n"
            )),
            module_level_comments: None,
        },
    );

    assert_eq!(submodule_documentation, Documentation {
      prefix_comments: Some(String::from("Test module used to check if the documentation is being attachted to the nodes correctly.\nAdditional comment for test_module.\n")),
      inner_comments: Some(String::from("Test module used to check if the documentation is being attachted to the nodes correctly.\n")),
      module_level_comments: None
    });

    assert_eq!(
        submodule_function_documentation,
        Documentation {
            prefix_comments: Some(String::from("Just a function outside the test_module.\n")),
            inner_comments: Some(String::from("Just a function inside the test_module.\n")),
            module_level_comments: None
        }
    );

    assert_eq!(
        struct_documentation,
        Documentation {
            prefix_comments: Some(String::from(
                "Point struct representing a point in a 2d space.\n"
            )),
            inner_comments: None,
            module_level_comments: None
        }
    );

    assert_eq!(
        enum_documentation,
        Documentation {
            prefix_comments: Some(String::from(
                "Answer Enum representing an answer to a yes/no question.\n"
            )),
            inner_comments: None,
            module_level_comments: None
        }
    );

    assert_eq!(
        member_1_documentation,
        Documentation {
            prefix_comments: Some(String::from("X coordinate.\n")),
            inner_comments: None,
            module_level_comments: None
        }
    );

    assert_eq!(
        member_2_documentation,
        Documentation {
            prefix_comments: Some(String::from("Y coordinate.\n")),
            inner_comments: None,
            module_level_comments: None
        }
    );
}
