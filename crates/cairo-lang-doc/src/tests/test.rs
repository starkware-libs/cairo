use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    EnumId, ImplDefId, ImplItemId, LookupItemId, ModuleId, ModuleItemId, StructId, TraitId,
    TraitItemId,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::test_utils::{TestDatabase, set_file_content, setup_test_module};
use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;

cairo_lang_test_utils::test_file_test!(
  item_documentation,
  "src/tests/test-data",
  {
    basic: "basic.txt",
    submodule: "submodule.txt",
    trivia: "trivia.txt"
  },
  documentation_test_runner
);

fn documentation_test_runner(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut output: OrderedHashMap<String, String> = OrderedHashMap::default();
    let mut db_val = TestDatabase::new().unwrap();
    let crate_id = setup_test_module(&mut db_val, inputs["cairo_code"].as_str());
    let submodule_code = inputs.get("cairo_submodule_code");

    if let Some(submodule_code) = submodule_code {
        set_file_content(&mut db_val, "src/cairo_submodule_code.cairo", submodule_code);
    }

    let db = &db_val;
    let mut item_counter: u32 = 1;

    document_module(db, &mut output, ModuleId::CrateRoot(crate_id), &mut item_counter);

    TestRunnerResult::success(output)
}

fn document_module(
    db: &TestDatabase,
    output: &mut OrderedHashMap<String, String>,
    module_id: ModuleId,
    item_number: &mut u32,
) {
    let module_doc = match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.get_item_documentation(DocumentableItemId::Crate(crate_id))
        }
        ModuleId::Submodule(submodule_id) => db.get_item_documentation(DocumentableItemId::from(
            LookupItemId::ModuleItem(ModuleItemId::Submodule(submodule_id)),
        )),
    };

    insert_doc_to_test_output(output, item_number, module_doc, "".to_owned());

    let submodule_items = db.module_items(module_id).unwrap();

    for submodule_item_id in submodule_items.iter() {
        match submodule_item_id {
            ModuleItemId::Struct(struct_id) => {
                document_struct_with_members(db, output, struct_id, item_number);
            }
            ModuleItemId::Enum(enum_id) => {
                document_enum_with_variants(db, output, enum_id, item_number);
            }
            ModuleItemId::Trait(trait_id) => {
                document_trait_with_items(db, output, trait_id, item_number);
            }
            ModuleItemId::Impl(impl_id) => {
                document_impl_with_items(db, output, impl_id, item_number);
            }
            ModuleItemId::Submodule(module_id) => {
                document_module(db, output, ModuleId::Submodule(*module_id), item_number)
            }
            _ => {
                let id = DocumentableItemId::from(LookupItemId::ModuleItem(*submodule_item_id));
                let item_doc = db.get_item_documentation(id);
                let item_signature = db.get_item_signature(id);
                insert_doc_to_test_output(output, item_number, item_doc, item_signature);
            }
        }
    }
}

fn document_struct_with_members(
    db: &TestDatabase,
    output: &mut OrderedHashMap<String, String>,
    struct_id: &StructId,
    item_number: &mut u32,
) {
    let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(*struct_id)));
    let struct_doc = db.get_item_documentation(id);
    let struct_signature = db.get_item_signature(id);
    insert_doc_to_test_output(output, item_number, struct_doc, struct_signature);
    let members = db.struct_members(*struct_id).unwrap();

    members.iter().for_each(|(_, semantic_member)| {
        let id = DocumentableItemId::from(semantic_member.id);
        let member_doc = db.get_item_documentation(id);
        let member_signature = db.get_item_signature(id);
        insert_doc_to_test_output(output, item_number, member_doc, member_signature);
    });
}

fn document_enum_with_variants(
    db: &TestDatabase,
    output: &mut OrderedHashMap<String, String>,
    enum_id: &EnumId,
    item_number: &mut u32,
) {
    let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(*enum_id)));
    let enum_doc = db.get_item_documentation(id);
    let enum_signature = db.get_item_signature(id);
    insert_doc_to_test_output(output, item_number, enum_doc, enum_signature);
    let variants = db.enum_variants(*enum_id).unwrap();

    variants.iter().for_each(|(_, variant_id)| {
        let id = DocumentableItemId::Variant(*variant_id);
        let variant_doc = db.get_item_documentation(id);
        let variant_signature = db.get_item_signature(id);
        insert_doc_to_test_output(output, item_number, variant_doc, variant_signature);
    })
}

fn document_trait_with_items(
    db: &TestDatabase,
    output: &mut OrderedHashMap<String, String>,
    trait_id: &TraitId,
    item_number: &mut u32,
) {
    let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(*trait_id)));
    let trait_doc = db.get_item_documentation(id);
    let trait_signature = db.get_item_signature(id);
    insert_doc_to_test_output(output, item_number, trait_doc, trait_signature);
    let trait_constants = db.trait_constants(*trait_id).unwrap();
    let trait_types = db.trait_types(*trait_id).unwrap();
    let trait_functions = db.trait_functions(*trait_id).unwrap();

    trait_constants.iter().for_each(|(_, trait_constant_id)| {
        let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Constant(
            *trait_constant_id,
        )));
        let trait_constant_doc = db.get_item_documentation(id);
        let trait_constant_signature = db.get_item_signature(id);
        insert_doc_to_test_output(
            output,
            item_number,
            trait_constant_doc,
            trait_constant_signature,
        );
    });

    trait_types.iter().for_each(|(_, trait_type_id)| {
        let id =
            DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Type(*trait_type_id)));
        let trait_type_doc = db.get_item_documentation(id);
        let trait_type_signature = db.get_item_signature(id);
        insert_doc_to_test_output(output, item_number, trait_type_doc, trait_type_signature);
    });

    trait_functions.iter().for_each(|(_, trait_function_id)| {
        let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Function(
            *trait_function_id,
        )));
        let trait_function_doc = db.get_item_documentation(id);
        let trait_function_signature = db.get_item_signature(id);
        insert_doc_to_test_output(
            output,
            item_number,
            trait_function_doc,
            trait_function_signature,
        );
    });
}

fn document_impl_with_items(
    db: &TestDatabase,
    output: &mut OrderedHashMap<String, String>,
    impl_id: &ImplDefId,
    item_number: &mut u32,
) {
    let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(*impl_id)));
    let impl_doc = db.get_item_documentation(id);
    let impl_signature = db.get_item_signature(id);
    insert_doc_to_test_output(output, item_number, impl_doc, impl_signature);
    let impl_types = db.impl_types(*impl_id).unwrap();
    let impl_constants = db.impl_constants(*impl_id).unwrap();
    let impl_functions = db.impl_functions(*impl_id).unwrap();

    impl_types.iter().for_each(|(impl_type_id, _)| {
        let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Type(*impl_type_id)));
        let impl_type_doc = db.get_item_documentation(id);
        let impl_type_signature = db.get_item_signature(id);
        insert_doc_to_test_output(output, item_number, impl_type_doc, impl_type_signature);
    });

    impl_constants.iter().for_each(|(impl_constant_id, _)| {
        let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Constant(
            *impl_constant_id,
        )));
        let impl_constant_doc = db.get_item_documentation(id);
        let impl_constant_signature = db.get_item_signature(id);
        insert_doc_to_test_output(output, item_number, impl_constant_doc, impl_constant_signature);
    });

    impl_functions.iter().for_each(|(_, impl_function_id)| {
        let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Function(
            *impl_function_id,
        )));
        let impl_function_doc = db.get_item_documentation(id);
        let impl_function_signature = db.get_item_signature(id);
        insert_doc_to_test_output(output, item_number, impl_function_doc, impl_function_signature);
    });
}

fn insert_doc_to_test_output(
    output: &mut OrderedHashMap<String, String>,
    item_number: &mut u32,
    documentation: Option<String>,
    signature: String,
) {
    output.insert("Item signature #".to_string() + &item_number.to_string(), signature);
    output.insert(
        "Item documentation #".to_string() + &item_number.to_string(),
        documentation.unwrap_or_default(),
    );
    *item_number += 1;
}
