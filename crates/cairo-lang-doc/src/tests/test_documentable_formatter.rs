use cairo_lang_defs::ids::{
    EnumId, ImplDefId, ImplItemId, LookupItemId, ModuleId, ModuleItemId, StructId, TraitId,
    TraitItemId,
};
use cairo_lang_semantic::items::enm::EnumSemantic;
use cairo_lang_semantic::items::imp::ImplSemantic;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::items::trt::TraitSemantic;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use super::test_utils::{
    TestDatabase, set_file_content, setup_test_module_without_syntax_diagnostics,
};
use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::location_links::LocationLink;
use crate::tests::test_utils::test_crate_id;

cairo_lang_test_utils::test_file_test!(
item_documentation,
  "src/tests/test-data",
  {
    signature_links: "signature_links.txt",
  },
  documentation_test_runner
);

fn documentation_test_runner(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut db_val = TestDatabase::new().unwrap();
    setup_test_module_without_syntax_diagnostics(&mut db_val, inputs["cairo_code"].as_str());
    let submodule_code = inputs.get("cairo_submodule_code");

    if let Some(submodule_code) = submodule_code {
        set_file_content(&mut db_val, "src/cairo_submodule_code.cairo", submodule_code);
    }

    let db = &db_val;

    let mut result_doc_builder = ResultSignatureBuilder::new(db);
    let crate_id = test_crate_id(db);
    result_doc_builder.document_module(ModuleId::CrateRoot(crate_id));

    TestRunnerResult::success(result_doc_builder.get_output())
}

struct ResultSignatureBuilder<'a> {
    db: &'a TestDatabase,
    item_counter: u32,
    output: OrderedHashMap<String, String>,
}

impl<'a> ResultSignatureBuilder<'a> {
    fn new(db: &'a TestDatabase) -> Self {
        Self { db, item_counter: 1, output: OrderedHashMap::default() }
    }

    fn get_output(self) -> OrderedHashMap<String, String> {
        self.output
    }

    fn document_module(&mut self, module_id: ModuleId<'_>) {
        let submodule_items = module_id.module_data(self.db).unwrap().items(self.db);

        for submodule_item_id in submodule_items.iter() {
            match submodule_item_id {
                ModuleItemId::Struct(struct_id) => {
                    self.document_struct_with_members(struct_id);
                }
                ModuleItemId::Enum(enum_id) => {
                    self.document_enum_with_variants(enum_id);
                }
                ModuleItemId::Trait(trait_id) => {
                    self.document_trait_with_items(trait_id);
                }
                ModuleItemId::Impl(impl_id) => {
                    self.document_impl_with_items(impl_id);
                }
                ModuleItemId::Submodule(module_id) => {
                    self.document_module(ModuleId::Submodule(*module_id))
                }
                _ => {
                    let id = DocumentableItemId::from(LookupItemId::ModuleItem(*submodule_item_id));
                    let (item_signature, links) = self.db.get_item_signature_with_links(id);
                    self.insert_signature_links_to_test_output(item_signature, links);
                }
            }
        }
    }

    fn document_struct_with_members(&mut self, struct_id: &StructId<'_>) {
        let id =
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(*struct_id)));
        let (struct_signature, links) = self.db.get_item_signature_with_links(id);
        self.insert_signature_links_to_test_output(struct_signature, links);

        let members = self.db.struct_members(*struct_id).unwrap();

        members.iter().for_each(|(_, semantic_member)| {
            let id = DocumentableItemId::from(semantic_member.id);
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });
    }

    fn document_enum_with_variants(&mut self, enum_id: &EnumId<'_>) {
        let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(*enum_id)));
        let (signature, links) = self.db.get_item_signature_with_links(id);
        self.insert_signature_links_to_test_output(signature, links);
        let variants = self.db.enum_variants(*enum_id).unwrap();

        variants.iter().for_each(|(_, variant_id)| {
            let id = DocumentableItemId::Variant(*variant_id);
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        })
    }

    fn document_trait_with_items(&mut self, trait_id: &TraitId<'_>) {
        let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(*trait_id)));
        let (signature, links) = self.db.get_item_signature_with_links(id);
        self.insert_signature_links_to_test_output(signature, links);
        let trait_constants = self.db.trait_constants(*trait_id).unwrap();
        let trait_types = self.db.trait_types(*trait_id).unwrap();
        let trait_functions = self.db.trait_functions(*trait_id).unwrap();

        trait_constants.iter().for_each(|(_, trait_constant_id)| {
            let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Constant(
                *trait_constant_id,
            )));
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });

        trait_types.iter().for_each(|(_, trait_type_id)| {
            let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Type(
                *trait_type_id,
            )));
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });

        trait_functions.iter().for_each(|(_, trait_function_id)| {
            let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Function(
                *trait_function_id,
            )));
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });
    }

    fn document_impl_with_items(&mut self, impl_id: &ImplDefId<'_>) {
        let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(*impl_id)));
        let (impl_signature, links) = self.db.get_item_signature_with_links(id);
        self.insert_signature_links_to_test_output(impl_signature, links);
        let impl_types = self.db.impl_types(*impl_id).unwrap();
        let impl_constants = self.db.impl_constants(*impl_id).unwrap();
        let impl_functions = self.db.impl_functions(*impl_id).unwrap();

        impl_types.iter().for_each(|(impl_type_id, _)| {
            let id =
                DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Type(*impl_type_id)));
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });

        impl_constants.iter().for_each(|(impl_constant_id, _)| {
            let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Constant(
                *impl_constant_id,
            )));
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });

        impl_functions.iter().for_each(|(_, impl_function_id)| {
            let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Function(
                *impl_function_id,
            )));
            let (signature, links) = self.db.get_item_signature_with_links(id);
            self.insert_signature_links_to_test_output(signature, links);
        });
    }

    fn insert_signature_links_to_test_output(
        &mut self,
        signature: Option<String>,
        linked_items: Vec<LocationLink<'_>>,
    ) {
        if let Some(signature) = signature {
            let formatted_links = linked_items
                .iter()
                .map(|link| self.cut_link_slice(signature.clone(), link.start, link.end))
                .join(", ");

            self.output
                .insert("Item signature #".to_string() + &self.item_counter.to_string(), signature);
            self.output.insert(
                "Item linked items #".to_string() + &self.item_counter.to_string(),
                formatted_links,
            );
            self.item_counter += 1;
        }
    }

    fn cut_link_slice(&mut self, signature: String, start: usize, end: usize) -> String {
        signature.chars().skip(start).take(end - start).collect()
    }
}
