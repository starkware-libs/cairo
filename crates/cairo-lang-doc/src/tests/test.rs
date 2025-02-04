use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    EnumId, ImplDefId, ImplItemId, LookupItemId, ModuleId, ModuleItemId, StructId, TraitId,
    TraitItemId,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use super::test_utils::{TestDatabase, set_file_content, setup_test_module};
use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::parser::DocumentationCommentToken;

cairo_lang_test_utils::test_file_test!(
  item_documentation,
  "src/tests/test-data",
  {
    basic: "basic.txt",
    submodule: "submodule.txt",
    trivia: "trivia.txt",
    comment_markers: "comment_markers.txt"
  },
  documentation_test_runner
);

fn documentation_test_runner(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut db_val = TestDatabase::new().unwrap();
    let crate_id = setup_test_module(&mut db_val, inputs["cairo_code"].as_str());
    let submodule_code = inputs.get("cairo_submodule_code");

    if let Some(submodule_code) = submodule_code {
        set_file_content(&mut db_val, "src/cairo_submodule_code.cairo", submodule_code);
    }

    let db = &db_val;

    let mut result_doc_builder = ResultDocBuilder::new(db);

    result_doc_builder.document_module(ModuleId::CrateRoot(crate_id));

    TestRunnerResult::success(result_doc_builder.get_output())
}

struct ResultDocBuilder<'a> {
    db: &'a TestDatabase,
    item_counter: u32,
    output: OrderedHashMap<String, String>,
}

impl<'a> ResultDocBuilder<'a> {
    fn new(db: &'a TestDatabase) -> Self {
        Self { db, item_counter: 1, output: OrderedHashMap::default() }
    }

    fn get_output(self) -> OrderedHashMap<String, String> {
        self.output
    }

    fn document_module(&mut self, module_id: ModuleId) {
        let module_doc = match module_id {
            ModuleId::CrateRoot(crate_id) => {
                self.db.get_item_documentation(DocumentableItemId::Crate(crate_id))
            }
            ModuleId::Submodule(submodule_id) => {
                self.db.get_item_documentation(DocumentableItemId::from(LookupItemId::ModuleItem(
                    ModuleItemId::Submodule(submodule_id),
                )))
            }
        };

        let doc_tokens = match module_id {
            ModuleId::CrateRoot(crate_id) => {
                self.db.get_item_documentation_as_tokens(DocumentableItemId::Crate(crate_id))
            }
            ModuleId::Submodule(submodule_id) => {
                self.db.get_item_documentation_as_tokens(DocumentableItemId::from(
                    LookupItemId::ModuleItem(ModuleItemId::Submodule(submodule_id)),
                ))
            }
        };

        self.insert_doc_to_test_output(module_doc, "".to_owned(), doc_tokens);

        let submodule_items = self.db.module_items(module_id).unwrap();

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
                    let item_doc = self.db.get_item_documentation(id);
                    let item_signature = self.db.get_item_signature(id);
                    let item_doc_tokens = self.db.get_item_documentation_as_tokens(id);
                    self.insert_doc_to_test_output(item_doc, item_signature, item_doc_tokens);
                }
            }
        }
    }

    fn document_struct_with_members(&mut self, struct_id: &StructId) {
        let id =
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(*struct_id)));
        let struct_doc = self.db.get_item_documentation(id);
        let struct_signature = self.db.get_item_signature(id);
        let struct_doc_tokens = self.db.get_item_documentation_as_tokens(id);
        self.insert_doc_to_test_output(struct_doc, struct_signature, struct_doc_tokens);
        let members = self.db.struct_members(*struct_id).unwrap();

        members.iter().for_each(|(_, semantic_member)| {
            let id = DocumentableItemId::from(semantic_member.id);
            let member_doc = self.db.get_item_documentation(id);
            let member_signature = self.db.get_item_signature(id);
            let member_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(member_doc, member_signature, member_doc_tokens);
        });
    }

    fn document_enum_with_variants(&mut self, enum_id: &EnumId) {
        let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(*enum_id)));
        let enum_doc = self.db.get_item_documentation(id);
        let enum_signature = self.db.get_item_signature(id);
        let enum_doc_tokens = self.db.get_item_documentation_as_tokens(id);
        self.insert_doc_to_test_output(enum_doc, enum_signature, enum_doc_tokens);
        let variants = self.db.enum_variants(*enum_id).unwrap();

        variants.iter().for_each(|(_, variant_id)| {
            let id = DocumentableItemId::Variant(*variant_id);
            let variant_doc = self.db.get_item_documentation(id);
            let variant_signature = self.db.get_item_signature(id);
            let variant_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(variant_doc, variant_signature, variant_doc_tokens);
        })
    }

    fn document_trait_with_items(&mut self, trait_id: &TraitId) {
        let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(*trait_id)));
        let trait_doc = self.db.get_item_documentation(id);
        let trait_signature = self.db.get_item_signature(id);
        let trait_doc_tokens = self.db.get_item_documentation_as_tokens(id);
        self.insert_doc_to_test_output(trait_doc, trait_signature, trait_doc_tokens);
        let trait_constants = self.db.trait_constants(*trait_id).unwrap();
        let trait_types = self.db.trait_types(*trait_id).unwrap();
        let trait_functions = self.db.trait_functions(*trait_id).unwrap();

        trait_constants.iter().for_each(|(_, trait_constant_id)| {
            let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Constant(
                *trait_constant_id,
            )));
            let trait_constant_doc = self.db.get_item_documentation(id);
            let trait_constant_signature = self.db.get_item_signature(id);
            let trait_constant_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(
                trait_constant_doc,
                trait_constant_signature,
                trait_constant_doc_tokens,
            );
        });

        trait_types.iter().for_each(|(_, trait_type_id)| {
            let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Type(
                *trait_type_id,
            )));
            let trait_type_doc = self.db.get_item_documentation(id);
            let trait_type_signature = self.db.get_item_signature(id);
            let trait_type_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(
                trait_type_doc,
                trait_type_signature,
                trait_type_doc_tokens,
            );
        });

        trait_functions.iter().for_each(|(_, trait_function_id)| {
            let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Function(
                *trait_function_id,
            )));
            let trait_function_doc = self.db.get_item_documentation(id);
            let trait_function_signature = self.db.get_item_signature(id);
            let trait_function_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(
                trait_function_doc,
                trait_function_signature,
                trait_function_doc_tokens,
            );
        });
    }

    fn document_impl_with_items(&mut self, impl_id: &ImplDefId) {
        let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(*impl_id)));
        let impl_doc = self.db.get_item_documentation(id);
        let impl_signature = self.db.get_item_signature(id);
        let impl_doc_tokens = self.db.get_item_documentation_as_tokens(id);
        self.insert_doc_to_test_output(impl_doc, impl_signature, impl_doc_tokens);
        let impl_types = self.db.impl_types(*impl_id).unwrap();
        let impl_constants = self.db.impl_constants(*impl_id).unwrap();
        let impl_functions = self.db.impl_functions(*impl_id).unwrap();

        impl_types.iter().for_each(|(impl_type_id, _)| {
            let id =
                DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Type(*impl_type_id)));
            let impl_type_doc = self.db.get_item_documentation(id);
            let impl_type_signature = self.db.get_item_signature(id);
            let impl_type_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(
                impl_type_doc,
                impl_type_signature,
                impl_type_doc_tokens,
            );
        });

        impl_constants.iter().for_each(|(impl_constant_id, _)| {
            let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Constant(
                *impl_constant_id,
            )));
            let impl_constant_doc = self.db.get_item_documentation(id);
            let impl_constant_signature = self.db.get_item_signature(id);
            let impl_constant_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(
                impl_constant_doc,
                impl_constant_signature,
                impl_constant_doc_tokens,
            );
        });

        impl_functions.iter().for_each(|(_, impl_function_id)| {
            let id = DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Function(
                *impl_function_id,
            )));
            let impl_function_doc = self.db.get_item_documentation(id);
            let impl_function_signature = self.db.get_item_signature(id);
            let impl_function_doc_tokens = self.db.get_item_documentation_as_tokens(id);
            self.insert_doc_to_test_output(
                impl_function_doc,
                impl_function_signature,
                impl_function_doc_tokens,
            );
        });
    }

    fn insert_doc_to_test_output(
        &mut self,
        documentation: Option<String>,
        signature: String,
        documentation_as_tokens: Option<Vec<DocumentationCommentToken>>,
    ) {
        self.output
            .insert("Item signature #".to_string() + &self.item_counter.to_string(), signature);
        self.output.insert(
            "Item documentation #".to_string() + &self.item_counter.to_string(),
            documentation.unwrap_or_default(),
        );
        self.output.insert(
            "Item documentation tokens #".to_string() + &self.item_counter.to_string(),
            documentation_as_tokens
                .unwrap_or_default()
                .iter()
                .map(|token| match token {
                    DocumentationCommentToken::Content(_) => format!("{:?}", token),
                    DocumentationCommentToken::Link(link_token) => {
                        format!("{:?}", link_token.debug(self.db))
                    }
                })
                .join("\n"),
        );
        self.item_counter += 1;
    }
}
