use crate::db::DocGroup;

use super::test_utils::{set_file_content, setup_test_module, TestDatabase};
use cairo_lang_defs::{
    db::DefsGroup,
    ids::{FreeFunctionId, ModuleId, ModuleItemId},
};
use indoc::indoc;

/// some content [label1](path1)[path2] some content after first links [path3][label3](path4) and later content
#[test]
fn test_documentation_to_tokens() {
    let mut db_val = TestDatabase::new().unwrap();
    let crate_id = setup_test_module(
        &mut db_val,
        indoc!(
            r#"

            struct Abc {}

            /// This refers to a struct above [crate::Abc]
            fn main () {
              println!("main");
            }
          "#
        ),
    );
    let db = &db_val;

    let main_module_id = ModuleId::CrateRoot(crate_id);
    let free_functions = db.module_free_functions(main_module_id).unwrap();
    let main_function_id: &FreeFunctionId =
        free_functions.keys().collect::<Vec<&FreeFunctionId>>().first().unwrap();

    db.get_item_documentation_as_tokens(crate::documentable_item::DocumentableItemId::LookupItem(
        cairo_lang_defs::ids::LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
            *main_function_id,
        )),
    ));

    // let tokens = doc_parser.from_documentation_comment(
    //     crate::documentable_item::DocumentableItemId::LookupItem(
    //         cairo_lang_defs::ids::LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
    //             *main_function,
    //         )),
    //     ),

    // );
    // assert_eq!(
    //     tokens,
    //     vec![
    //         DocumentationCommentToken::CommentContent("some content ".to_string()),
    //         DocumentationCommentToken::CommentLink(CommentLinkToken {
    //             label: "label1".to_string(),
    //             path: Some("path1".to_string())
    //             resolved_item: None
    //         }),
    //         DocumentationCommentToken::CommentLink(CommentLinkToken {
    //             label: "path2".to_string(),
    //             path: None,
    //             resolved_item: None
    //         }),
    //         DocumentationCommentToken::CommentContent(
    //             " some content after first links ".to_string()
    //         ),
    //         DocumentationCommentToken::CommentLink(CommentLinkToken {
    //             label: "path3".to_string(),
    //             path: None,
    //             resolved_item: None
    //         }),
    //         DocumentationCommentToken::CommentLink(CommentLinkToken {
    //             label: "label3".to_string(),
    //             path: Some("path4".to_string()),
    //             resolved_item: None
    //         }),
    //         DocumentationCommentToken::CommentContent(" and later content".to_string())
    //     ]
    // );
}
