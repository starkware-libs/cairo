use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FunctionWithBodyId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::db::{AsFilesGroupMut, CrateConfiguration, FilesGroupEx};
use cairo_lang_filesystem::ids::{CrateId, Directory, FileLongId};
use cairo_lang_utils::{Intern, extract_matches};
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_resolve_path() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = setup_test_module(
        db,
        indoc! {"
            use core::Box;
            extern type S<T>;
            extern fn bar<T>(value: S::<felt252>) -> S::<()> nopanic;

            fn foo<Q>(value: S::<felt252>, b: Q, c: Box::<Q>) {
                bar::<(felt252,Q)>(value);
                let _c = b;
            }
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let function_id = FunctionWithBodyId::Free(extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    ));
    let expr_formatter = ExprFormatter { db, function_id };
    let body = db.function_body_expr(function_id);
    assert_eq!(
        format!("{:?}", body.to_option().debug(&expr_formatter)),
        "Some(Block(ExprBlock { statements: [Expr(StatementExpr { expr: \
         FunctionCall(ExprFunctionCall { function: test::bar::<(core::felt252, Q)>, args: \
         [Value(Var(ParamId(test::value)))], coupon_arg: None, ty: test::S::<()> }) }), \
         Let(StatementLet { pattern: Variable(_c), expr: Var(ParamId(test::b)) })], tail: None, \
         ty: () }))"
    );
}

fn set_file_content(db: &mut SemanticDatabaseForTesting, path: &str, content: &str) {
    let file_id = FileLongId::OnDisk(path.into()).intern(db);
    db.as_files_group_mut().override_file_content(file_id, Some(content.into()));
}

#[test]
fn test_resolve_path_super() {
    let mut db_val = SemanticDatabaseForTesting::new_empty();
    let db = &mut db_val;

    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(
        db,
        "src/lib.cairo",
        indoc! {"
        mod inner1;
        mod inner2;
        struct OuterStruct {}
    "},
    );
    set_file_content(db, "src/inner1.cairo", "struct InnerStruct1 {}");
    set_file_content(
        db,
        "src/inner2.cairo",
        indoc! {"
            struct InnerStruct2 {
                a: super::inner1::InnerStruct1,
                b: super::OuterStruct,
            }
        "},
    );
    let test_module = ModuleId::CrateRoot(crate_id);
    let inner2_module_id = ModuleId::Submodule(extract_matches!(
        db.module_item_by_name(test_module, "inner2".into()).unwrap().unwrap(),
        ModuleItemId::Submodule
    ));
    let struct_id = extract_matches!(
        db.module_item_by_name(inner2_module_id, "InnerStruct2".into()).unwrap().unwrap(),
        ModuleItemId::Struct
    );
    let members = db.struct_members(struct_id).unwrap();
    assert_eq!(
        format!("{:?}", members["a"].debug(db)),
        "Member { id: MemberId(test::inner2::a), ty: test::inner1::InnerStruct1, visibility: \
         Private }"
    );
    assert_eq!(
        format!("{:?}", members["b"].debug(db)),
        "Member { id: MemberId(test::inner2::b), ty: test::OuterStruct, visibility: Private }"
    );
}

#[test]
fn test_resolve_path_trait_impl() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = setup_test_module(
        db,
        indoc! {"
            trait MyTrait {
                fn foo() -> felt252;
            }

            impl MyImpl of MyTrait {
                fn foo() -> felt252 {
                    7
                }
            }

            fn main() -> felt252 {
                MyTrait::foo() + 1
            }
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let function_id = FunctionWithBodyId::Free(extract_matches!(
        db.module_item_by_name(module_id, "main".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    ));
    let expr_formatter = ExprFormatter { db, function_id };
    let body = db.function_body_expr(function_id);
    assert_eq!(
        format!("{:?}", body.to_option().debug(&expr_formatter)),
        "Some(Block(ExprBlock { statements: [], tail: Some(FunctionCall(ExprFunctionCall { \
         function: core::Felt252Add::add, args: [Value(FunctionCall(ExprFunctionCall { function: \
         test::MyImpl::foo, args: [], coupon_arg: None, ty: core::felt252 })), \
         Value(Literal(ExprLiteral { value: 1, ty: core::felt252 }))], coupon_arg: None, ty: \
         core::felt252 })), ty: core::felt252 }))"
    );
}
