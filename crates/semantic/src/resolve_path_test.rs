use std::sync::Arc;

use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::{ModuleId, ModuleItemId};
use diagnostics::ToOption;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory, FileLongId};
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_resolve_path() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc! {"
            use core::Box;
            extern type S<T>;
            extern func bar<T>(value: S::<felt>) -> S::<()> nopanic;

            func foo<Q>(value: S::<felt>, b: Q, c: Box::<Q>) {
                bar::<(felt,Q)>(value);
                let c = b;
            }
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let free_function_id = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    );
    let expr_formatter = ExprFormatter { db, free_function_id };
    let body = db.free_function_definition_body(free_function_id);
    assert_eq!(
        format!("{:?}", body.to_option().debug(&expr_formatter)),
        "Some(Block(ExprBlock { statements: [Expr(StatementExpr { expr: \
         FunctionCall(ExprFunctionCall { function: test::bar<Type((core::felt, Q)),>, ref_args: \
         [], args: [Var(ExprVar { var: ParamId(test::value), ty: test::S::<core::felt> })], ty: \
         test::S::<()> }) }), Let(StatementLet { pattern: Variable(c), expr: Var(ExprVar { var: \
         ParamId(test::b), ty: Q }) })], tail: None, ty: () }))"
    );
}

fn set_file_content(db: &mut SemanticDatabaseForTesting, path: &str, content: &str) {
    let file_id = db.intern_file(FileLongId::OnDisk(path.into()));
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.into())));
}

#[test]
fn test_resolve_path_super() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

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
        "Member { id: MemberId(test::inner2::a), ty: test::inner1::InnerStruct1 }"
    );
    assert_eq!(
        format!("{:?}", members["b"].debug(db)),
        "Member { id: MemberId(test::inner2::b), ty: test::OuterStruct }"
    );
}
