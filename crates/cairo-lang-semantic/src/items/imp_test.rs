use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{ModuleId, ModuleItemId, TopLevelLanguageElementId};
use cairo_lang_filesystem::db::{CrateConfiguration, FilesGroup};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, Directory, FileLongId, SmolStrId};
use cairo_lang_filesystem::{override_file_content, set_crate_config};
use cairo_lang_test_utils::test;
use cairo_lang_utils::{Intern, extract_matches};
use pretty_assertions::{assert_eq, assert_ne};
use salsa::Database;

use super::{ConcreteImplLongId, ImplLongId, UninferredImpl, uninferred_impl_name};
use crate::items::imp::ImplSemantic;
use crate::items::module::ModuleSemantic;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};
use crate::types::{ConcreteStructLongId, ConcreteTypeId, TypeLongId};

#[test]
fn test_impl() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let (test_module, diagnostics) = setup_test_module(
        db,
        indoc::indoc! {"
            trait IContract {
                fn foo(a: felt252);
            }

            impl Contract of IContract {
                fn foo(a: felt252) {
                }
            }
        "},
    )
    .split();

    assert!(diagnostics.is_empty());

    let impl_def_id = extract_matches!(
        db.module_item_by_name(test_module.module_id, SmolStrId::from(db, "Contract"))
            .unwrap()
            .unwrap(),
        ModuleItemId::Impl
    );

    assert_eq!(format!("{:?}", db.impl_def_generic_params(impl_def_id).unwrap()), "[]");

    let impl_functions = db.impl_functions(impl_def_id).unwrap();
    let impl_function_id = impl_functions.get(&SmolStrId::from(db, "foo")).unwrap();
    let signature = db.impl_function_signature(*impl_function_id).unwrap();
    assert_eq!(
        format!("{:?}", signature.debug(db)),
        "Signature { params: [Parameter { id: ParamId(test::Contract::foo::a), name: \"a\", ty: \
         core::felt252, mutability: Immutable }], return_type: (), implicits: [], panicable: \
         true, is_const: false }"
    );

    db.impl_def_concrete_trait(impl_def_id).unwrap();
}

/// The id of the crate named `dep` with the given discriminator.
fn crate_copy<'db>(db: &'db dyn Database, discriminator: &str) -> CrateId<'db> {
    CrateLongId::Real {
        name: SmolStrId::from(db, "dep"),
        discriminator: Some(discriminator.into()),
    }
    .intern(db)
}

/// Sets up the crate named `dep` with the given discriminator, with `content` as its root module.
fn setup_crate_copy(db: &mut dyn Database, discriminator: &str, content: &str) {
    let crate_id = crate_copy(db, discriminator);
    let root = Directory::Real(format!("dep{discriminator}").into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));
    let file_id = FileLongId::OnDisk(format!("dep{discriminator}/lib.cairo").into()).intern(db);
    override_file_content!(db, file_id, Some(content.into()));
}

#[test]
fn test_names_distinguish_crate_copies() {
    let mut db_val = SemanticDatabaseForTesting::new_empty();
    let db: &mut dyn Database = &mut db_val;
    let content = "struct S {}\ntrait Tr {}\nimpl Imp of Tr {}\n";
    for discriminator in ["1", "2"] {
        setup_crate_copy(db, discriminator, content);
    }
    let db: &dyn Database = db;
    let [(struct1, impl1), (struct2, impl2)] = ["1", "2"].map(|discriminator| {
        let module_id = ModuleId::CrateRoot(crate_copy(db, discriminator));
        let [struct_id] = db.module_structs_ids(module_id).unwrap() else { panic!() };
        let [impl_def_id] = db.module_impls_ids(module_id).unwrap() else { panic!() };
        (*struct_id, *impl_def_id)
    });
    // The copies' items share their paths, so the names that keys are built from tell them apart.
    assert_eq!(struct1.full_path(db), struct2.full_path(db));
    assert_eq!(struct1.key_path(db), "[1]dep::S");
    assert_eq!(struct2.key_path(db), "[2]dep::S");

    let [ty1, ty2] = [struct1, struct2].map(|struct_id| {
        let concrete = ConcreteStructLongId { struct_id, generic_args: vec![] }.intern(db);
        TypeLongId::Concrete(ConcreteTypeId::Struct(concrete)).intern(db)
    });
    assert_eq!(ty1.short_name(db), ty2.short_name(db));
    assert_ne!(ty1.key_name(db), ty2.key_name(db));

    let [imp1, imp2] = [impl1, impl2].map(|impl_def_id| {
        let concrete = ConcreteImplLongId { impl_def_id, generic_args: vec![] }.intern(db);
        ImplLongId::Concrete(concrete).intern(db)
    });
    assert_eq!(imp1.format(db), imp2.format(db));
    assert_ne!(imp1.short_name(db), imp2.short_name(db));
    assert_ne!(
        uninferred_impl_name(db, (), UninferredImpl::Def(impl1)),
        uninferred_impl_name(db, (), UninferredImpl::Def(impl2))
    );
}
