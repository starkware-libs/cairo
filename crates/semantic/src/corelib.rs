use std::path::PathBuf;

use defs::ids::GenericTypeId;
use filesystem::db::ProjectConfig;
use filesystem::ids::{CrateLongId, FileLongId, ModuleId};

use crate::db::SemanticGroup;
use crate::{ConcreteType, TypeId, TypeLongId};

pub fn core_config(db: &dyn SemanticGroup) -> ProjectConfig {
    let core_crate = db.intern_crate(CrateLongId("core".into()));
    // TODO(spapini): find the correct path.
    // This is the directory of Cargo.toml of the syntax_codegen crate.
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/crates/semantic" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().parent().unwrap().to_owned();
    path.push("corelib/mod.cairo");
    let core_root_file = db.intern_file(FileLongId::OnDisk(path));
    ProjectConfig::default().with_crate(core_crate, core_root_file)
}

pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = db.intern_crate(CrateLongId("core".into()));
    ModuleId::CrateRoot(core_crate)
}

pub fn core_felt_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "felt".into())
        .expect("Unexpected diagnostics when looking for corelib")
        .and_then(GenericTypeId::from)
        .unwrap();
    db.intern_type(TypeLongId::Concrete(ConcreteType { generic_type, generic_args: vec![] }))
}

pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    db.intern_type(TypeLongId::Tuple(vec![]))
}
