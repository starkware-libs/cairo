use defs::ids::ExternTypeLongId;
use filesystem::ids::{CrateLongId, ModuleId};

use crate::db::SemanticGroup;
use crate::{ConcreteType, GenericType, TypeId, TypeLongId};

pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = db.intern_crate(CrateLongId("core".into()));
    ModuleId::CrateRoot(core_crate)
}

pub fn core_felt_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    let extern_type_id =
        db.intern_extern_type(ExternTypeLongId { parent: core_module, name: "felt".into() });
    db.intern_type(TypeLongId::Concrete(ConcreteType {
        generic_type: GenericType::External(extern_type_id),
        generic_args: vec![],
    }))
}

pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    db.intern_type(TypeLongId::Tuple(vec![]))
}
