use std::collections::HashMap;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{EnumId, ModuleId, StructId, TopLevelLanguageElementId};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::items::enm::EnumSemantic;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::{GenericArg, Program};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;
use serde::{Deserialize, Serialize};

use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId};

/// Serializable type names debug info for all user-defined struct and enum types in a Sierra
/// program.
pub struct SerializableTypeNamesDebugInfo {
    structs: HashMap<SierraTypeId, StructInfo>,
    enums: HashMap<SierraTypeId, EnumInfo>,
}

#[derive(Serialize, Deserialize)]
struct StructInfo {
    name: String,
    members: Vec<String>,
}

#[derive(Serialize, Deserialize)]
struct EnumInfo {
    name: String,
    variants: Vec<String>,
}

/// An id of a Sierra type - equivalent of the `id` field of
/// [`cairo_lang_sierra::ids::ConcreteTypeId`].
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
struct SierraTypeId(u64);

/// Extracts the type names debug info from a Sierra program.
pub fn extract_type_names(db: &dyn Database, program: &Program) -> SerializableTypeNamesDebugInfo {
    let mut structs = HashMap::new();
    let mut enums = HashMap::new();

    let struct_map = build_struct_map(db);
    let enum_map = build_enum_map(db);

    for type_decl in &program.type_declarations {
        match db.lookup_concrete_type(&type_decl.id) {
            SierraGeneratorTypeLongId::Regular(long_id) => {
                let Some(GenericArg::UserType(ut)) = long_id.generic_args.first() else {
                    continue;
                };
                let Some(debug_name) = ut.debug_name.as_deref() else { continue };
                let base_name = strip_generic_args(debug_name);

                if long_id.generic_id.0 == "Struct" {
                    let Some(&struct_id) = struct_map.get(base_name) else { continue };
                    let Ok(members) = db.struct_members(struct_id) else { continue };
                    structs.insert(
                        SierraTypeId(type_decl.id.id),
                        StructInfo {
                            name: debug_name.to_string(),
                            members: members.keys().map(|n| n.long(db).to_string()).collect(),
                        },
                    );
                } else if long_id.generic_id.0 == "Enum" {
                    let Some(&enum_id) = enum_map.get(base_name) else { continue };
                    let Ok(variants) = db.enum_variants(enum_id) else { continue };
                    enums.insert(
                        SierraTypeId(type_decl.id.id),
                        EnumInfo {
                            name: debug_name.to_string(),
                            variants: variants.keys().map(|v| v.long(db).to_string()).collect(),
                        },
                    );
                }
            }
            SierraGeneratorTypeLongId::CycleBreaker(sem_ty)
            | SierraGeneratorTypeLongId::Phantom(sem_ty) => {
                if let cairo_lang_semantic::TypeLongId::Concrete(ty) = sem_ty.long(db) {
                    match ty {
                        cairo_lang_semantic::ConcreteTypeId::Struct(concrete_struct_id) => {
                            let Ok(members) = db.struct_members(concrete_struct_id.struct_id(db))
                            else {
                                continue;
                            };
                            structs.insert(
                                SierraTypeId(type_decl.id.id),
                                StructInfo {
                                    name: ty.format(db),
                                    members: members
                                        .keys()
                                        .map(|name| name.long(db).to_string())
                                        .collect(),
                                },
                            );
                        }
                        cairo_lang_semantic::ConcreteTypeId::Enum(concrete_enum_id) => {
                            let Ok(variants) = db.enum_variants(concrete_enum_id.enum_id(db))
                            else {
                                continue;
                            };
                            enums.insert(
                                SierraTypeId(type_decl.id.id),
                                EnumInfo {
                                    name: ty.format(db),
                                    variants: variants
                                        .keys()
                                        .map(|v| v.long(db).to_string())
                                        .collect(),
                                },
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    SerializableTypeNamesDebugInfo { structs, enums }
}

impl From<SerializableTypeNamesDebugInfo> for Annotations {
    fn from(value: SerializableTypeNamesDebugInfo) -> Self {
        OrderedHashMap::from([(
            "github.com/software-mansion-labs/cairo-debugger/user-types".to_string(),
            serde_json::json!({
                "structs": value.structs,
                "enums": value.enums,
            }),
        )])
    }
}

/// Returns the base type name, stripping any generic arguments.
/// E.g. `"foo::Bar::<u32>"` -> `"foo::Bar"`.
fn strip_generic_args(name: &str) -> &str {
    match name.find("::<") {
        Some(idx) => &name[..idx],
        None => name,
    }
}

/// Builds a map from fully-qualified struct path to [`StructId`] by scanning all crates.
fn build_struct_map<'db>(db: &'db dyn Database) -> HashMap<String, StructId<'db>> {
    let mut map = HashMap::new();
    for crate_id in db.crates() {
        collect_structs_from_module(db, ModuleId::CrateRoot(*crate_id), &mut map);
    }
    map
}

/// Builds a map from fully-qualified enum path to [`EnumId`] by scanning all crates.
fn build_enum_map<'db>(db: &'db dyn Database) -> HashMap<String, EnumId<'db>> {
    let mut map = HashMap::new();
    for crate_id in db.crates() {
        collect_enums_from_module(db, ModuleId::CrateRoot(*crate_id), &mut map);
    }
    map
}

fn collect_structs_from_module<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    map: &mut HashMap<String, StructId<'db>>,
) {
    if let Ok(struct_ids) = db.module_structs_ids(module_id) {
        for struct_id in struct_ids.iter() {
            map.insert(struct_id.full_path(db), *struct_id);
        }
    }
    if let Ok(submodule_ids) = db.module_submodules_ids(module_id) {
        for submod_id in submodule_ids.iter() {
            collect_structs_from_module(db, ModuleId::Submodule(*submod_id), map);
        }
    }
}

fn collect_enums_from_module<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    map: &mut HashMap<String, EnumId<'db>>,
) {
    if let Ok(enum_ids) = db.module_enums_ids(module_id) {
        for enum_id in enum_ids.iter() {
            map.insert(enum_id.full_path(db), *enum_id);
        }
    }
    if let Ok(submodule_ids) = db.module_submodules_ids(module_id) {
        for submod_id in submodule_ids.iter() {
            collect_enums_from_module(db, ModuleId::Submodule(*submod_id), map);
        }
    }
}
