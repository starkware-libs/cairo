use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{ModuleId, TopLevelLanguageElementId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::items::enm::EnumSemantic;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::{GenericArg, Program};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use salsa::Database;
use serde::{Deserialize, Serialize};

use crate::canonical_id_replacer::CanonicalReplacer;
use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId};
use crate::replace_ids::SierraIdReplacer;

/// Serializable type names debug info for all user-defined struct and enum types in a Sierra
/// program.
pub struct SerializableTypeNamesDebugInfo {
    /// Map between sierra type ids of concretized struct types and information about the structs.
    pub structs: OrderedHashMap<SierraTypeId, StructInfo>,
    /// Map between sierra type ids of concretized enum types and information about the enums.
    pub enums: OrderedHashMap<SierraTypeId, EnumInfo>,
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

impl SerializableTypeNamesDebugInfo {
    /// Extracts the type names debug info from a Sierra program.
    pub fn extract_type_names(db: &dyn Database, program: &Program) -> Self {
        let mut structs = OrderedHashMap::default();
        let mut enums = OrderedHashMap::default();

        let struct_map = build_items_map(db, |db, module_id| db.module_structs_ids(module_id));
        let enum_map = build_items_map(db, |db, module_id| db.module_enums_ids(module_id));

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
                                let Ok(members) =
                                    db.struct_members(concrete_struct_id.struct_id(db))
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

        Self { structs, enums }
    }

    pub fn replace_type_ids(mut self, replacer: &CanonicalReplacer) -> Self {
        fn replace_ids_in_map<T>(
            replacer: &CanonicalReplacer,
            id_map: OrderedHashMap<SierraTypeId, T>,
        ) -> OrderedHashMap<SierraTypeId, T> {
            id_map
                .into_iter()
                .map(|(id, val)| {
                    let id = replacer.replace_type_id(&ConcreteTypeId::new(id.0));
                    (SierraTypeId(id.id), val)
                })
                .collect()
        }

        self.structs = replace_ids_in_map(replacer, self.structs);
        self.enums = replace_ids_in_map(replacer, self.enums);

        self
    }
}

/// Debug information about a concretized struct type.
#[derive(Serialize, Deserialize)]
pub struct StructInfo {
    /// Concretized name of the struct type.
    pub name: String,
    /// Names of members of the struct type.
    pub members: Vec<String>,
}

/// Debug information about a concretized enum type.
#[derive(Serialize, Deserialize)]
pub struct EnumInfo {
    /// Concretized name of the enum type.
    pub name: String,
    /// Names of variants of the enum type.
    pub variants: Vec<String>,
}

/// An id of a Sierra type - equivalent of the `id` field of
/// [`cairo_lang_sierra::ids::ConcreteTypeId`].
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SierraTypeId(pub u64);

/// Builds a map from fully-qualified path to item id by scanning all crates.
fn build_items_map<'db, T: TopLevelLanguageElementId<'db> + Copy>(
    db: &'db dyn Database,
    f: fn(&'db dyn Database, ModuleId<'db>) -> Maybe<&'db [T]>,
) -> UnorderedHashMap<String, T> {
    let mut result = UnorderedHashMap::default();

    for crate_id in db.crates() {
        collect_items_from_module(db, ModuleId::CrateRoot(*crate_id), f, &mut result);
    }

    result
}

/// Collects items from the module and its submodules to a map from fully-qualified path to item id.
fn collect_items_from_module<'db, T: TopLevelLanguageElementId<'db> + Copy>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    f: fn(&'db dyn Database, ModuleId<'db>) -> Maybe<&'db [T]>,
    result: &mut UnorderedHashMap<String, T>,
) {
    if let Ok(struct_ids) = f(db, module_id) {
        for struct_id in struct_ids.iter() {
            result.insert(struct_id.full_path(db), *struct_id);
        }
    }

    if let Ok(submodule_ids) = db.module_submodules_ids(module_id) {
        for submod_id in submodule_ids.iter() {
            collect_items_from_module(db, ModuleId::Submodule(*submod_id), f, result);
        }
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
