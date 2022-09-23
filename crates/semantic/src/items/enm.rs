use defs::ids::{EnumId, LanguageElementId, VariantId, VariantLongId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use smol_str::SmolStr;
use syntax::node::{Terminal, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::types::resolve_type;
use crate::{semantic, SemanticDiagnostic};

#[cfg(test)]
#[path = "enm_test.rs"]
mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct EnumData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    variants: OrderedHashMap<SmolStr, Variant>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Variant {
    pub id: VariantId,
    pub ty: semantic::TypeId,
}

/// Query implementation of [crate::db::SemanticGroup::struct_semantic_diagnostics].
pub fn enum_semantic_diagnostics(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_enum_semantic_data(enum_id).map(|data| data.diagnostics).unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::enum_variants].
pub fn enum_variants(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Option<OrderedHashMap<SmolStr, Variant>> {
    Some(db.priv_enum_semantic_data(enum_id)?.variants)
}

/// Query implementation of [crate::db::SemanticGroup::priv_enum_semantic_data].
pub fn priv_enum_semantic_data(db: &dyn SemanticGroup, enum_id: EnumId) -> Option<EnumData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_id = enum_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let enum_ast = module_data.enums.get(&enum_id)?;
    let syntax_db = db.upcast();
    let mut variants = OrderedHashMap::default();
    for variant in enum_ast.variants(syntax_db).elements(syntax_db) {
        let id = db.intern_variant(VariantLongId(module_id, variant.stable_ptr()));
        let ty = resolve_type(
            db,
            &mut diagnostics,
            module_id,
            &variant.type_clause(syntax_db).ty(syntax_db),
        );
        let variant_name = variant.name(syntax_db).text(syntax_db);
        if let Some(_other_variant) = variants.insert(variant_name.clone(), Variant { id, ty }) {
            diagnostics.report(&variant, EnumVariantRedefinition { enum_id, variant_name })
        }
    }

    Some(EnumData { diagnostics: diagnostics.diagnostics, variants })
}
