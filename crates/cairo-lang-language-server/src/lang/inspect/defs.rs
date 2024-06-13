use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LookupItemId;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, ResolvedGenericItem};
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::Upcast;

use crate::lang::db::LsSemanticGroup;
use crate::markdown::Markdown;
use crate::{find_definition, ResolvedItem};

/// Keeps information about the symbol that is being searched for/inspected.
///
/// This is an ephemeral data structure.
/// Do not store it in any kind of state.
pub enum SymbolDef {
    Item(ItemDef),
}

impl SymbolDef {
    /// Finds definition of the symbol referred by the given identifier.
    pub fn find(db: &RootDatabase, identifier: &TerminalIdentifier) -> Option<Self> {
        // Get the resolved item info and the syntax node of the definition.
        let (definition_item, definition_node) = {
            let lookup_items = db.collect_lookup_items_stack(&identifier.as_syntax_node())?;
            let (resolved_item, stable_ptr) = find_definition(db, identifier, &lookup_items)?;
            let node = stable_ptr.lookup(db.upcast());
            (resolved_item, node)
        };

        match definition_item {
            ResolvedItem::Generic(ResolvedGenericItem::GenericConstant(_))
            | ResolvedItem::Generic(ResolvedGenericItem::Module(_))
            | ResolvedItem::Generic(ResolvedGenericItem::GenericFunction(_))
            | ResolvedItem::Generic(ResolvedGenericItem::TraitFunction(_))
            | ResolvedItem::Generic(ResolvedGenericItem::GenericType(_))
            | ResolvedItem::Generic(ResolvedGenericItem::GenericTypeAlias(_))
            | ResolvedItem::Generic(ResolvedGenericItem::GenericImplAlias(_))
            | ResolvedItem::Generic(ResolvedGenericItem::Variant(_))
            | ResolvedItem::Generic(ResolvedGenericItem::Trait(_))
            | ResolvedItem::Generic(ResolvedGenericItem::Impl(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Constant(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Module(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Function(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::TraitFunction(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Type(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Variant(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Trait(_))
            | ResolvedItem::Concrete(ResolvedConcreteItem::Impl(_)) => {
                // Get the lookup item representing the defining item.
                let lookup_item_id = db.find_lookup_item(&definition_node)?;

                Some(Self::Item(ItemDef { lookup_item_id }))
            }

            // TODO(mkaput): Implement hover for variables.
            ResolvedItem::Generic(ResolvedGenericItem::Variable(_)) => None,
        }
    }
}

/// Information about definition of an item (function, trait, impl, module, etc.).
pub struct ItemDef {
    /// The [`LookupItemId`] associated with the item.
    lookup_item_id: LookupItemId,
}

impl ItemDef {
    /// Get item signature without its body.
    pub fn signature(&self, db: &RootDatabase) -> String {
        db.get_item_signature(self.lookup_item_id)
    }

    /// Gets item documentation in a final form usable for display.
    pub fn documentation(&self, db: &RootDatabase) -> Option<Markdown> {
        db.get_item_documentation(self.lookup_item_id)
            // Nullify empty documentation strings in case the compiler fails to output something.
            .and_then(|doc| (!doc.is_empty()).then_some(doc))
            // Convert to a Markdown object and perform usual transformations.
            .map(|doc| {
                let mut md = Markdown::from(doc);
                md.convert_fenced_code_blocks_to_cairo();
                md.ensure_trailing_newline();
                md
            })
    }
}
