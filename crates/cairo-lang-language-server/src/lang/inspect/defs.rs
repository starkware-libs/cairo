use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LookupItemId;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::Upcast;

use crate::find_definition;
use crate::lang::db::LsSemanticGroup;
use crate::markdown::Markdown;

/// Keeps information about the symbol that is being searched for/inspected.
///
/// This is an ephemeral data structure.
/// Do not store it in any kind of state.
pub enum SymbolDef {
    Item(ItemDef),
}

impl SymbolDef {
    /// Finds definition of the symbol referred by the given identifier.
    pub fn find(db: &RootDatabase, identifier: &ast::TerminalIdentifier) -> Option<Self> {
        // Get the syntax node of the definition.
        let definition_node = {
            let lookup_items = db.collect_lookup_items_stack(&identifier.as_syntax_node())?;
            let stable_ptr = find_definition(db, identifier, &lookup_items)?;
            stable_ptr.lookup(db.upcast())
        };

        // Get the lookup item representing the defining item.
        let lookup_item_id = db.find_lookup_item(&definition_node)?;

        // TODO(mkaput): Support patterns etc.
        Some(Self::Item(ItemDef { lookup_item_id }))
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
