use std::iter;

use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, ModuleItemId, TopLevelLanguageElementId, TraitItemId,
};
use cairo_lang_doc::db::DocGroup;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::pattern::QueryPatternVariablesFromDb;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, ResolvedGenericItem};
use cairo_lang_semantic::{Mutability, Variable};
use cairo_lang_syntax::node::ast::{Param, PatternIdentifier, PatternPtr, TerminalIdentifier};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode};
use cairo_lang_utils::Upcast;
use itertools::Itertools;
use smol_str::SmolStr;
use tracing::error;

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup};
use crate::{find_definition, ResolvedItem};

/// Keeps information about the symbol that is being searched for/inspected.
///
/// This is an ephemeral data structure.
/// Do not store it in any kind of state.
pub enum SymbolDef {
    Item(ItemDef),
    Variable(VariableDef),
}

impl SymbolDef {
    /// Finds definition of the symbol referred by the given identifier.
    #[tracing::instrument(name = "SymbolDef::find", level = "trace", skip_all)]
    pub fn find(db: &AnalysisDatabase, identifier: &TerminalIdentifier) -> Option<Self> {
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
                ItemDef::new(db, &definition_node).map(Self::Item)
            }

            ResolvedItem::Generic(ResolvedGenericItem::Variable(_)) => {
                VariableDef::new(db, definition_node).map(Self::Variable)
            }
        }
    }
}

/// Information about the definition of an item (function, trait, impl, module, etc.).
pub struct ItemDef {
    /// The [`LookupItemId`] associated with the item.
    lookup_item_id: LookupItemId,

    /// Parent item to use as context when building signatures, etc.
    ///
    /// Sometimes, a signature of an item, it might contain parts that are defined elsewhere.
    /// For example, for trait/impl items,
    /// signature may refer to generic params defined in the defining trait/impl.
    /// This reference allows including simplified signatures of such contexts alongside
    /// the signature of this item.
    context_items: Vec<LookupItemId>,
}

impl ItemDef {
    /// Constructs new [`ItemDef`] instance.
    fn new(db: &AnalysisDatabase, definition_node: &SyntaxNode) -> Option<Self> {
        let mut lookup_item_ids = db.collect_lookup_items_stack(definition_node)?.into_iter();

        // Pull the lookup item representing the defining item.
        let lookup_item_id = lookup_item_ids.next()?;

        // Collect context items.
        let context_items = lookup_item_ids
            .take_while(|item| {
                matches!(
                    item,
                    LookupItemId::ModuleItem(ModuleItemId::Struct(_))
                        | LookupItemId::ModuleItem(ModuleItemId::Enum(_))
                        | LookupItemId::ModuleItem(ModuleItemId::Trait(_))
                        | LookupItemId::ModuleItem(ModuleItemId::Impl(_))
                        | LookupItemId::TraitItem(TraitItemId::Impl(_))
                )
            })
            .collect();

        Some(Self { lookup_item_id, context_items })
    }

    /// Get item signature without its body including signatures of its contexts.
    pub fn signature(&self, db: &AnalysisDatabase) -> String {
        let contexts = self.context_items.iter().map(|item| db.get_item_signature(*item)).rev();
        let this = iter::once(db.get_item_signature(self.lookup_item_id));
        contexts.chain(this).map(fmt).join("\n")
    }

    /// Gets item documentation in a final form usable for display.
    pub fn documentation(&self, db: &AnalysisDatabase) -> Option<String> {
        db.get_item_documentation(self.lookup_item_id)
    }

    /// Gets the full path (including crate name and defining trait/impl if applicable)
    /// to the module containing the item.
    pub fn definition_path(&self, db: &AnalysisDatabase) -> String {
        let defs_db = db.upcast();
        match self.lookup_item_id {
            LookupItemId::ModuleItem(item) => item.parent_module(defs_db).full_path(defs_db),
            LookupItemId::TraitItem(item) => item.trait_id(defs_db).full_path(defs_db),
            LookupItemId::ImplItem(item) => item.impl_def_id(defs_db).full_path(defs_db),
        }
    }
}

/// Information about the definition of a variable (local, function parameter).
pub struct VariableDef {
    name: SmolStr,
    var: Variable,
}

impl VariableDef {
    /// Constructs new [`VariableDef`] instance.
    fn new(db: &AnalysisDatabase, definition_node: SyntaxNode) -> Option<Self> {
        match definition_node.kind(db.upcast()) {
            SyntaxKind::TerminalIdentifier => {
                let definition_node = definition_node.parent()?;
                match definition_node.kind(db.upcast()) {
                    SyntaxKind::PatternIdentifier => {
                        let pattern_identifier =
                            PatternIdentifier::from_syntax_node(db.upcast(), definition_node);
                        Self::new_pattern_identifier(db, pattern_identifier)
                    }
                    kind => {
                        error!(
                            "variable definition node parent is not an pattern identifier: \
                             {kind:?}"
                        );
                        None
                    }
                }
            }

            SyntaxKind::Param => {
                let param = Param::from_syntax_node(db.upcast(), definition_node);
                Self::new_param(db, param)
            }

            kind => {
                error!("variable definition node is not an identifier nor param: {kind:?}");
                None
            }
        }
    }

    /// Constructs new [`VariableDef`] instance for [`PatternIdentifier`].
    fn new_pattern_identifier(
        db: &AnalysisDatabase,
        pattern_identifier: PatternIdentifier,
    ) -> Option<Self> {
        let name = pattern_identifier.name(db.upcast()).text(db.upcast());

        // Get the function which contains the variable/parameter.
        let function_id =
            db.find_lookup_item(&pattern_identifier.as_syntax_node())?.function_with_body()?;

        // Get semantic model for the pattern.
        let pattern = {
            let pattern_ptr = PatternPtr::from(pattern_identifier.stable_ptr());
            let id = db.lookup_pattern_by_ptr(function_id, pattern_ptr).ok()?;
            db.pattern_semantic(function_id, id)
        };

        // Extract variable semantic from the found pattern.
        let var = pattern
            .variables(&QueryPatternVariablesFromDb(db.upcast(), function_id))
            .into_iter()
            .find(|pv| pv.name == name)?
            .var
            .into();

        Some(Self { name, var })
    }

    /// Constructs new [`VariableDef`] instance for [`Param`].
    fn new_param(db: &AnalysisDatabase, param: Param) -> Option<Self> {
        let name = param.name(db.upcast()).text(db.upcast());

        // Get the function which contains the variable/parameter.
        let function_id = db.find_lookup_item(&param.as_syntax_node())?.function_with_body()?;

        // Get function signature.
        let signature = db.function_with_body_signature(function_id).ok()?;

        // Extract parameter semantic from the found signature.
        let var = signature.params.into_iter().find(|p| p.name == name)?.into();

        Some(Self { name, var })
    }

    /// Gets variable signature, which tries to resemble the way how it is defined in code.
    pub fn signature(&self, db: &AnalysisDatabase) -> String {
        let Self { name, var } = self;

        let prefix = match var {
            Variable::Local(_) => "let ",
            Variable::Param(_) => "",
        };

        let mutability = match var {
            Variable::Local(local) => {
                if local.is_mut {
                    "mut "
                } else {
                    ""
                }
            }
            Variable::Param(param) => match param.mutability {
                Mutability::Immutable => "",
                Mutability::Mutable => "mut ",
                Mutability::Reference => "ref ",
            },
        };

        let ty = var.ty().format(db.upcast());

        format!("{prefix}{mutability}{name}: {ty}")
    }
}

/// Run Cairo formatter over code with extra post-processing that is specific to signatures.
fn fmt(code: String) -> String {
    let code = cairo_lang_formatter::format_string(&SimpleParserDatabase::default(), code);

    code
        // Trim any whitespace that formatter tends to leave.
        .trim_end()
        // Trim trailing semicolons, that are present in trait/impl functions, constants, etc.
        // and that formatter tends to put in separate line.
        .trim_end_matches("\n;")
        .to_owned()
}
