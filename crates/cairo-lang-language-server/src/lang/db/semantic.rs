use std::iter;

use cairo_lang_defs::db::get_all_path_leaves;
use cairo_lang_defs::ids::{
    ConstantLongId, EnumLongId, ExternFunctionLongId, ExternTypeLongId, FileIndex,
    FreeFunctionLongId, ImplAliasLongId, ImplDefLongId, ImplFunctionLongId, ImplItemId,
    LookupItemId, ModuleFileId, ModuleId, ModuleItemId, ModuleTypeAliasLongId, StructLongId,
    TraitFunctionLongId, TraitItemId, TraitLongId, UseLongId,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax::node::ast::ItemModule;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::is_grandparent_of_kind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode, ast};
use cairo_lang_utils::{Intern, Upcast};

// TODO(mkaput): Make this a real Salsa query group with sensible LRU.
/// Language server-specific extensions to the semantic group.
pub trait LsSemanticGroup: Upcast<dyn SemanticGroup> {
    /// Returns a [`LookupItemId`] corresponding to the node or its first parent all the way up to
    /// syntax root in the file.
    ///
    /// This method is a shortcut for getting the first item out of `collect_lookup_items_leaf`.
    /// Returns `None` if there is missing data in the compiler database.
    fn find_lookup_item(&self, node: &SyntaxNode) -> Option<LookupItemId> {
        self.collect_lookup_items_leaf(node)?.into_iter().next()
    }

    /// Returns [`LookupItemId`]s corresponding to the node or its first parent all the way up to
    /// syntax root in the file.
    ///
    /// Returns `None` if there is missing data in the compiler database.
    /// It is not expected for this function to return `Some([])`, but do not assume this.
    fn collect_lookup_items_leaf(&self, node: &SyntaxNode) -> Option<Vec<LookupItemId>> {
        let module_file_id = self.find_module_file_containing_node(node)?;
        let db = self.upcast();

        iter::successors(Some(node.clone()), SyntaxNode::parent)
            .find_map(|node| lookup_item_from_ast(db, module_file_id, node))
    }

    /// Returns [`LookupItemId`]s corresponding to the node and its parents all the way up to syntax
    /// root in the file.
    ///
    /// Returns `None` if there is missing data in the compiler database.
    /// It is not expected for this function to return `Some([])`, but do not assume this.
    fn collect_lookup_items_stack(&self, node: &SyntaxNode) -> Option<Vec<LookupItemId>> {
        let module_file_id = self.find_module_file_containing_node(node)?;

        let db = self.upcast();
        Some(
            iter::successors(Some(node.clone()), SyntaxNode::parent)
                .flat_map(|node| lookup_item_from_ast(db, module_file_id, node).unwrap_or_default())
                .collect(),
        )
    }

    /// Returns a [`ModuleFileId`] containing the node.
    ///
    /// If the node is located in a virtual file generated by a compiler plugin, this method will
    /// return a [`ModuleFileId`] pointing to the main, user-written file of the module.
    fn find_module_file_containing_node(&self, node: &SyntaxNode) -> Option<ModuleFileId> {
        let module_id = self.find_module_containing_node(node)?;
        let file_index = FileIndex(0);
        Some(ModuleFileId(module_id, file_index))
    }

    /// Finds a [`ModuleId`] containing the node.
    ///
    /// If the node is located in a virtual file generated by a compiler plugin, this method will
    /// return the (sub)module of the main, user-written file that leads to the node.
    fn find_module_containing_node(&self, node: &SyntaxNode) -> Option<ModuleId> {
        let db = self.upcast();
        let syntax_db = db.upcast();

        // Get the main module of the main file that leads to the node.
        // The node may be located in a virtual file of a submodule.
        // This code attempts to get the absolute "parent" of both "module" and "file" parts.
        let main_module = {
            // Get the file where the node is located.
            // This might be a virtual file generated by a compiler plugin.
            let node_file_id = node.stable_ptr().file_id(syntax_db);

            // Get the root module of a file containing the node.
            let node_main_module = db.file_modules(node_file_id).ok()?.iter().copied().next()?;

            // Get the main module of the file.
            let main_file = db.module_main_file(node_main_module).ok()?;

            // Get the main module of that file.
            db.file_modules(main_file).ok()?.iter().copied().next()?
        };

        // Get the stack (bottom-up) of submodule names in the file containing the node, in the main
        // module, that lead to the node.
        iter::successors(node.parent(), SyntaxNode::parent)
            .filter(|node| node.kind(syntax_db) == SyntaxKind::ItemModule)
            .map(|node| {
                ItemModule::from_syntax_node(syntax_db, node)
                    .stable_ptr()
                    .name_green(syntax_db)
                    .identifier(syntax_db)
            })
            // Buffer the stack to get DoubleEndedIterator.
            .collect::<Vec<_>>()
            .into_iter()
            // And get id of the (sub)module containing the node by traversing this stack top-down.
            .try_rfold(main_module, |module, name| {
                let ModuleItemId::Submodule(submodule) =
                    db.module_item_by_name(module, name).ok()??
                else {
                    return None;
                };
                Some(ModuleId::Submodule(submodule))
            })
    }
}

impl<T> LsSemanticGroup for T where T: Upcast<dyn SemanticGroup> + ?Sized {}

/// If the ast node is a lookup item, return corresponding ids. Otherwise, returns `None`.
/// See [LookupItemId].
#[tracing::instrument(level = "trace", skip_all)]
fn lookup_item_from_ast(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
    node: SyntaxNode,
) -> Option<Vec<LookupItemId>> {
    let syntax_db = db.upcast();
    // TODO(spapini): Handle trait items.
    Some(match node.kind(syntax_db) {
        SyntaxKind::ItemConstant => vec![LookupItemId::ModuleItem(ModuleItemId::Constant(
            ConstantLongId(
                module_file_id,
                ast::ItemConstant::from_syntax_node(syntax_db, node).stable_ptr(),
            )
            .intern(db),
        ))],
        SyntaxKind::FunctionWithBody => {
            if is_grandparent_of_kind(syntax_db, &node, SyntaxKind::ImplBody) {
                vec![LookupItemId::ImplItem(ImplItemId::Function(
                    ImplFunctionLongId(
                        module_file_id,
                        ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                    )
                    .intern(db),
                ))]
            } else {
                vec![LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
                    FreeFunctionLongId(
                        module_file_id,
                        ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                    )
                    .intern(db),
                ))]
            }
        }
        SyntaxKind::ItemExternFunction => {
            vec![LookupItemId::ModuleItem(ModuleItemId::ExternFunction(
                ExternFunctionLongId(
                    module_file_id,
                    ast::ItemExternFunction::from_syntax_node(syntax_db, node).stable_ptr(),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemExternType => vec![LookupItemId::ModuleItem(ModuleItemId::ExternType(
            ExternTypeLongId(
                module_file_id,
                ast::ItemExternType::from_syntax_node(syntax_db, node).stable_ptr(),
            )
            .intern(db),
        ))],
        SyntaxKind::ItemTrait => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Trait(
                TraitLongId(
                    module_file_id,
                    ast::ItemTrait::from_syntax_node(syntax_db, node).stable_ptr(),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::TraitItemFunction => {
            vec![LookupItemId::TraitItem(TraitItemId::Function(
                TraitFunctionLongId(
                    module_file_id,
                    ast::TraitItemFunction::from_syntax_node(syntax_db, node).stable_ptr(),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemImpl => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Impl(
                ImplDefLongId(
                    module_file_id,
                    ast::ItemImpl::from_syntax_node(syntax_db, node).stable_ptr(),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemStruct => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Struct(
                StructLongId(
                    module_file_id,
                    ast::ItemStruct::from_syntax_node(syntax_db, node).stable_ptr(),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemEnum => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Enum(
                EnumLongId(
                    module_file_id,
                    ast::ItemEnum::from_syntax_node(syntax_db, node).stable_ptr(),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemUse => {
            // Item use is not a lookup item, so we need to collect all UseLeaf, which are lookup
            // items.
            let item_use = ast::ItemUse::from_syntax_node(db.upcast(), node);
            get_all_path_leaves(db.upcast(), &item_use)
                .into_iter()
                .map(|leaf| {
                    let use_long_id = UseLongId(module_file_id, leaf.stable_ptr());
                    LookupItemId::ModuleItem(ModuleItemId::Use(use_long_id.intern(db)))
                })
                .collect()
        }
        SyntaxKind::ItemTypeAlias => vec![LookupItemId::ModuleItem(ModuleItemId::TypeAlias(
            ModuleTypeAliasLongId(
                module_file_id,
                ast::ItemTypeAlias::from_syntax_node(syntax_db, node).stable_ptr(),
            )
            .intern(db),
        ))],
        SyntaxKind::ItemImplAlias => vec![LookupItemId::ModuleItem(ModuleItemId::ImplAlias(
            ImplAliasLongId(
                module_file_id,
                ast::ItemImplAlias::from_syntax_node(syntax_db, node).stable_ptr(),
            )
            .intern(db),
        ))],
        _ => return None,
    })
}
