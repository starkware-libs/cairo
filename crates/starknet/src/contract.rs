use defs::ids::{LanguageElementId, ModuleItemId, StructId};
use semantic::db::SemanticGroup;
use semantic::diagnostic::SemanticDiagnostics;
use semantic::resolve_path::{ResolvedConcreteItem, Resolver};
use semantic::ConcreteImplId;
use syntax::node::{ast, TypedSyntaxNode};

#[cfg(test)]
#[path = "contract_test.rs"]
mod test;

// Represents a contract defintion
pub struct ContractDefinition {
    // the is of the struct the defined the contracts storage.
    pub id: StructId,
    // The implementations included in the contract.
    pub impls: Vec<ConcreteImplId>,
}

pub fn find_contract_structs(db: &dyn SemanticGroup) -> Vec<ContractDefinition> {
    let mut contracts = vec![];
    for crate_id in db.crates() {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Some(module_items) = db.module_items(*module_id) else {
                continue;
            };

            for (_name, item) in module_items.items.iter() {
                if let ModuleItemId::Struct(struct_id) = item {
                    {
                        if let Some(attrs) = db.struct_attributes(*struct_id) {
                            if let [attr] = attrs.as_slice() {
                                if attr.id == "contract" {
                                    contracts.push(ContractDefinition {
                                        id: *struct_id,
                                        impls: resolve_impls(db, struct_id, &attr.args),
                                    });
                                }
                            };
                        }
                    }
                }
            }
        }
    }
    contracts
}

// Resolve impl_exprs into ConcreteImplId's.
fn resolve_impls(
    db: &dyn SemanticGroup,
    struct_id: &StructId,
    impl_exprs: &[ast::Expr],
) -> Vec<ConcreteImplId> {
    let syntax_db = db.upcast();

    let module_file_id = struct_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);

    let mut resolver =
        Resolver::new(db, module_file_id, &db.struct_generic_params(*struct_id).unwrap());

    let mut impls = vec![];
    for expr in impl_exprs {
        match expr {
            ast::Expr::Path(path) => match resolver.resolve_concrete_path(&mut diagnostics, path) {
                Some(ResolvedConcreteItem::Impl(concrete_impl_id)) => impls.push(concrete_impl_id),
                Some(item) => panic!("Expected an `impl`, Got {:?}.", item),
                None => {
                    panic!("Failed to resolve {}.", path.as_syntax_node().text(syntax_db).unwrap())
                }
            },

            _ => panic!("Expected a path, Got {}.", expr.as_syntax_node().text(syntax_db).unwrap()),
        }
    }
    let diag = diagnostics.build();
    diag.expect("Unexpected diagnostics.");
    impls
}
