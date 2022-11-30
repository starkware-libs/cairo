use defs::ids::{LanguageElementId, ModuleItemId, StructId};
use num_bigint::BigUint;
use semantic::db::SemanticGroup;
use semantic::diagnostic::SemanticDiagnostics;
use semantic::resolve_path::{ResolvedConcreteItem, Resolver};
use semantic::ConcreteImplId;
use sha3::{Digest, Keccak256};
use syntax::node::{ast, TypedSyntaxNode};

#[cfg(test)]
#[path = "contract_test.rs"]
mod test;

/// Represents a declaration of a contract.
pub struct ContractDeclaration {
    /// The id of the struct that defines the contracts storage.
    pub struct_id: StructId,
    /// A list of Expressions that specify the implementations included in the contract.
    /// See resolve_contract_impls(...) for more detail.
    pub impls: Vec<syntax::node::ast::Expr>,
}

/// A variant of eth-keccak that computes a value that fits in a StarkNet field element.
pub fn starknet_keccak(data: &[u8]) -> BigUint {
    let mut hasher = Keccak256::new();
    hasher.update(data);
    let mut result = hasher.finalize();

    // Truncate result to 250 bits.
    *result.first_mut().unwrap() &= 3;
    BigUint::from_bytes_be(&result)
}

/// Finds the structs annotated as contracts in the current compilation units and
/// returns the corresponding ContractDeclarations.
pub fn find_contract_structs(db: &dyn SemanticGroup) -> Vec<ContractDeclaration> {
    let mut contracts = vec![];
    for crate_id in db.crates() {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Some(module_items) = db.module_items(*module_id) else {
                continue;
            };

            for item in module_items.items.values() {
                if let ModuleItemId::Struct(struct_id) = item {
                    if let Some(attrs) = db.struct_attributes(*struct_id) {
                        if let [attr] = attrs.as_slice() {
                            if attr.id == "contract" {
                                contracts.push(ContractDeclaration {
                                    struct_id: *struct_id,
                                    impls: attr.args.clone(),
                                });
                            }
                        };
                    }
                }
            }
        }
    }
    contracts
}

/// Returns the ConcreteImplId's that should be included in a contract.
pub fn resolve_contract_impls(
    db: &(dyn SemanticGroup + 'static),
    contract: &ContractDeclaration,
) -> anyhow::Result<Vec<ConcreteImplId>> {
    let syntax_db = db.upcast();

    let module_file_id = contract.struct_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);

    let mut resolver =
        Resolver::new(db, module_file_id, &db.struct_generic_params(contract.struct_id).unwrap());

    let mut impls = vec![];

    // TODO(ilya): Add error locations.
    for expr in &contract.impls {
        match expr {
            ast::Expr::Path(path) => match resolver.resolve_concrete_path(&mut diagnostics, path) {
                Some(ResolvedConcreteItem::Impl(concrete_impl_id)) => impls.push(concrete_impl_id),
                Some(_item) => anyhow::bail!(
                    "`{}` is not an `impl`.",
                    path.as_syntax_node().get_text(syntax_db)
                ),
                None => {
                    anyhow::bail!(
                        "Failed to resolve `{}`.",
                        path.as_syntax_node().get_text(syntax_db)
                    )
                }
            },

            _ => {
                anyhow::bail!(
                    "Expected a path, Got `{}`.",
                    expr.as_syntax_node().get_text(syntax_db)
                )
            }
        }
    }
    let diag = diagnostics.build();
    if !diag.get_all().is_empty() {
        // TODO(ilya): Print diagnostics.
        anyhow::bail!("Got diagnostics while resolving impl path: {}", diag.format(db.upcast()));
    }
    Ok(impls)
}
