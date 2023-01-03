use cairo_defs::ids::FreeFunctionId;
use cairo_diagnostics::Maybe;
use cairo_sierra::extensions::uninitialized::UninitializedType;
use cairo_sierra::extensions::NamedType;
use cairo_sierra::program::{ConcreteTypeLongId, GenericArg};
use cairo_utils::ordered_hash_map::OrderedHashMap;
use cairo_utils::unordered_hash_map::UnorderedHashMap;

use crate::db::SierraGenGroup;
use crate::id_allocator::IdAllocator;
use crate::lifetime::{DropLocation, SierraGenVar, VariableLifetimeResult};
use crate::pre_sierra;

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SierraGenGroup,
    lowered: &'a cairo_lowering::lower::Lowered,
    function_id: FreeFunctionId,
    // TODO(lior): Remove `allow(dead_code)` once this field is used.
    #[allow(dead_code)]
    lifetime: &'a VariableLifetimeResult,

    var_id_allocator: IdAllocator,
    label_id_allocator: IdAllocator,
    variables: UnorderedHashMap<SierraGenVar, cairo_sierra::ids::VarId>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(
        db: &'a dyn SierraGenGroup,
        lowered: &'a cairo_lowering::lower::Lowered,
        function_id: FreeFunctionId,
        lifetime: &'a VariableLifetimeResult,
    ) -> Self {
        ExprGeneratorContext {
            db,
            lowered,
            function_id,
            lifetime,
            var_id_allocator: IdAllocator::default(),
            label_id_allocator: IdAllocator::default(),
            variables: UnorderedHashMap::default(),
        }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_sierra_variable(&mut self) -> cairo_sierra::ids::VarId {
        cairo_sierra::ids::VarId::from_usize(self.var_id_allocator.allocate())
    }

    /// Returns the SierraGenGroup salsa database.
    pub fn get_db(&self) -> &'a dyn SierraGenGroup {
        self.db
    }

    /// Returns the Sierra variable that corresponds to [cairo_lowering::VariableId].
    /// Allocates a new Sierra variable on the first call (for each variable).
    pub fn get_sierra_variable(
        &mut self,
        var: impl Into<SierraGenVar>,
    ) -> cairo_sierra::ids::VarId {
        let var: SierraGenVar = var.into();
        if let Some(sierra_var) = self.variables.get(&var) {
            return sierra_var.clone();
        }

        let sierra_var = self.allocate_sierra_variable();
        self.variables.insert(var, sierra_var.clone());
        sierra_var
    }

    /// Same as [Self::get_sierra_variable] except that it operates of a list of variables.
    pub fn get_sierra_variables(
        &mut self,
        vars: &[cairo_lowering::VariableId],
    ) -> Vec<cairo_sierra::ids::VarId> {
        vars.iter().map(|var| self.get_sierra_variable(*var)).collect()
    }

    /// Generates a label id and a label statement.
    // TODO(lior): Consider using stable ids, instead of allocating sequential ids.
    pub fn new_label(&mut self) -> (pre_sierra::Statement, pre_sierra::LabelId) {
        let id = self.db.intern_label_id(pre_sierra::LabelLongId {
            parent: self.function_id,
            id: self.label_id_allocator.allocate(),
        });
        (pre_sierra::Statement::Label(pre_sierra::Label { id }), id)
    }

    /// Returns the [cairo_sierra::ids::ConcreteTypeId] associated with
    /// [cairo_lowering::VariableId].
    pub fn get_variable_sierra_type(
        &self,
        var: impl Into<SierraGenVar>,
    ) -> Maybe<cairo_sierra::ids::ConcreteTypeId> {
        Ok(match var.into() {
            SierraGenVar::LoweringVar(lowering_var) => {
                self.db.get_concrete_type_id(self.lowered.variables[lowering_var].ty)?
            }
            SierraGenVar::UninitializedLocal(lowering_var) => {
                let inner_type =
                    self.db.get_concrete_type_id(self.lowered.variables[lowering_var].ty)?;
                self.db.intern_concrete_type(ConcreteTypeLongId {
                    generic_id: UninitializedType::ID,
                    generic_args: vec![GenericArg::Type(inner_type)],
                })
            }
        })
    }

    /// Returns the block ([cairo_lowering::Block]) associated with [cairo_lowering::BlockId].
    pub fn get_lowered_block(
        &self,
        block_id: cairo_lowering::BlockId,
    ) -> &'a cairo_lowering::Block {
        &self.lowered.blocks[block_id]
    }

    /// Returns the places where variables should be dropped. See [VariableLifetimeResult::drops].
    pub fn get_drops(&self) -> &'a OrderedHashMap<DropLocation, Vec<SierraGenVar>> {
        &self.lifetime.drops
    }
}
