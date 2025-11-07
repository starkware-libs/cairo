use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::ids::LocationId;
use cairo_lang_sierra::extensions::NamedType;
use cairo_lang_sierra::extensions::uninitialized::UninitializedType;
use cairo_lang_sierra::program::{ConcreteTypeLongId, GenericArg};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use lowering::ids::ConcreteFunctionWithBodyId;
use lowering::{BlockId, Lowered};
use salsa::Database;

use crate::ap_tracking::ApTrackingConfiguration;
use crate::db::SierraGenGroup;
use crate::id_allocator::IdAllocator;
use crate::lifetime::{DropLocation, SierraGenVar, UseLocation, VariableLifetimeResult};
use crate::pre_sierra;

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'db, 'a> {
    db: &'db dyn Database,
    lowered: &'a Lowered<'db>,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lifetime: &'a VariableLifetimeResult,

    var_id_allocator: IdAllocator,
    label_id_allocator: IdAllocator,
    variables: OrderedHashMap<SierraGenVar, cairo_lang_sierra::ids::VarId>,
    block_labels: OrderedHashMap<BlockId, pre_sierra::LabelId<'db>>,

    /// The current ap tracking status.
    ap_tracking_enabled: bool,
    /// Information about where AP tracking should be enabled and disabled.
    ap_tracking_configuration: ApTrackingConfiguration,

    /// The current location for adding statements.
    pub curr_cairo_location: Option<LocationId<'db>>,
    /// The accumulated statements for the expression.
    statements: Vec<pre_sierra::StatementWithLocation<'db>>,
}
impl<'db, 'a> ExprGeneratorContext<'db, 'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(
        db: &'db dyn Database,
        lowered: &'a Lowered<'db>,
        function_id: ConcreteFunctionWithBodyId<'db>,
        lifetime: &'a VariableLifetimeResult,
        ap_tracking_configuration: ApTrackingConfiguration,
    ) -> Self {
        ExprGeneratorContext {
            db,
            lowered,
            function_id,
            lifetime,
            var_id_allocator: IdAllocator::default(),
            label_id_allocator: IdAllocator::default(),
            variables: OrderedHashMap::default(),
            block_labels: OrderedHashMap::default(),
            ap_tracking_enabled: true,
            ap_tracking_configuration,
            statements: vec![],
            curr_cairo_location: None,
        }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_sierra_variable(&mut self) -> cairo_lang_sierra::ids::VarId {
        cairo_lang_sierra::ids::VarId::new(self.var_id_allocator.allocate() as u64)
    }

    /// Returns the SierraGenGroup salsa database.
    pub fn get_db(&self) -> &'db dyn Database {
        self.db
    }

    /// Returns the Sierra variable that corresponds to [lowering::VariableId].
    /// Allocates a new Sierra variable on the first call (for each variable).
    pub fn get_sierra_variable(
        &mut self,
        var: impl Into<SierraGenVar>,
    ) -> cairo_lang_sierra::ids::VarId {
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
        vars: &[lowering::VariableId],
    ) -> Vec<cairo_lang_sierra::ids::VarId> {
        vars.iter().map(|var| self.get_sierra_variable(*var)).collect()
    }

    /// Allocates a label id inside the given function.
    pub fn alloc_label_id(&mut self) -> pre_sierra::LabelId<'db> {
        // TODO(lior): Consider using stable ids, instead of allocating sequential ids.
        alloc_label_id(self.db, self.function_id, &mut self.label_id_allocator)
    }

    /// Generates a label id and a label statement.
    pub fn new_label(&mut self) -> (pre_sierra::Statement<'db>, pre_sierra::LabelId<'db>) {
        let id = self.alloc_label_id();
        (pre_sierra::Statement::Label(pre_sierra::Label { id }), id)
    }

    /// Adds the block to pending_blocks and returns the label id of the block.
    pub fn block_label(&mut self, block_id: BlockId) -> &pre_sierra::LabelId<'db> {
        self.block_labels.entry(block_id).or_insert_with(|| {
            alloc_label_id(self.db, self.function_id, &mut self.label_id_allocator)
        })
    }

    /// Returns the [cairo_lang_sierra::ids::ConcreteTypeId] associated with
    /// [lowering::VariableId].
    pub fn get_variable_sierra_type(
        &self,
        var: impl Into<SierraGenVar>,
    ) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
        Ok(match var.into() {
            SierraGenVar::LoweringVar(lowering_var) => {
                self.db.get_concrete_type_id(self.lowered.variables[lowering_var].ty)?.clone()
            }
            SierraGenVar::UninitializedLocal(lowering_var) => {
                let inner_type =
                    self.db.get_concrete_type_id(self.lowered.variables[lowering_var].ty)?.clone();
                self.db.intern_concrete_type(crate::db::SierraGeneratorTypeLongId::Regular(
                    ConcreteTypeLongId {
                        generic_id: UninitializedType::ID,
                        generic_args: vec![GenericArg::Type(inner_type.clone())],
                    }
                    .into(),
                ))
            }
        })
    }

    /// Returns the block ([lowering::Block]) associated with
    /// [lowering::BlockId].
    /// Assumes `block_id` exists in `self.lowered.blocks`.
    pub fn get_lowered_block(&self, block_id: lowering::BlockId) -> &'a lowering::Block<'db> {
        &self.lowered.blocks[block_id]
    }

    /// Returns the places where variables should be dropped. See [VariableLifetimeResult::drops].
    pub fn get_drops(&self) -> &'a OrderedHashMap<DropLocation, Vec<SierraGenVar>> {
        &self.lifetime.drops
    }

    /// Returns `true` if the given [UseLocation] is the last time a variable is used (namely,
    /// it will not be used after the current statement).
    pub fn is_last_use(&self, use_location: &UseLocation) -> bool {
        self.lifetime.last_use.contains(use_location)
    }

    /// Gets the current ap tracking state.
    pub fn get_ap_tracking(&self) -> bool {
        self.ap_tracking_enabled
    }

    /// Sets the current ap tracking state.
    pub fn set_ap_tracking(&mut self, ap_tracking_state: bool) {
        self.ap_tracking_enabled = ap_tracking_state;
    }

    /// Returns true if ap tracking should be enabled at the end of block_id.
    pub fn should_enable_ap_tracking(&self, block_id: &BlockId) -> bool {
        !self.ap_tracking_enabled
            && self.ap_tracking_configuration.enable_ap_tracking.contains(block_id)
    }

    /// Returns true if ap tracking should be disabled in the beginning of block_id.
    pub fn should_disable_ap_tracking(&self, block_id: &BlockId) -> bool {
        self.ap_tracking_enabled
            && self.ap_tracking_configuration.disable_ap_tracking.contains(block_id)
    }

    /// Adds a statement for the expression.
    pub fn push_statement(&mut self, statement: pre_sierra::Statement<'db>) {
        self.statements.push(pre_sierra::StatementWithLocation {
            statement,
            location: self.curr_cairo_location,
        });
    }

    /// Sets up a location for the next pushed statements.
    pub fn maybe_set_cairo_location(&mut self, location: Option<LocationId<'db>>) {
        if let Some(location) = location {
            self.curr_cairo_location = Some(location);
        }
    }

    /// Returns the statements generated for the expression.
    pub fn statements(self) -> Vec<pre_sierra::StatementWithLocation<'db>> {
        self.statements
    }

    /// Returns the locations per variable of the variables in context.
    pub fn variable_locations(&self) -> Vec<(cairo_lang_sierra::ids::VarId, LocationId<'db>)> {
        self.variables
            .iter()
            .map(|(definition, var)| {
                (
                    var.clone(),
                    match definition {
                        SierraGenVar::LoweringVar(id) | SierraGenVar::UninitializedLocal(id) => {
                            self.lowered.variables[*id].location
                        }
                    },
                )
            })
            .collect()
    }
}

/// A variant of ExprGeneratorContext::alloc_label_id that allows the caller to avoid
/// allocate labels while parts of the context are borrowed.
pub fn alloc_label_id<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    label_id_allocator: &mut IdAllocator,
) -> pre_sierra::LabelId<'db> {
    pre_sierra::LabelLongId { parent: function_id, id: label_id_allocator.allocate() }.intern(db)
}
