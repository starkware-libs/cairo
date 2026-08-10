use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{ExternFunctionId, LanguageElementId, TopLevelLanguageElementId};
use cairo_lang_diagnostics::{Maybe, MaybeAsRef};
use cairo_lang_filesystem::flag::FlagsGroup;
use cairo_lang_filesystem::ids::{CrateId, Tracked};
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::panic::PanicSignatureInfo;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::constant::ConstValueId;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::extensions::{ConcreteType, GenericTypeEx};
use cairo_lang_sierra::ids::ConcreteTypeId;
use lowering::ids::ConcreteFunctionWithBodyId;
use salsa::plumbing::FromId;
use salsa::{Database, Id, Setter};

use crate::program_generator::{self, SierraProgramWithDebug};
use crate::replace_ids::SierraIdReplacer;
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::types::cycle_breaker_info;
use crate::{ap_change, function_generator, pre_sierra, replace_ids};

/// Helper type for Sierra long IDs, which can be either a type long ID or a cycle breaker.
/// This is required for cases where the type long id is self referential.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum SierraGeneratorTypeLongId<'db> {
    /// A normal type long id.
    Regular(Arc<cairo_lang_sierra::program::ConcreteTypeLongId>),
    /// The long id for cycle breakers, such as `Box` and `Nullable`.
    CycleBreaker(semantic::TypeId<'db>),
    /// This is a long id of a phantom type.
    /// Phantom types have a one to one mapping from the semantic type to the Sierra type.
    Phantom(semantic::TypeId<'db>),
}

/// The reserved name of the extern function whose calls are replaced by a constant supplied by an
/// installed [`ExternalConstPlugin`]. Recognized solely by this name, and never reaches Sierra.
pub const EXTERNALLY_PROVIDED_CONST: &str = "__externally_provided_const__";

/// A plugin supplying constant values for calls to the reserved `__externally_provided_const__`
/// extern function.
///
/// Any number of plugins may be installed on the database (see
/// [`SierraGenGroup::set_external_const_plugins`]), so that independent flows - e.g. class hash
/// injection and build configuration values - may each supply their own constants.
pub trait ExternalConstPlugin: std::fmt::Debug + Send + Sync {
    /// Returns the value for the declaration `extern_id` returning `ty`, or `None` to leave it to
    /// the following plugins. Validated against `ty` by the caller.
    ///
    /// Memoized per declaration, so the answer must be stable - a plugin changing its answers must
    /// be reinstalled through [`SierraGenGroup::set_external_const_plugins`].
    fn provide<'db>(
        &self,
        db: &'db dyn Database,
        extern_id: ExternFunctionId<'db>,
        ty: semantic::TypeId<'db>,
    ) -> Option<Maybe<ConstValueId<'db>>>;
}

/// The inputs of [`SierraGenGroup`], set through its `set_*` methods.
#[salsa::input]
pub struct SierraGenGroupInput {
    /// The plugins supplying the values of the externally provided constants.
    #[returns(ref)]
    pub external_const_plugins: Option<Vec<Arc<dyn ExternalConstPlugin>>>,
}

/// Returns a reference to the inputs of [`SierraGenGroup`].
/// The reference is also used to set the inputs to new values.
#[salsa::tracked]
pub fn sierra_gen_group_input(db: &dyn Database) -> SierraGenGroupInput {
    SierraGenGroupInput::new(db, None)
}

/// Wrapper for the concrete libfunc long id, providing a unique id for each libfunc.
#[salsa::interned(revisions = usize::MAX)]
struct ConcreteLibfuncIdLongWrapper {
    id: cairo_lang_sierra::program::ConcreteLibfuncLongId,
}

/// Handle for the concrete libfunc long id, used to lookup the concrete libfunc long id.
struct ConcreteLibfuncHandle(u64);

/// Wrapper for Sierra type long ID, providing a unique ID for each type.
#[salsa::interned(revisions = usize::MAX)]
struct SierraGeneratorTypeLongIdWrapper<'db> {
    id: SierraGeneratorTypeLongId<'db>,
}

/// Handle for the concrete type long id, used to lookup the concrete type long id.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct ConcreteTypeHandle(u64);

/// Handle for the Sierra function ID, used to lookup the Sierra function ID.
struct FunctionHandle(u64);

fn intern_concrete_lib_func(
    db: &dyn Database,
    id: cairo_lang_sierra::program::ConcreteLibfuncLongId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    let interned = ConcreteLibfuncIdLongWrapper::new(db, id);
    cairo_lang_sierra::ids::ConcreteLibfuncId::from(interned.0.as_bits())
}

fn lookup_concrete_lib_func(
    db: &dyn Database,
    id: ConcreteLibfuncHandle,
) -> &cairo_lang_sierra::program::ConcreteLibfuncLongId {
    let interned = ConcreteLibfuncIdLongWrapper::from_id(Id::from_bits(id.0));
    interned.id(db)
}

fn intern_concrete_type<'db>(
    db: &'db dyn Database,
    id: SierraGeneratorTypeLongId<'db>,
) -> cairo_lang_sierra::ids::ConcreteTypeId {
    let interned = SierraGeneratorTypeLongIdWrapper::new(db, id);
    cairo_lang_sierra::ids::ConcreteTypeId::from(interned.0.as_bits())
}

fn lookup_concrete_type<'db>(
    db: &'db dyn Database,
    id: ConcreteTypeHandle,
) -> &'db SierraGeneratorTypeLongId<'db> {
    let interned = SierraGeneratorTypeLongIdWrapper::from_id(Id::from_bits(id.0));
    interned.id(db)
}

fn intern_sierra_function<'db>(
    id: lowering::ids::FunctionId<'db>,
) -> cairo_lang_sierra::ids::FunctionId {
    cairo_lang_sierra::ids::FunctionId::from(id.as_intern_id().as_bits())
}

fn lookup_sierra_function<'db>(id: FunctionHandle) -> lowering::ids::FunctionId<'db> {
    lowering::ids::FunctionId::from_id(Id::from_bits(id.0))
}

pub trait SierraGenGroup: Database {
    fn intern_concrete_lib_func(
        &self,
        id: cairo_lang_sierra::program::ConcreteLibfuncLongId,
    ) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
        intern_concrete_lib_func(self.as_dyn_database(), id)
    }

    fn lookup_concrete_lib_func<'db>(
        &'db self,
        id: &cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> &'db cairo_lang_sierra::program::ConcreteLibfuncLongId {
        lookup_concrete_lib_func(self.as_dyn_database(), ConcreteLibfuncHandle(id.id))
    }

    fn intern_concrete_type<'db>(
        &'db self,
        id: SierraGeneratorTypeLongId<'db>,
    ) -> cairo_lang_sierra::ids::ConcreteTypeId {
        intern_concrete_type(self.as_dyn_database(), id)
    }

    fn lookup_concrete_type<'db>(
        &'db self,
        id: &cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> &'db SierraGeneratorTypeLongId<'db> {
        lookup_concrete_type(self.as_dyn_database(), ConcreteTypeHandle(id.id))
    }

    /// Creates a Sierra function id for a lowering function id.
    // TODO(lior): Can we have the short and long ids in the same place? Currently, the short
    //   id is defined in sierra and the long id is defined in lowering.
    fn intern_sierra_function<'db>(
        &'db self,
        id: lowering::ids::FunctionId<'db>,
    ) -> cairo_lang_sierra::ids::FunctionId {
        intern_sierra_function(id)
    }

    fn lookup_sierra_function<'db>(
        &'db self,
        id: &cairo_lang_sierra::ids::FunctionId,
    ) -> lowering::ids::FunctionId<'db> {
        lookup_sierra_function(FunctionHandle(id.id))
    }

    /// Returns the matching Sierra concrete type ID for a given semantic type ID.
    fn get_concrete_type_id<'db>(
        &'db self,
        type_id: semantic::TypeId<'db>,
    ) -> Maybe<&'db cairo_lang_sierra::ids::ConcreteTypeId> {
        crate::types::get_concrete_type_id(self.as_dyn_database(), type_id).maybe_as_ref()
    }

    /// Returns the ConcreteTypeId of the index enum type with the given index count.
    fn get_index_enum_type_id(
        &self,
        index_count: usize,
    ) -> Maybe<&cairo_lang_sierra::ids::ConcreteTypeId> {
        crate::types::get_index_enum_type_id(self.as_dyn_database(), (), index_count).maybe_as_ref()
    }

    /// Returns the matching Sierra concrete type long ID for a given semantic type ID.
    fn get_concrete_long_type_id<'db>(
        &'db self,
        type_id: semantic::TypeId<'db>,
    ) -> Maybe<&'db Arc<cairo_lang_sierra::program::ConcreteTypeLongId>> {
        crate::types::get_concrete_long_type_id(self.as_dyn_database(), type_id).maybe_as_ref()
    }

    /// Returns if the semantic id has a circular definition.
    fn is_self_referential<'db>(&self, type_id: semantic::TypeId<'db>) -> Maybe<bool> {
        crate::types::is_self_referential(self.as_dyn_database(), type_id)
    }

    /// Returns the semantic type IDs the type is directly dependent on.
    ///
    /// A type depends on another type if it contains or may contain it, as a field or by holding a
    /// reference to it.
    fn type_dependencies<'db>(
        &'db self,
        type_id: semantic::TypeId<'db>,
    ) -> Maybe<&'db [semantic::TypeId<'db>]> {
        Ok(crate::types::type_dependencies(self.as_dyn_database(), type_id).maybe_as_ref()?)
    }

    fn has_in_deps<'db>(
        &self,
        type_id: semantic::TypeId<'db>,
        needle: semantic::TypeId<'db>,
    ) -> Maybe<bool> {
        crate::types::has_in_deps(self.as_dyn_database(), type_id, needle)
    }

    /// Returns the [cairo_lang_sierra::program::FunctionSignature] object for the given function
    /// id.
    fn get_function_signature(
        &self,
        function_id: cairo_lang_sierra::ids::FunctionId,
    ) -> Maybe<&cairo_lang_sierra::program::FunctionSignature> {
        get_function_signature(self.as_dyn_database(), (), function_id).maybe_as_ref()
    }

    /// Returns the [cairo_lang_sierra::extensions::types::TypeInfo] object for the given type id.
    fn get_type_info(
        &self,
        concrete_type_id: cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> Maybe<&cairo_lang_sierra::extensions::types::TypeInfo> {
        get_type_info(self.as_dyn_database(), (), ConcreteTypeHandle(concrete_type_id.id))
            .maybe_as_ref()
    }

    /// Private query to compute Sierra data about a function with body.
    fn priv_function_with_body_sierra_data<'db>(
        &'db self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<&'db function_generator::SierraFunctionWithBodyData<'db>> {
        function_generator::priv_function_with_body_sierra_data(self.as_dyn_database(), function_id)
            .maybe_as_ref()
    }
    /// Returns the Sierra code (as [pre_sierra::Function]) for a given function with body.
    fn function_with_body_sierra<'db>(
        &'db self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<&'db pre_sierra::Function<'db>> {
        self.priv_function_with_body_sierra_data(function_id)?.function.maybe_as_ref()
    }

    /// Private query to generate a dummy function for a given function with body.
    fn priv_get_dummy_function<'db>(
        &'db self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<&'db pre_sierra::Function<'db>> {
        function_generator::priv_get_dummy_function(self.as_dyn_database(), function_id)
            .maybe_as_ref()
    }

    /// Returns the ap change of a given function if it is known at compile time or
    /// [SierraApChange::Unknown] otherwise.
    fn get_ap_change<'db>(
        &self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<SierraApChange> {
        ap_change::get_ap_change(self.as_dyn_database(), function_id)
    }

    /// Private query to returns the type dependencies of a given libfunc.
    fn priv_libfunc_dependencies(
        &self,
        libfunc_id: cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> &[ConcreteTypeId] {
        program_generator::priv_libfunc_dependencies(self.as_dyn_database(), (), libfunc_id)
    }

    /// Returns the [SierraProgramWithDebug] object of the requested functions.
    fn get_sierra_program_for_functions<'db>(
        &'db self,
        requested_function_ids: Vec<ConcreteFunctionWithBodyId<'db>>,
    ) -> Maybe<&'db SierraProgramWithDebug<'db>> {
        program_generator::get_sierra_program_for_functions(
            self.as_dyn_database(),
            (),
            requested_function_ids,
        )
        .maybe_as_ref()
    }

    /// Returns the [SierraProgramWithDebug] object of the requested crates.
    fn get_sierra_program<'db>(
        &'db self,
        requested_crate_ids: Vec<CrateId<'db>>,
    ) -> Maybe<&'db SierraProgramWithDebug<'db>> {
        program_generator::get_sierra_program(self.as_dyn_database(), (), requested_crate_ids)
            .maybe_as_ref()
    }

    /// Returns the constant value supplied for the reserved `__externally_provided_const__` extern
    /// function declared by `extern_id` and returning `ty`.
    ///
    /// The installed [`ExternalConstPlugin`]s are queried in order with the declaration, and the
    /// first supplied value wins, after being validated against `ty`. There is no default value - a
    /// declaration none of the plugins supplies a value for fails Sierra generation.
    ///
    /// Resolution is a query, so a plugin computing its value through the database - e.g. by
    /// compiling another crate - has that work memoized per declaration instead of repeated per
    /// call site, and a value transitively depending on itself is reported as a cycle rather than
    /// recursing endlessly.
    fn externally_provided_const<'db>(
        &'db self,
        extern_id: ExternFunctionId<'db>,
        ty: semantic::TypeId<'db>,
    ) -> Maybe<ConstValueId<'db>> {
        #[salsa::tracked(returns(copy), cycle_result = externally_provided_const_cycle)]
        fn externally_provided_const_tracked<'db>(
            db: &'db dyn Database,
            extern_id: ExternFunctionId<'db>,
            ty: semantic::TypeId<'db>,
        ) -> Maybe<ConstValueId<'db>> {
            let declaration = || {
                format!(
                    "`{}` at {:?}",
                    extern_id.full_path(db),
                    extern_id.stable_location(db).span_in_file(db).user_location(db).debug(db)
                )
            };
            let Some(value) = db
                .external_const_plugins()
                .iter()
                .find_map(|plugin| plugin.provide(db, extern_id, ty))
            else {
                panic!(
                    "No `{EXTERNALLY_PROVIDED_CONST}` plugin provided a value for {}.",
                    declaration()
                );
            };
            let value = value?;
            // The plugins return a value of an arbitrary type; ensure it matches the declared one,
            // as a mismatch would produce a type-incorrect Sierra program.
            if value.ty(db)? != ty {
                panic!(
                    "`{EXTERNALLY_PROVIDED_CONST}` plugin returned a value whose type does not \
                     match the declared return type of {}.",
                    declaration()
                );
            }
            Ok(value)
        }
        /// A plugin supplied a value depending on the value itself, which has no fixed point.
        fn externally_provided_const_cycle<'db>(
            db: &'db dyn Database,
            _id: salsa::Id,
            extern_id: ExternFunctionId<'db>,
            _ty: semantic::TypeId<'db>,
        ) -> Maybe<ConstValueId<'db>> {
            panic!(
                "`{EXTERNALLY_PROVIDED_CONST}` value of `{}` at {:?} depends on itself.",
                extern_id.full_path(db),
                extern_id.stable_location(db).span_in_file(db).user_location(db).debug(db)
            );
        }
        externally_provided_const_tracked(self.as_dyn_database(), extern_id, ty)
    }

    /// Returns the installed [`ExternalConstPlugin`]s, in the order they are queried in.
    fn external_const_plugins(&self) -> &[Arc<dyn ExternalConstPlugin>] {
        sierra_gen_group_input(self.as_dyn_database())
            .external_const_plugins(self)
            .as_deref()
            .unwrap_or_default()
    }

    /// Sets the [`ExternalConstPlugin`]s used to resolve calls to the reserved
    /// `__externally_provided_const__` extern function, replacing the previously installed ones.
    ///
    /// The plugins are queried in order, and the first one supplying a value for a call resolves
    /// it.
    fn set_external_const_plugins(&mut self, plugins: Vec<Arc<dyn ExternalConstPlugin>>) {
        let input = sierra_gen_group_input(self.as_dyn_database());
        input.set_external_const_plugins(self).to(Some(plugins));
    }

    /// Adds an [`ExternalConstPlugin`], to be queried after the already installed ones.
    fn add_external_const_plugin(&mut self, plugin: Arc<dyn ExternalConstPlugin>) {
        let mut plugins = self.external_const_plugins().to_vec();
        plugins.push(plugin);
        self.set_external_const_plugins(plugins);
    }
}
impl<T: Database + ?Sized> SierraGenGroup for T {}

#[salsa::tracked(returns(ref))]
fn get_function_signature(
    db: &dyn Database,
    _tracked: Tracked,
    function_id: cairo_lang_sierra::ids::FunctionId,
) -> Maybe<cairo_lang_sierra::program::FunctionSignature> {
    // TODO(yuval): add another version of this function that directly received semantic FunctionId.
    // Call it from function_generators::get_function_code. Take ret_types from the result instead
    // of only the explicit ret_type. Also use it for params instead of the current logic. Then use
    // it in the end of program_generator::get_sierra_program instead of calling this function from
    // there.
    let lowered_function_id = db.lookup_sierra_function(&function_id);
    let signature = lowered_function_id.signature(db)?;

    let implicits = db
        .function_implicits(lowered_function_id)?
        .iter()
        .map(|ty| db.get_concrete_type_id(*ty).cloned())
        .collect::<Maybe<Vec<ConcreteTypeId>>>()?;

    // TODO(spapini): Handle ret_types in lowering.
    let mut all_params = implicits.clone();
    let mut extra_rets = vec![];
    for ty in &signature.params {
        let concrete_type_id = db.get_concrete_type_id(*ty)?;
        all_params.push(concrete_type_id.clone());
    }
    for ty in &signature.extra_rets {
        let concrete_type_id = db.get_concrete_type_id(*ty)?;
        extra_rets.push(concrete_type_id.clone());
    }

    let mut ret_types = implicits;

    let may_panic = !db.flag_unsafe_panic() && db.function_may_panic(lowered_function_id)?;
    if may_panic {
        let panic_info = PanicSignatureInfo::new(db, &signature);
        ret_types.push(db.get_concrete_type_id(panic_info.actual_return_ty)?.clone());
    } else {
        ret_types.extend(extra_rets);
        // Functions that return the unit type don't have a return type in the signature.
        if !signature.return_type.is_unit(db) {
            ret_types.push(db.get_concrete_type_id(signature.return_type)?.clone());
        }
    }

    Ok(cairo_lang_sierra::program::FunctionSignature { param_types: all_params, ret_types })
}

/// Initializes the [`Database`] database to a proper state.
/// Currently does nothing, but is required to initialize the downcaster.
pub fn init_sierra_gen_group(db: &mut dyn Database) {
    Database::zalsa_register_downcaster(db);
}

#[salsa::tracked(returns(ref))]
fn get_type_info(
    db: &dyn Database,
    _tracked: Tracked,
    id: ConcreteTypeHandle,
) -> Maybe<cairo_lang_sierra::extensions::types::TypeInfo> {
    let long_id = match lookup_concrete_type(db, id) {
        SierraGeneratorTypeLongId::Regular(long_id) => long_id.clone(),
        SierraGeneratorTypeLongId::CycleBreaker(ty) => {
            let info = cycle_breaker_info(db, *ty)?;
            return Ok(cairo_lang_sierra::extensions::types::TypeInfo {
                long_id: db.get_concrete_long_type_id(*ty)?.as_ref().clone(),
                storable: true,
                droppable: info.droppable,
                duplicatable: info.duplicatable,
                zero_sized: false,
            });
        }
        SierraGeneratorTypeLongId::Phantom(ty) => {
            let long_id = db.get_concrete_long_type_id(*ty)?.as_ref().clone();
            return Ok(cairo_lang_sierra::extensions::types::TypeInfo {
                long_id,
                storable: false,
                droppable: false,
                duplicatable: false,
                zero_sized: true,
            });
        }
    };
    let concrete_ty = cairo_lang_sierra::extensions::core::CoreType::specialize_by_id(
        &SierraSignatureSpecializationContext(db),
        &long_id.generic_id,
        &long_id.generic_args,
    )
    .unwrap_or_else(|err| {
        let mut long_id = long_id.as_ref().clone();
        replace_ids::DebugReplacer { db }.replace_generic_args(&mut long_id.generic_args);
        panic!("Got failure while specializing type `{long_id}`: {err}")
    });
    Ok(concrete_ty.info().clone())
}

/// Returns the concrete Sierra long type id given the concrete id.
pub fn sierra_concrete_long_id(
    db: &dyn Database,
    concrete_type_id: cairo_lang_sierra::ids::ConcreteTypeId,
) -> Maybe<Arc<cairo_lang_sierra::program::ConcreteTypeLongId>> {
    match db.lookup_concrete_type(&concrete_type_id) {
        SierraGeneratorTypeLongId::Regular(long_id) => Ok(long_id.clone()),
        SierraGeneratorTypeLongId::Phantom(type_id)
        | SierraGeneratorTypeLongId::CycleBreaker(type_id) => {
            db.get_concrete_long_type_id(*type_id).cloned()
        }
    }
}
