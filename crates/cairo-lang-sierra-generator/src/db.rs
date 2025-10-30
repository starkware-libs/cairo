use std::sync::Arc;

use cairo_lang_diagnostics::{Maybe, MaybeAsRef};
use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_filesystem::ids::{CrateId, Tracked};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::panic::PanicSignatureInfo;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::extensions::{ConcreteType, GenericTypeEx};
use cairo_lang_sierra::ids::ConcreteTypeId;
use lowering::ids::ConcreteFunctionWithBodyId;
use salsa::plumbing::FromId;
use salsa::{Database, Id};
use {cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::program_generator::{self, SierraProgramWithDebug};
use crate::replace_ids::SierraIdReplacer;
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::types::cycle_breaker_info;
use crate::{ap_change, function_generator, pre_sierra, replace_ids};

/// Helper type for Sierra long IDs, which can be either a type long ID or a cycle breaker.
/// This is required for cases where the type long id is self referential.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum SierraGeneratorTypeLongId<'db> {
    /// A normal type long id.
    Regular(Arc<cairo_lang_sierra::program::ConcreteTypeLongId>),
    /// The long id for cycle breakers, such as `Box` and `Nullable`.
    CycleBreaker(semantic::TypeId<'db>),
    /// This is a long id of a phantom type.
    /// Phantom types have a one to one mapping from the semantic type to the Sierra type.
    Phantom(semantic::TypeId<'db>),
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
) -> cairo_lang_sierra::program::ConcreteLibfuncLongId {
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
) -> SierraGeneratorTypeLongId<'db> {
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

    fn lookup_concrete_lib_func(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> cairo_lang_sierra::program::ConcreteLibfuncLongId {
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
    ) -> SierraGeneratorTypeLongId<'db> {
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
    for param in &signature.params {
        let concrete_type_id = db.get_concrete_type_id(param.ty)?;
        all_params.push(concrete_type_id.clone());
    }
    for var in &signature.extra_rets {
        let concrete_type_id = db.get_concrete_type_id(var.ty())?;
        extra_rets.push(concrete_type_id.clone());
    }

    let mut ret_types = implicits;

    let may_panic = !flag_unsafe_panic(db) && db.function_may_panic(lowered_function_id)?;
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
        SierraGeneratorTypeLongId::Regular(long_id) => long_id,
        SierraGeneratorTypeLongId::CycleBreaker(ty) => {
            let info = cycle_breaker_info(db, ty)?;
            return Ok(cairo_lang_sierra::extensions::types::TypeInfo {
                long_id: db.get_concrete_long_type_id(ty)?.as_ref().clone(),
                storable: true,
                droppable: info.droppable,
                duplicatable: info.duplicatable,
                zero_sized: false,
            });
        }
        SierraGeneratorTypeLongId::Phantom(ty) => {
            let long_id = db.get_concrete_long_type_id(ty)?.as_ref().clone();
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
        SierraGeneratorTypeLongId::Regular(long_id) => Ok(long_id),
        SierraGeneratorTypeLongId::Phantom(type_id)
        | SierraGeneratorTypeLongId::CycleBreaker(type_id) => {
            db.get_concrete_long_type_id(type_id).cloned()
        }
    }
}
