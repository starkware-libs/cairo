use std::sync::Arc;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::panic::PanicSignatureInfo;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::extensions::{ConcreteType, GenericTypeEx};
use cairo_lang_sierra::ids::ConcreteTypeId;
use lowering::ids::ConcreteFunctionWithBodyId;
use {cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::program_generator::{self, SierraProgramWithDebug};
use crate::replace_ids::SierraIdReplacer;
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::types::cycle_breaker_info;
use crate::{ap_change, function_generator, pre_sierra, replace_ids};

/// Helper type for Sierra long ids, which can be either a type long id or a cycle breaker.
/// This is required for cases where the type long id is self referential.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum SierraGeneratorTypeLongId<'db> {
    /// A normal type long id.
    Regular(Arc<cairo_lang_sierra::program::ConcreteTypeLongId>),
    /// The long id for cycle breakers, such as `Box` and `Nullable`.
    CycleBreaker(semantic::TypeId<'db>),
    /// This is a long id of a phantom type.
    /// Phantom types have a one to one mapping from the semantic type to the sierra type.
    Phantom(semantic::TypeId<'db>),
}

#[salsa::interned(revisions = usize::MAX)]
struct ConcreteLibfuncIdLongWrapper {
    id: cairo_lang_sierra::program::ConcreteLibfuncLongId,
}

#[salsa::interned(revisions = usize::MAX)]
struct SierraGeneratorTypeLongIdWrapper<'db> {
    id: SierraGeneratorTypeLongId<'db>,
}

#[salsa::interned(revisions = usize::MAX)]
struct LoweringFunctionIdWrapper<'db> {
    id: lowering::ids::FunctionId<'db>,
}

fn intern_concrete_lib_func(
    db: &dyn SierraGenGroup,
    id: cairo_lang_sierra::program::ConcreteLibfuncLongId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    let interned = ConcreteLibfuncIdLongWrapper::new(db, id);
    cairo_lang_sierra::ids::ConcreteLibfuncId::from(interned.0.as_bits())
}

use salsa::plumbing::FromId;
use salsa::{Database, Id};

fn lookup_concrete_lib_func(
    db: &dyn SierraGenGroup,
    id: cairo_lang_sierra::ids::ConcreteLibfuncId,
) -> cairo_lang_sierra::program::ConcreteLibfuncLongId {
    let interned = ConcreteLibfuncIdLongWrapper::from_id(unsafe { Id::from_bits(id.id) });
    interned.id(db)
}

fn intern_concrete_type<'db>(
    db: &'db dyn SierraGenGroup,
    id: SierraGeneratorTypeLongId<'db>,
) -> cairo_lang_sierra::ids::ConcreteTypeId {
    let interned = SierraGeneratorTypeLongIdWrapper::new(db, id);
    cairo_lang_sierra::ids::ConcreteTypeId::from(interned.0.as_bits())
}

fn lookup_concrete_type<'db>(
    db: &'db dyn SierraGenGroup,
    id: cairo_lang_sierra::ids::ConcreteTypeId,
) -> SierraGeneratorTypeLongId<'db> {
    let interned = SierraGeneratorTypeLongIdWrapper::from_id(unsafe { Id::from_bits(id.id) });
    interned.id(db)
}
fn intern_sierra_function<'db>(
    db: &'db dyn SierraGenGroup,
    id: lowering::ids::FunctionId<'db>,
) -> cairo_lang_sierra::ids::FunctionId {
    let interned = LoweringFunctionIdWrapper::new(db, id);
    cairo_lang_sierra::ids::FunctionId::from(interned.0.as_bits())
}

fn lookup_sierra_function<'db>(
    db: &'db dyn SierraGenGroup,
    id: cairo_lang_sierra::ids::FunctionId,
) -> lowering::ids::FunctionId<'db> {
    let interned = LoweringFunctionIdWrapper::from_id(unsafe { Id::from_bits(id.id) });
    interned.id(db)
}

#[cairo_lang_proc_macros::query_group]
pub trait SierraGenGroup: Database {
    fn intern_concrete_lib_func(
        &self,
        id: cairo_lang_sierra::program::ConcreteLibfuncLongId,
    ) -> cairo_lang_sierra::ids::ConcreteLibfuncId;

    fn lookup_concrete_lib_func(
        &self,
        id: cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> cairo_lang_sierra::program::ConcreteLibfuncLongId;

    fn intern_concrete_type<'db>(
        &'db self,
        id: SierraGeneratorTypeLongId<'db>,
    ) -> cairo_lang_sierra::ids::ConcreteTypeId;

    fn lookup_concrete_type<'db>(
        &'db self,
        id: cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> SierraGeneratorTypeLongId<'db>;

    /// Creates a Sierra function id for a lowering function id.
    // TODO(lior): Can we have the short and long ids in the same place? Currently, the short
    //   id is defined in sierra and the long id is defined in lowering.
    fn intern_sierra_function<'db>(
        &'db self,
        id: lowering::ids::FunctionId<'db>,
    ) -> cairo_lang_sierra::ids::FunctionId;

    fn lookup_sierra_function<'db>(
        &'db self,
        id: cairo_lang_sierra::ids::FunctionId,
    ) -> lowering::ids::FunctionId<'db>;

    /// Returns the matching sierra concrete type id for a given semantic type id.
    #[salsa::invoke(crate::types::get_concrete_type_id)]
    fn get_concrete_type_id<'db>(
        &'db self,
        type_id: semantic::TypeId<'db>,
    ) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId>;

    /// Returns the ConcreteTypeId of the index enum type with the given index count.
    #[salsa::invoke(crate::types::get_index_enum_type_id)]
    fn get_index_enum_type_id(
        &self,
        index_count: usize,
    ) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId>;

    /// Returns the matching sierra concrete type long id for a given semantic type id.
    #[salsa::invoke(crate::types::get_concrete_long_type_id)]
    fn get_concrete_long_type_id<'db>(
        &self,
        type_id: semantic::TypeId<'db>,
    ) -> Maybe<Arc<cairo_lang_sierra::program::ConcreteTypeLongId>>;

    /// Returns if the semantic id has a circular definition.
    #[salsa::invoke(crate::types::is_self_referential)]
    fn is_self_referential<'db>(&self, type_id: semantic::TypeId<'db>) -> Maybe<bool>;

    /// Returns the semantic type ids the type is directly dependent on.
    ///
    /// A type depends on another type if it contains or may contain it, as a field or by holding a
    /// reference to it.
    #[salsa::invoke(crate::types::type_dependencies)]
    fn type_dependencies<'db>(
        &'db self,
        type_id: semantic::TypeId<'db>,
    ) -> Maybe<Arc<Vec<semantic::TypeId<'db>>>>;

    #[salsa::invoke(crate::types::has_in_deps)]
    #[salsa::cycle(crate::types::has_in_deps_cycle)]
    fn has_in_deps<'db>(
        &self,
        type_id: semantic::TypeId<'db>,
        needle: semantic::TypeId<'db>,
    ) -> Maybe<bool>;

    /// Returns the [cairo_lang_sierra::program::FunctionSignature] object for the given function
    /// id.
    fn get_function_signature(
        &self,
        function_id: cairo_lang_sierra::ids::FunctionId,
    ) -> Maybe<Arc<cairo_lang_sierra::program::FunctionSignature>>;

    /// Returns the [cairo_lang_sierra::extensions::types::TypeInfo] object for the given type id.
    fn get_type_info(
        &self,
        concrete_type_id: cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> Maybe<Arc<cairo_lang_sierra::extensions::types::TypeInfo>>;

    /// Private query to compute Sierra data about a function with body.
    #[salsa::invoke(function_generator::priv_function_with_body_sierra_data)]
    fn priv_function_with_body_sierra_data<'db>(
        &'db self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<function_generator::SierraFunctionWithBodyData<'db>>;
    /// Returns the Sierra code (as [pre_sierra::Function]) for a given function with body.
    #[salsa::invoke(function_generator::function_with_body_sierra)]
    fn function_with_body_sierra<'db>(
        &'db self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<Arc<pre_sierra::Function<'db>>>;

    /// Private query to generate a dummy function for a given function with body.
    #[salsa::invoke(function_generator::priv_get_dummy_function)]
    fn priv_get_dummy_function<'db>(
        &'db self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<Arc<pre_sierra::Function<'db>>>;

    /// Returns the ap change of a given function if it is known at compile time or
    /// [SierraApChange::Unknown] otherwise.
    #[salsa::invoke(ap_change::get_ap_change)]
    fn get_ap_change<'db>(
        &self,
        function_id: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<SierraApChange>;

    /// Private query to returns the type dependencies of a given libfunc.
    #[salsa::invoke(program_generator::priv_libfunc_dependencies)]
    fn priv_libfunc_dependencies(
        &self,
        libfunc_id: cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> Arc<[ConcreteTypeId]>;

    /// Returns the [SierraProgramWithDebug] object of the requested functions.
    #[salsa::invoke(program_generator::get_sierra_program_for_functions)]
    fn get_sierra_program_for_functions<'db>(
        &'db self,
        requested_function_ids: Vec<ConcreteFunctionWithBodyId<'db>>,
    ) -> Maybe<Arc<SierraProgramWithDebug<'db>>>;

    /// Returns the [SierraProgramWithDebug] object of the requested crates.
    #[salsa::invoke(program_generator::get_sierra_program)]
    fn get_sierra_program<'db>(
        &'db self,
        requested_crate_ids: Vec<CrateId<'db>>,
    ) -> Maybe<Arc<SierraProgramWithDebug<'db>>>;
}

fn get_function_signature(
    db: &dyn SierraGenGroup,
    function_id: cairo_lang_sierra::ids::FunctionId,
) -> Maybe<Arc<cairo_lang_sierra::program::FunctionSignature>> {
    // TODO(yuval): add another version of this function that directly received semantic FunctionId.
    // Call it from function_generators::get_function_code. Take ret_types from the result instead
    // of only the explicit ret_type. Also use it for params instead of the current logic. Then use
    // it in the end of program_generator::get_sierra_program instead of calling this function from
    // there.
    let lowered_function_id = db.lookup_sierra_function(function_id);
    let signature = lowered_function_id.signature(db)?;

    let implicits = db
        .function_implicits(lowered_function_id)?
        .iter()
        .map(|ty| db.get_concrete_type_id(*ty))
        .collect::<Maybe<Vec<ConcreteTypeId>>>()?;

    // TODO(spapini): Handle ret_types in lowering.
    let mut all_params = implicits.clone();
    let mut extra_rets = vec![];
    for param in &signature.params {
        let concrete_type_id = db.get_concrete_type_id(param.ty())?;
        all_params.push(concrete_type_id.clone());
    }
    for var in &signature.extra_rets {
        let concrete_type_id = db.get_concrete_type_id(var.ty())?;
        extra_rets.push(concrete_type_id);
    }

    let mut ret_types = implicits;

    let may_panic = !flag_unsafe_panic(db) && db.function_may_panic(lowered_function_id)?;
    if may_panic {
        let panic_info = PanicSignatureInfo::new(db, &signature);
        ret_types.push(db.get_concrete_type_id(panic_info.actual_return_ty)?);
    } else {
        ret_types.extend(extra_rets);
        // Functions that return the unit type don't have a return type in the signature.
        if !signature.return_type.is_unit(db) {
            ret_types.push(db.get_concrete_type_id(signature.return_type)?);
        }
    }

    Ok(Arc::new(cairo_lang_sierra::program::FunctionSignature {
        param_types: all_params,
        ret_types,
    }))
}

/// Initializes the [`SierraGenGroup`] database to a proper state.
/// Currently does nothing, but is required to initialize the downcaster.
pub fn init_sierra_gen_group(db: &mut dyn SierraGenGroup) {
    SierraGenGroup::zalsa_register_downcaster(db);
}

fn get_type_info(
    db: &dyn SierraGenGroup,
    concrete_type_id: cairo_lang_sierra::ids::ConcreteTypeId,
) -> Maybe<Arc<cairo_lang_sierra::extensions::types::TypeInfo>> {
    let long_id = match db.lookup_concrete_type(concrete_type_id) {
        SierraGeneratorTypeLongId::Regular(long_id) => long_id,
        SierraGeneratorTypeLongId::CycleBreaker(ty) => {
            let info = cycle_breaker_info(db, ty)?;
            return Ok(Arc::new(cairo_lang_sierra::extensions::types::TypeInfo {
                long_id: db.get_concrete_long_type_id(ty)?.as_ref().clone(),
                storable: true,
                droppable: info.droppable,
                duplicatable: info.duplicatable,
                zero_sized: false,
            }));
        }
        SierraGeneratorTypeLongId::Phantom(ty) => {
            let long_id = db.get_concrete_long_type_id(ty)?.as_ref().clone();
            return Ok(Arc::new(cairo_lang_sierra::extensions::types::TypeInfo {
                long_id,
                storable: false,
                droppable: false,
                duplicatable: false,
                zero_sized: true,
            }));
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
    Ok(Arc::new(concrete_ty.info().clone()))
}

/// Returns the concrete Sierra long type id given the concrete id.
pub fn sierra_concrete_long_id(
    db: &dyn SierraGenGroup,
    concrete_type_id: cairo_lang_sierra::ids::ConcreteTypeId,
) -> Maybe<Arc<cairo_lang_sierra::program::ConcreteTypeLongId>> {
    match db.lookup_concrete_type(concrete_type_id) {
        SierraGeneratorTypeLongId::Regular(long_id) => Ok(long_id),
        SierraGeneratorTypeLongId::Phantom(type_id)
        | SierraGeneratorTypeLongId::CycleBreaker(type_id) => db.get_concrete_long_type_id(type_id),
    }
}
