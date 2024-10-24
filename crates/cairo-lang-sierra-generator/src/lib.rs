//! Lowering from the semantic model down to Sierra. See [cairo_lang_semantic] and
//! [cairo_lang_sierra].

use db::{SierraGenGroup, SierraGeneratorTypeLongId};

mod ap_change;
mod ap_tracking;
mod block_generator;
pub mod canonical_id_replacer;
pub mod db;
pub mod executables;
mod expr_generator_context;
mod extra_sierra_info;
mod function_generator;
#[cfg(any(feature = "testing", test))]
pub mod function_generator_test_utils;
mod id_allocator;
mod lifetime;
mod local_variables;
mod next_statement_index_fetch;
pub mod pre_sierra;
pub mod program_generator;
pub mod replace_ids;
mod resolve_labels;
mod specialization_context;
pub mod statements_code_locations;
pub mod statements_functions;
pub mod statements_locations;
mod store_variables;
#[cfg(any(feature = "testing", test))]
pub mod test_utils;
mod types;
mod utils;

impl<'a>
    cairo_lang_utils::LookupIntern<
        'a,
        dyn SierraGenGroup + 'a,
        cairo_lang_sierra::program::ConcreteLibfuncLongId,
    > for cairo_lang_sierra::ids::ConcreteLibfuncId
{
    fn lookup_intern(
        &self,
        db: &(impl cairo_lang_utils::Upcast<dyn SierraGenGroup + 'a> + ?Sized),
    ) -> cairo_lang_sierra::program::ConcreteLibfuncLongId {
        SierraGenGroup::lookup_intern_concrete_lib_func(db.upcast(), self.clone())
    }
}

impl<'a>
    cairo_lang_utils::LookupIntern<
        'a,
        dyn SierraGenGroup + 'a,
        cairo_lang_lowering::ids::FunctionId,
    > for cairo_lang_sierra::ids::FunctionId
{
    fn lookup_intern(
        &self,
        db: &(impl cairo_lang_utils::Upcast<dyn SierraGenGroup + 'a> + ?Sized),
    ) -> cairo_lang_lowering::ids::FunctionId {
        SierraGenGroup::lookup_intern_sierra_function(db.upcast(), self.clone())
    }
}

impl<'a> cairo_lang_utils::LookupIntern<'a, dyn SierraGenGroup + 'a, SierraGeneratorTypeLongId>
    for cairo_lang_sierra::ids::ConcreteTypeId
{
    fn lookup_intern(
        &self,
        db: &(impl cairo_lang_utils::Upcast<dyn SierraGenGroup + 'a> + ?Sized),
    ) -> SierraGeneratorTypeLongId {
        SierraGenGroup::lookup_intern_concrete_type(db.upcast(), self.clone())
    }
}

impl<'a>
    cairo_lang_utils::Intern<'a, dyn SierraGenGroup + 'a, cairo_lang_sierra::ids::ConcreteLibfuncId>
    for cairo_lang_sierra::program::ConcreteLibfuncLongId
{
    fn intern(
        self,
        db: &(impl cairo_lang_utils::Upcast<dyn SierraGenGroup + 'a> + ?Sized),
    ) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
        SierraGenGroup::intern_concrete_lib_func(db.upcast(), self)
    }
}

impl<'a> cairo_lang_utils::Intern<'a, dyn SierraGenGroup + 'a, cairo_lang_sierra::ids::FunctionId>
    for cairo_lang_lowering::ids::FunctionId
{
    fn intern(
        self,
        db: &(impl cairo_lang_utils::Upcast<dyn SierraGenGroup + 'a> + ?Sized),
    ) -> cairo_lang_sierra::ids::FunctionId {
        SierraGenGroup::intern_sierra_function(db.upcast(), self)
    }
}

impl<'a>
    cairo_lang_utils::Intern<'a, dyn SierraGenGroup + 'a, cairo_lang_sierra::ids::ConcreteTypeId>
    for SierraGeneratorTypeLongId
{
    fn intern(
        self,
        db: &(impl cairo_lang_utils::Upcast<dyn SierraGenGroup + 'a> + ?Sized),
    ) -> cairo_lang_sierra::ids::ConcreteTypeId {
        SierraGenGroup::intern_concrete_type(db.upcast(), self)
    }
}
