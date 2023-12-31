use std::sync::Arc;

use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::Attribute;

use crate::db::SemanticGroup;
use crate::resolve::ResolverData;
use crate::SemanticDiagnostic;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ItemTypeData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub attributes: Vec<Attribute>,
    pub resolver_data: Arc<ResolverData>,
}
