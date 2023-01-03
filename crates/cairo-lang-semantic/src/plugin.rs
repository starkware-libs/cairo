use std::any::Any;
use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_defs::plugin::{GeneratedFileAuxData, MacroPlugin};
use cairo_lang_filesystem::span::TextSpan;

use crate::db::SemanticGroup;

pub trait SemanticPlugin: std::fmt::Debug + Sync + Send + AsDynMacroPlugin {}

pub trait AsDynMacroPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a;
}

/// A trait for mapping plugin generated diagnostics to more readable diagnostics.
pub trait DiagnosticMapper:
    std::fmt::Debug + Sync + Send + GeneratedFileAuxData + AsDynGeneratedFileAuxData
{
    fn map_diag(
        &self,
        db: &(dyn SemanticGroup + 'static),
        diag: &dyn Any,
    ) -> Option<PluginMappedDiagnostic>;
}
pub trait AsDynGeneratedFileAuxData {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static);
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PluginMappedDiagnostic {
    pub span: TextSpan,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct DynDiagnosticMapper(pub Arc<dyn DiagnosticMapper>);
impl DynDiagnosticMapper {
    pub fn new<T: DiagnosticMapper + 'static>(mapper: T) -> Self {
        DynDiagnosticMapper(Arc::new(mapper))
    }
}
impl Deref for DynDiagnosticMapper {
    type Target = Arc<dyn DiagnosticMapper>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl GeneratedFileAuxData for DynDiagnosticMapper {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<DynDiagnosticMapper>() {
            self.0.eq(other.0.as_dyn_macro_token())
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TrivialMapper {}
impl GeneratedFileAuxData for TrivialMapper {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
impl AsDynGeneratedFileAuxData for TrivialMapper {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static) {
        self
    }
}
impl DiagnosticMapper for TrivialMapper {
    fn map_diag(
        &self,
        _db: &dyn SemanticGroup,
        _diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        None
    }
}
