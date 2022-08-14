use diagnostics_proc_macros::with_diagnostics;

use super::{Diagnostic, DiagnosticBag, DiagnosticLocation, WithDiagnostics};

struct SimpleDiag {}
impl Diagnostic for SimpleDiag {
    fn format(&self, _db: &dyn filesystem::db::FilesGroup) -> String {
        unimplemented!();
    }

    fn location(&self, _db: &dyn filesystem::db::FilesGroup) -> DiagnosticLocation {
        unimplemented!();
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[test]
fn test_diagnostics() {
    let mut bag = DiagnosticBag::default();
    let diagnostic = SimpleDiag {};
    bag.add(Box::new(diagnostic));
    bag.0[0].as_any().downcast_ref::<SimpleDiag>().expect("Unexpected type");
}

#[test]
fn test_option_with_diagnostics() {
    dummy_compute_macro(2);
}

#[with_diagnostics]
fn dummy_compute_macro(bag: &mut DiagnosticBag, x: usize) -> Option<usize> {
    let param = WithDiagnostics::pure(Some(x * x));
    let res = param.unwrap(bag)?;
    Some(res * res)
}
