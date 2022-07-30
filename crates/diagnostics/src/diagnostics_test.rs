use super::{Diagnostic, DiagnosticBag, DiagnosticLocation, OptionWithDiagnostics};

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
    dummy_compute(2);
}

fn dummy_compute(x: usize) -> OptionWithDiagnostics<usize> {
    OptionWithDiagnostics::new(|bag: &mut DiagnosticBag| {
        let param = OptionWithDiagnostics::pure(Some(x * x));
        let res = param.unwrap(bag)?;
        Some(res * res)
    })
}
