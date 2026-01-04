use cairo_lang_macro::ProcMacroResult;
use indoc::formatdoc;

pub(crate) fn format_proc_macro_result(raw_result: ProcMacroResult) -> String {
    let none = "None";

    let mut token_stream = raw_result.token_stream.to_string();
    let mut diagnostics = String::new();
    for d in raw_result.diagnostics {
        diagnostics += format!("====\n{:?}: {}====", d.severity, d.message).as_str();
    }

    if token_stream.is_empty() {
        token_stream = none.to_string();
    }
    if diagnostics.is_empty() {
        diagnostics = none.to_string();
    }

    formatdoc! {
        "
        TokenStream:

        {}

        Diagnostics:

        {}

        AuxData:

        {:?}
        ",
        token_stream, diagnostics, raw_result.aux_data
    }
}
