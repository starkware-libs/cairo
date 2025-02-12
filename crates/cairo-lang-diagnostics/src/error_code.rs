use std::fmt;

/// The unique and never-changing identifier of an error or warning.
///
/// Valid error codes must start with capital `E` followed by 4 decimal digits, e.g.: `E0001`.
///
/// Use the [`error_code!`][`crate::error_code!`] macro to construct a well-formed `ErrorCode` in
/// compile-time.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct ErrorCode(&'static str);

impl ErrorCode {
    #[doc(hidden)]
    pub const fn new(code: &'static str) -> Self {
        assert!(
            matches!(code.as_bytes(), [b'E', b'0'..=b'9', b'0'..=b'9', b'0'..=b'9', b'0'..=b'9']),
            "Error codes must start with capital `E` followed by 4 decimal digits."
        );
        Self(code)
    }

    /// Format this error code in a way that is suitable for display in an error message.
    ///
    /// ```
    /// # use cairo_lang_diagnostics::error_code;
    /// assert_eq!(error_code!(E0001).display_bracketed(), "[E0001]");
    /// ```
    pub fn display_bracketed(self) -> String {
        format!("[{}]", self)
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.0, f)
    }
}

/// Constructs an [`ErrorCode`].
///
/// ```
/// # use cairo_lang_diagnostics::{error_code, ErrorCode};
/// let code: ErrorCode = error_code!(E0001);
/// # assert_eq!(format!("{code}"), "E0001");
/// ```
#[macro_export]
macro_rules! error_code {
    ($code:expr) => {{
        use $crate::ErrorCode;
        // NOTE: This is a magic trick to trigger the validation in compile time.
        const ENSURE_CONST: ErrorCode = ErrorCode::new(stringify!($code));
        ENSURE_CONST
    }};
}

/// Utilities for `Option<ErrorCode>`.
pub trait OptionErrorCodeExt {
    fn display_bracketed(self) -> String;
}

impl OptionErrorCodeExt for Option<ErrorCode> {
    /// Format this error code in a way that is suitable for display in error message.
    ///
    /// ```
    /// # use cairo_lang_diagnostics::{error_code, OptionErrorCodeExt};
    /// assert_eq!(Some(error_code!(E0001)).display_bracketed(), "[E0001]");
    /// assert_eq!(None.display_bracketed(), "");
    /// ```
    fn display_bracketed(self) -> String {
        self.map(ErrorCode::display_bracketed).unwrap_or_default()
    }
}
