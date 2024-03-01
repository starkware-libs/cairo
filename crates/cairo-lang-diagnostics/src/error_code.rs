use std::fmt;

/// The unique and never-changing identifier of an error or warning.
///
/// For example: `E0001`, `W1234`.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct ErrorCode(&'static str);

impl ErrorCode {
    /// Create a new error code.
    ///
    /// Valid error codes must start with capital `E` or `W` followed by 4 decimal digits.
    /// For example: `E0001`, `W1234`.
    ///
    /// ## Panics
    ///
    /// This function will panic if the `code` does not match error code patter.
    pub const fn new(code: &'static str) -> Self {
        assert!(
            matches!(
                code.as_bytes(),
                [b'E' | b'W', b'0'..=b'9', b'0'..=b'9', b'0'..=b'9', b'0'..=b'9']
            ),
            "Error codes must start with capital `E` or `W` followed by 4 decimal digits."
        );
        Self(code)
    }

    /// Format this error code in a way that is suitable for display in error message.
    ///
    /// ```
    /// # use cairo_lang_diagnostics::error_code;
    /// assert_eq!(error_code!(E0001).format_bracketed(), "[E0001]");
    /// ```
    pub fn format_bracketed(self) -> String {
        format!("[{}]", self)
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
    ($code:expr) => {
        $crate::ErrorCode::new(stringify!($code))
    };
}

/// Utilities for `Option<ErrorCode>`.
pub trait OptionErrorCodeExt {
    fn format_bracketed(self) -> String;
}

impl OptionErrorCodeExt for Option<ErrorCode> {
    /// Format this error code in a way that is suitable for display in error message.
    ///
    /// ```
    /// # use cairo_lang_diagnostics::{error_code, OptionErrorCodeExt};
    /// assert_eq!(Some(error_code!(E0001)).format_bracketed(), "[E0001]");
    /// assert_eq!(None.format_bracketed(), "");
    /// ```
    fn format_bracketed(self) -> String {
        self.map(ErrorCode::format_bracketed).unwrap_or_default()
    }
}
