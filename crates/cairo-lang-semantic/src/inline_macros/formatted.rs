use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::{try_extract_matches, OptionHelper};
use num_bigint::{BigInt, Sign};

use super::{try_extract_unnamed_arg, unsupported_bracket_diagnostic};

/// Information about a formatted string macro.
pub struct FormattedInfo {
    /// The formatter to add the formatted string into.
    pub formatter: RewriteNode,
    /// The format string argument.
    pub format_string_arg: ast::Arg,
    /// The format string content.
    pub format_string: String,
    /// The positional arguments for the format string.
    pub args: Vec<ast::Expr>,
}
impl FormattedInfo {
    /// Extracts the arguments from a formatted string macro.
    /// `defined_formatter` should be provided if it is defined outside.
    pub fn extract(
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        defined_formatter: Option<&str>,
    ) -> Result<FormattedInfo, Vec<PluginDiagnostic>> {
        let arguments = syntax.arguments(db);
        if !matches!(
            arguments,
            ast::WrappedArgList::ParenthesizedArgList(_) | ast::WrappedArgList::BracedArgList(_)
        ) {
            return Err(unsupported_bracket_diagnostic(db, syntax).diagnostics);
        }
        // `unwrap` is ok because the above `matches` condition ensures it's not None (unless the
        // pattern contains the `Missing` variant).
        let macro_arg_list = arguments.arg_list(db).unwrap();
        let macro_arg_list_elements = macro_arg_list.elements(db);
        let (formatter, without_formatter) = if let Some(defined_formatter) = defined_formatter {
            (RewriteNode::text(defined_formatter), macro_arg_list_elements.as_slice())
        } else {
            let Some((formatter, without_formatter)) = macro_arg_list_elements.split_first() else {
                return Err(vec![PluginDiagnostic {
                    stable_ptr: macro_arg_list.stable_ptr().untyped(),
                    message: "Macro must have at least one argument.".to_string(),
                }]);
            };
            (RewriteNode::new_trimmed(formatter.as_syntax_node()), without_formatter)
        };
        let Some((format_string_arg, args)) = without_formatter.split_first() else {
            return Err(vec![PluginDiagnostic {
                stable_ptr: macro_arg_list.stable_ptr().untyped(),
                message: "Macro must have at least one argument.".to_string(),
            }]);
        };
        let mut diagnostics = vec![];
        let format_string = try_extract_unnamed_arg(db, format_string_arg)
            .and_then(|arg| try_extract_matches!(arg, ast::Expr::String))
            .and_then(|arg| arg.string_value(db))
            .on_none(|| {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: format_string_arg.stable_ptr().untyped(),
                    message: "Argument must be a string literal.".to_string(),
                })
            });
        let args: Vec<_> = args
            .iter()
            .filter_map(|arg| {
                try_extract_unnamed_arg(db, arg).on_none(|| {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: arg.stable_ptr().untyped(),
                        message: "Expected unnamed argument.".to_string(),
                    })
                })
            })
            .collect();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(FormattedInfo {
            formatter,
            format_string_arg: format_string_arg.clone(),
            format_string: format_string.unwrap(),
            args,
        })
    }

    /// Adds the formatted string from macro to the formatter.
    pub fn add_to_formatter(
        &self,
        builder: &mut PatchBuilder<'_>,
        with_newline: bool,
    ) -> Result<(), PluginDiagnostic> {
        let mut arg_iter = self.args.iter().enumerate();
        let mut arg_used = vec![false; self.args.len()];
        let mut format_iter = self.format_string.chars().peekable();
        let mut pending_chars = String::new();
        while let Some(c) = format_iter.next() {
            if c == '{' {
                if format_iter.peek() == Some(&'{') {
                    pending_chars.push('{');
                    format_iter.next();
                    continue;
                }
                match format_iter.peek() {
                    None => {
                        return Err(PluginDiagnostic {
                            stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                            message: "Opening `{` without a matching `}`.".to_string(),
                        });
                    }
                    Some(&'}') => {
                        if let Some((position, arg)) = arg_iter.next() {
                            arg_used[position] = true;
                            self.append_formatted_arg(
                                builder,
                                &mut pending_chars,
                                RewriteNode::new_trimmed(arg.as_syntax_node()),
                            );
                            format_iter.next();
                            continue;
                        } else {
                            return Err(PluginDiagnostic {
                                stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                                message: "More format specifiers than arguments.".to_string(),
                            });
                        }
                    }
                    _ => {}
                }
                let Some(argument) = extract_argument(&mut format_iter) else {
                    return Err(PluginDiagnostic {
                        stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                        message: "Invalid format string: expected `'}'.".to_string(),
                    });
                };
                if let Ok(positional) = argument.parse::<usize>() {
                    let Some(arg) = self.args.get(positional) else {
                        return Err(PluginDiagnostic {
                            stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                            message: format!(
                                "Invalid reference to positional argument {positional} (there are \
                                 {} arguments).",
                                self.args.len()
                            ),
                        });
                    };
                    arg_used[positional] = true;
                    self.append_formatted_arg(
                        builder,
                        &mut pending_chars,
                        RewriteNode::new_trimmed(arg.as_syntax_node()),
                    );
                } else {
                    self.append_formatted_arg(
                        builder,
                        &mut pending_chars,
                        RewriteNode::text(&argument),
                    );
                }
            } else if c == '}' {
                if format_iter.peek() == Some(&'}') {
                    pending_chars.push('}');
                    format_iter.next();
                    continue;
                }
                Err(PluginDiagnostic {
                    stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                    message: "Closing `}` without a matching `{`.".to_string(),
                })?;
            } else {
                pending_chars.push(c);
            }
        }
        if with_newline {
            pending_chars.push('\n');
        }
        self.flush_pending_chars(builder, &mut pending_chars);
        for (position, used) in arg_used.into_iter().enumerate() {
            if !used {
                Err(PluginDiagnostic {
                    stable_ptr: self.args[position].as_syntax_node().stable_ptr(),
                    message: "Unused argument.".to_string(),
                })?;
            }
        }
        Ok(())
    }

    /// Appends a formatted argument to the formatter, flushing the pending bytes if necessary.
    fn append_formatted_arg(
        &self,
        builder: &mut PatchBuilder<'_>,
        pending_chars: &mut String,
        arg: RewriteNode,
    ) {
        self.flush_pending_chars(builder, pending_chars);
        builder.add_modified(RewriteNode::interpolate_patched(
            "core::result::ResultTrait::unwrap(core::fmt::Display::fmt(@$arg$, ref \
             $formatter$));\n",
            &[("arg".to_string(), arg), ("formatter".to_string(), self.formatter.clone())].into(),
        ));
    }

    /// Flushes the pending bytes to the formatter.
    fn flush_pending_chars(&self, builder: &mut PatchBuilder<'_>, pending_chars: &mut String) {
        for chunk in pending_chars.as_bytes().chunks(31) {
            builder.add_modified(RewriteNode::interpolate_patched(
                &format!(
                    "core::byte_array::ByteArrayTrait::append_word(ref $formatter$.buffer, {:#x}, \
                     {});\n",
                    BigInt::from_bytes_be(Sign::Plus, chunk),
                    chunk.len(),
                ),
                &[("formatter".to_string(), self.formatter.clone())].into(),
            ));
        }
        pending_chars.clear();
    }
}

/// Extracts an argument from a format string.
fn extract_argument(format_iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Option<String> {
    let mut argument = String::new();
    for c in format_iter.by_ref() {
        if c == '}' {
            return Some(argument);
        }
        if c.is_ascii_alphanumeric() || c == '_' {
            argument.push(c);
        } else {
            break;
        }
    }
    None
}
