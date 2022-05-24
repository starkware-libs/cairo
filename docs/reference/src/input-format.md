# Input format

## Encoding

Crust input is interpreted as a sequence of Unicode code points encoded in UTF-8,
though most grammar rules are defined in terms of printable ASCII characters.
Other encodings should not be converted to UTF-8 before processing.

The NUL character (`U+0000`) is not allowed anywhere in input text.

The optional [_UTF8 byte order mark_] (`U+FEFF`) indicates that the file is encoded in UTF8.
It can only occur at the beginning of the file and is ignored by the compiler.

### File paths

When reading Crust input from filesystem, absolute paths of processed files are valid UTF-8.
Running Crust tools from paths containing byte sequences invalid in UTF-8 is not guaranteed to work.

## Shebang

[Unix Shebang] syntax is not allowed and will generate syntax error.

[_utf8 byte order mark_]: https://en.wikipedia.org/wiki/Byte_order_mark#UTF-8

[unix shebang]: https://en.wikipedia.org/wiki/Shebang_(Unix)
