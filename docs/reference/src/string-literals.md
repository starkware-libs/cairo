# String literals

```bnf
STRING_LITERAL
    : STRING_MODIFIER* ( INLINE_STRING_LITERAL | MULTILINE_STRING_LITERAL )

INLINE_STRING_LITERAL : `"` CHARACTER* `"`

MULTILINE_STRING_LITERAL
    : `"""` ( CHARACTER | [" ] | EOL | MULTILINE_STRING_CONTINUE )* `"""`

CHARACTER : ~[" \ \r \n] | ESCAPE_SEQUENCE

ESCAPE_SEQUENCE : `\"`
                | "\n" | "\t"
                | "\0"
                | "\\"
                | "\x" [a-fA-F0-9_]{2}
                | "\u{" [a-fA-F0-9_]{1,6} "}"

MULTILINE_STRING_CONTINUE : "\" EOL

STRING_MODIFIER : [a-z]
```

A _string literal_ is a sequence of any Unicode characters enclosed within two double quotes (`"`)
or two triples of double quote (`"""`).

Line breaks are only allowed in multiline variant (enclosed with `"""`).
A line-break is either a newline (`U+000A`) or a pair of carriage return and newline (`U+000D`,
`U+000A`).
Both byte sequences are normally translated to `U+000A`, but as a special exception, when an
unescaped `U+005C` character (`\`) occurs immediately before the line-break, then the `U+005C`
character, the line-break, and all whitespace at the beginning of the next line are ignored.
Thus, both strings are equal:

```crust
"foobar"
"""\
foo\
    bar\
"""
```

## Modifiers

Interpretation of string literals values and output types can be changed by applying many
_modifiers_.
Following list outlines all supported modifiers:

- `b`\
  _Binary literal_: Processes string as an ASCII-only bytes sequence.\
  Conflicts with: `c`

- `c`\
  _Character literal_: Allows only one character in literal and returns `char`.\
  Conflicts with: `b`

- `r`\
  _Raw string_: Does not process any escape sequences.

- `t`\
  _Trims_ leading and trailing whitespace.

- `u`\
  _Unindent_: Removes any common leading whitespace from every line in string.

Crust accepts any order of the modifiers, but by convention these should be sorted alphabetically.

> **Note:** All `IDENTIFIER STRING_LITERAL IDENTIFIER` productions  (i.e. string surrounded on both
> sides by valid identifiers without spaces) are reserved for future use.

## Escape sequences

Some additional _escape sequences_ are available in either character or non-raw string literals.
An escape starts with a backtick (`\`) and continues with one of the following forms:

- A _7-bit code point escape_ starts with `x` and is followed by exactly two _hex digits_ with value
  up to `0x7F`.
  It denotes the ASCII character with value equal to the provided hex value.
  Higher values are not permitted because it is ambiguous whether they mean Unicode code points or
  byte values.
- A _24-bit code point escape_ starts with `u` and is followed by up to six _hex digits_ surrounded
  by braces.
  It denotes the Unicode code point equal to the provided hex value.
- A _whitespace escape_ is one of the characters `n`, `t`, denoting the Unicode values `U+000A` (LF)
  and `U+0009` (HT) respectively.
- The _null escape_ is the character `0` and denotes the Unicode value `U+0000` (NUL).
- The _backslash escape_ is the character `\` which must be escaped in order to denote itself.

## Encoding

TODO(mkaput, 16/05/2022) Encoding section: UTF-8 vs compressed UTF-8 vs UTF-32 1 char per 1 felt?
    I think UTF-32 will be best for memory usage.
