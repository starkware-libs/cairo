# Identifiers

```bnf
IDENTIFIER_OR_KEYWORD : XID_Start XID_Continue*
                      | "_" XID_Continue+

IDENTIFIER : IDENTIFIER_OR_KEYWORD /* except strict or reserved keywords */
```

<!-- When updating the version, update the UAX links, too. -->
Identifiers follow the specification in [Unicode Standard Annex #31][UAX31] for Unicode version
13.0, with the additions described below.
Some examples of identifiers:

- `foo`
- `_identifier`
- `Kraków`
- `Київ`
- `東京`

The profile used from UAX #31 is:

- `Start := `[`XID_Start`][xid-start], plus the underscore character (`U+005F`)
- `Continue := `[`XID_Continue`][xid-continue]
- `Medial := empty`

with the additional constraint that a single underscore character is not an identifier.

> **Note**: Identifiers starting with an underscore are typically used to indicate an identifier
> that is intentionally unused, and will silence the unused warning.

Identifiers may not be a [strict] or [reserved] keyword.

Zero width non-joiner (ZWNJ `U+200C`) and zero width joiner (ZWJ `U+200D`) characters are not
allowed in identifiers.

Identifiers are restricted to the ASCII subset of [`XID_Start`][xid-start]
and [`XID_Continue`][xid-continue] in the following situations:

- Module names loaded from the filesystem.

## Normalization

Identifiers are normalized using Normalization Form C (NFC) as defined
in [Unicode Standard Annex #15][UAX15].
Two identifiers are equal if their NFC forms are equal.

## Case sensitivity

Crust is a _case-sensitive_ language which means that upper and lower case code points are distinct.

[xid-continue]: http://unicode.org/cldr/utility/list-unicodeset.jsp?a=%5B%3AXID_Continue%3A%5D&abb=on&g=&i=

[xid-start]:  http://unicode.org/cldr/utility/list-unicodeset.jsp?a=%5B%3AXID_Start%3A%5D&abb=on&g=&i=

[UAX15]: https://www.unicode.org/reports/tr15/tr15-50.html

[UAX31]: https://www.unicode.org/reports/tr31/tr31-33.html
