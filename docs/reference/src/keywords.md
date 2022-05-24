# Keywords

There are three keyword categories:

- [strict](#strict-keywords)
- [reserved](#reserved-keywords)
- [contextual](#contextual-keywords)

## Strict keywords

These keywords can only be used in their correct contexts.
They cannot be used as names of any items.

`break`
`const`
`continue`
`else`
`enum`
`fn`
`for`
`hint`
`if`
`impl`
`in`
`match`
`pub`
`return`
`Self`
`self`
`struct`
`trait`
`true`
`type`
`use`

## Reserved keywords

These keywords aren't used yet, but they are reserved for future use.
They have the same restrictions as strict keywords.
The reasoning behind this is to make current programs forward compatible with future versions of
Crust by forbidding them to use these keywords.

`as`
`assert`
`do`
`dyn`
`extern`
`let`
`macro`
`mod`
`move`
`ref`
`static_assert`
`static`
`super`
`try`
`typeof`
`unsafe`
`where`
`while`
`with`
`yield`

## Contextual keywords

Some grammar productions may make use of new keywords not listed here.
Such keywords have special meaning only in these certain contexts.
Outside these places, these character sequences are treated as regular [identifiers], thus it is
possible to declare a function or variable with such names.

> **Note:** No contextual keywords are in use as for now.

[identifiers]: identifiers.md
