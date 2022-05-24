# Crust documentation style conventions

## Markdown

1. Use ATX-style heading with sentence case

   ```diff
   - Heading of a Section
   - ====================
   + # Heading of a section
   ```

2. Use one line per sentence to make diffs nicer.
   Wrap lines at 100 characters margin.
3. Use reference links, with shortcuts if appropriate.
   Place the sorted link reference definitions at the bottom of the file, or at the bottom of a
   section if there is an unusually large number of links that are specific to the section.

   ```markdown
   Example of shortcut link: [enumerations]
   Example of reference link with label: [block expression][block]
   [block]: expressions/block-expr.md
   [enumerations]: types/enum.md
   ```

4. Links should be relative with the `.md` extension.
5. Use `-` for unordered lists and `1. 2. 3.` for ordered ones.
6. Code examples should use code blocks with triple backticks.
   The language should always be specified (such as `crust`).
   See https://highlightjs.org/ for a list of supported languages.
7. Formatting to avoid:
    - Avoid trailing spaces.
    - Avoid double blank lines.

## Language and grammar

- Use American English spelling.
- Use Oxford commas.
- Tell that something _is_ instead of something _must be_ if it is not obvious.
  ```diff
  - Paths must be UTF-8 encoded.
  + Paths are UTF-8 encoded.
  ```
- Idioms and styling to avoid:
    - Avoid slashes for alternatives ("program/binary"), use conjunctions or rewrite it ("program or
      binary").
    - Avoid qualifying something as "in Crust", the entire reference is about Crust.
