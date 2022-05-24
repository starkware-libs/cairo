#!/usr/bin/env python3

"""
This script is an mdBook preprocessor which does these things:

1. It collects all ```bnf code fences and combines them into full grammar, which is then included
   in the book via {{#bnf_index}} block.

2. It collects URLs to pages where all grammar non_terminals are defined, then tailors this index
   for each chapter, so that the index will not include non_terminals defined in processed chapter,
   and injects BNF_INDEX JavaScript variable at the beginning of chapter content. For the
   "Full Grammar" chapter we include full index so users will be able to navigate to description
   of non-terminals.

   The BNF syntax highlighter will later pick this index and link externally defined non_terminals
   for easier navigation.

   This approach may seem a bit complicated, but mdBook preprocessor's API only allows modifying
   chapter's content (from what interests us).
"""

import json
import re
import sys
from dataclasses import dataclass
from typing import List, Iterable

RE_BNF_INDEX_BLOCK = re.compile(r"\{\{#bnf_index}}")
RE_BNF_CODE_FENCE = re.compile(
    r"""
    ```bnf
    (.*?)
    ```
    """,
    re.MULTILINE | re.DOTALL | re.VERBOSE,
)
RE_BNF_non_terminal = re.compile(r"^([A-Z\d_]+)\s+:", re.MULTILINE)


def dbg(*args, **kwargs):
    """
    Use this while debugging instead of naked ``print``.
    """
    print(*args, **kwargs, file=sys.stderr)


@dataclass
class BnfChunk:
    chapter_name: str
    chapter_path: str
    bnf: str


@dataclass
class BnfNonTerminalDefinition:
    name: str
    chapter_path: str


def all_chapters_with_content(book: dict) -> Iterable[dict]:
    def iter_sections(sections):
        for section in sections:
            if "Chapter" in section:
                chapter = section["Chapter"]

                if chapter["content"]:
                    yield chapter

                if sub_items := chapter["sub_items"]:
                    yield from iter_sections(sub_items)

    return iter_sections(book["sections"])


def collect_bnf_chunks(book: dict) -> List[BnfChunk]:
    chunks = []

    for chapter in all_chapters_with_content(book):
        chapter_name = chapter["name"]
        chapter_path = chapter["path"]
        content = chapter["content"]

        for match in re.finditer(RE_BNF_CODE_FENCE, content):
            chunk = BnfChunk(
                chapter_name=chapter_name,
                chapter_path=chapter_path,
                bnf=match.group(1),
            )
            chunks.append(chunk)

    return chunks


def collect_non_terminal_definitions_from_chunk(
    chunk: BnfChunk,
) -> Iterable[BnfNonTerminalDefinition]:
    for match in re.finditer(RE_BNF_non_terminal, chunk.bnf):
        yield BnfNonTerminalDefinition(
            name=match.group(1),
            chapter_path=chunk.chapter_path,
        )


def build_non_terminal_index(chunks: Iterable[BnfChunk]):
    index = {}
    chapters_with_bnf = set()
    for chunk in chunks:
        for definition in collect_non_terminal_definitions_from_chunk(chunk):
            if definition.name not in index:
                index[definition.name] = definition.chapter_path
                chapters_with_bnf.add(definition.chapter_path)
            else:
                raise KeyError(f"{definition.name} has been defined multiple times.")
    return index, chapters_with_bnf


def inject_full_grammar(book, chunks):
    source = "\n".join((chunk.bnf for chunk in chunks))
    index_markdown = f"```bnf{source}```"

    for chapter in all_chapters_with_content(book):
        chapter["content"] = re.sub(
            RE_BNF_INDEX_BLOCK, lambda _: index_markdown, chapter["content"]
        )


def convert_chapter_path_to_url(chapter_path: str) -> str:
    return re.sub(r"\.md$", ".html", chapter_path)


def inject_bnf_index(book, index, chapters_with_bnf):
    for chapter in all_chapters_with_content(book):
        this_chapter_path = chapter["path"]
        if this_chapter_path == "bnf.md":
            tailored_index = index
        elif this_chapter_path in chapters_with_bnf:
            tailored_index = {
                non_terminal: chapter_path
                for non_terminal, chapter_path in index.items()
                if chapter_path != this_chapter_path
            }
        else:
            tailored_index = {}

        tailored_index = {
            non_terminal: convert_chapter_path_to_url(chapter_path)
            for non_terminal, chapter_path in tailored_index.items()
        }

        script = f"""<script>window.BNF_INDEX={json.dumps(tailored_index)};</script>"""

        chapter["content"] = script + "\n" + chapter["content"]


def main():
    context, book = json.load(sys.stdin)
    chunks = collect_bnf_chunks(book)
    index, chapters_with_bnf = build_non_terminal_index(chunks)
    inject_full_grammar(book, chunks)
    inject_bnf_index(book, index, chapters_with_bnf)
    json.dump(book, sys.stdout)


def supports_command():
    # This preprocessor is renderer agnostic.
    sys.exit(0)


if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == "supports":
            supports_command()
        else:
            raise ValueError(f"Unsupported command {sys.argv}")
    else:
        main()
