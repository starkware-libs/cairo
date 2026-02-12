#!/usr/bin/env python3
"""Check that the final line of each doc comment block ends with punctuation.

Scans .rs files under crates/ for doc comment blocks (/// or //!) and
reports any block whose last non-blank comment line does NOT end with punctuation.

Exclusions (non-prose that doesn't need punctuation):
- Lines inside code fences (``` blocks)
- Empty comment lines
- Lines that look like code examples
- Markdown headers (# ...)
- Lines ending with `:` (introducing a code block or list)
- Bullet/list items (* ..., - ...)
- Math expressions / ranges
"""

import argparse
import os
import re
import sys
from pathlib import Path
from typing import List


def is_doc_comment(line: str) -> bool:
    stripped = line.lstrip()
    return stripped.startswith("///") or stripped.startswith("//!")


def extract_comment_text(line: str) -> str:
    """Extract the text content after /// or //! and whether it was indented."""
    stripped = line.lstrip()
    if stripped.startswith("///") or stripped.startswith("//!"):
        return stripped[3:]
    return ""


def should_skip_line(text: str) -> bool:
    """Return True if this final comment line should be excluded from the check."""
    skip = (
        # Code fences
        text.strip().startswith("```")
        # Indented
        or text.startswith("  ")
        or text.startswith("\t")
        # Markdown headers
        or re.match(r"^\s*#+\s", text) is not None
        # Bullet/list items
        or re.match(r"^\s*[\*\-]\s", text) is not None
        # Lines ending with `:` (introducing examples/code blocks)
        or text.rstrip().endswith(":")
        # Lines that are entirely a backtick-wrapped code reference (e.g. `SomeType<T>`)
        or re.match(r"^`[^`]+`$", text.strip()) is not None
        # Math expressions: lines that are just numbers/operators
        or re.match(r"^[\d\s\+\-\*\/\(\)\[\]<>,]+$", text) is not None
        # Comments starting with `----`
        or re.match(r"^\s*----", text) is not None
    )

    # Code indicators
    code_indicators = [
        ";",
        "{",
        "}",
    ]
    for indicator in code_indicators:
        if indicator in text:
            skip = True

    return skip


def check_file(lines: List[str]) -> list[tuple[int, str]]:
    """Check text for doc comment blocks whose final line doesn't end with punctuation.

    Returns list of (line_number, final_comment_text).
    """
    violations = []

    i = 0

    while i < len(lines):
        if not is_doc_comment(lines[i]):
            i += 1
            continue

        # Collect all consecutive doc comment lines.
        block_lines = []  # (line_number, text)

        # Track code fences and find the last non-blank, non-code text line.
        in_code_fence = False
        last_text_line = None  # (line_number, text)
        while i < len(lines) and is_doc_comment(lines[i]):
            text = extract_comment_text(lines[i])
            i += 1
            block_lines.append((i, text))  # 1-indexed line number
            if text.strip().startswith("```"):
                in_code_fence = not in_code_fence
            if in_code_fence or not text.strip() or text.strip().startswith("```"):
                continue
            last_text_line = (i, text)

        if last_text_line is None:
            continue

        lineno, text = last_text_line

        # Skip indented lines (example output)
        if should_skip_line(text):
            continue

        # Single-line doc blocks that start lowercase are labels, not prose.
        if len(block_lines) == 1 and text.strip() and text.strip()[0].islower():
            continue

        # Check if it ends with proper punctuation (period, question mark, or exclamation)
        stripped_text = text.strip()
        if not stripped_text.endswith((".", "?", "!")):
            violations.append((lineno, text))

    return violations


def main():
    parser = argparse.ArgumentParser(
        description="Check that the final line of each doc comment block ends with punctuation."
    )
    parser.add_argument(
        "root",
        type=Path,
        help="Project root directory.",
    )
    args = parser.parse_args()

    project_root = args.root.resolve()
    if not project_root.exists():
        print(f"Error: {project_root} does not exist", file=sys.stderr)
        sys.exit(1)

    all_violations = []  # (line_number, text, filepath)
    file_count = 0

    for root, dirs, files in os.walk(project_root):
        for fname in sorted(files):
            if not fname.endswith(".rs"):
                continue
            filepath = os.path.join(root, fname)
            with open(filepath) as f:
                text = f.readlines()
                file_count += 1
                for lineno, comment_text in check_file(text):
                    all_violations.append((lineno, comment_text, filepath))

    # Group by the ending character for analysis.
    endings = {}
    for lineno, text, filepath in all_violations:
        last_char = text.rstrip()[-1] if text.rstrip() else "(empty)"
        endings.setdefault(last_char, []).append((lineno, text, filepath))

    print(f"Scanned {file_count} .rs files under crates/")
    print(f"Found {len(all_violations)} doc comment blocks NOT ending with period")
    print()

    # Print summary by ending character.
    print("=== Summary by ending character ===")
    for char, items in sorted(endings.items(), key=lambda x: -len(x[1])):
        print(f"  '{char}': {len(items)} occurrences")
    print()

    # Print all violations.
    print("=== All violations ===")
    for lineno, text, filepath in sorted(all_violations, key=lambda x: x[2]):
        rel_path = os.path.relpath(filepath, project_root)
        print(f"{rel_path}:{lineno}: {text}")

    if all_violations:
        sys.exit(1)


if __name__ == "__main__":
    main()
