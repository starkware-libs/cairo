#!/usr/bin/env python3.10

import argparse
import sys
from script_utils import changed_files, color_txt, git_files


def main():
    parser = argparse.ArgumentParser(description="Check that all code lines are not too long.")
    parser.add_argument(
        "--max_line_length",
        "-l",
        type=int,
        default=100,
        help="The maximal amount of characters a line can have (default: 100)",
    )
    parser.add_argument("--files", nargs="+", help="Run on specified files. Ignore other flags.")
    parser.add_argument("--changes_only", action="store_true", help="Run only on changed files.")
    parser.add_argument("--quiet", "-q", dest="verbose", action="store_false")

    args = parser.parse_args()

    extensions = ["py"]
    if args.files:
        files = [path for path in args.files if path.endswith(tuple(extensions))]
    elif args.changes_only:
        files = changed_files(extensions)
    else:
        files = git_files(extensions)

    if args.verbose:
        print(color_txt("yellow", "=== checking the following files: ===\n" + "\n".join(files)))
        sys.stdout.flush()

    long_lines = []
    for f in files:
        with open(f, "r") as file:
            for line_num, line in enumerate(file, 1):
                line = line.rstrip("\n")
                if line.startswith("from") or line.startswith("import"):
                    continue
                if len(line) > args.max_line_length:
                    long_lines.append((f, line_num))

    if len(long_lines) > 0:
        print(color_txt("red", "The following lines are too long:"))
        for file_name, line_num in long_lines:
            print(f"{file_name}:{line_num}")
        sys.exit(1)

    if args.verbose:
        print(color_txt("green", "=== Line length check completed successfully ==="))


if __name__ == "__main__":
    main()
