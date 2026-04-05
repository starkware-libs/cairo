---
name: prepush
description: Run pre-push checks and Orizi code review for Cairo repository
allowed-tools: Bash(./scripts/pre_push.sh:*), Bash(./scripts/rust_fmt.sh:*), Bash(./scripts/clippy.sh:*), Bash(./scripts/docs.sh:*), Bash(typos:*), Bash(taplo:*), Bash(cargo machete:*), Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(cargo fmt:*), Bash(cargo clippy:*), Read, Glob, Grep
---

## Context

- Current directory: !`pwd`
- Git status: !`git status --short`
- Last commit: !`git log --oneline -1`
- Changed files in last commit: !`git diff --name-only HEAD~1 HEAD 2>/dev/null || git diff --name-only HEAD`

## Your task

Perform the following pre-push checks:

1. **Run pre_push.sh**: Execute `./scripts/pre_push.sh` and ensure no errors exist. If there are errors, fix them by running the appropriate fix commands (e.g., `./scripts/rust_fmt.sh` for formatting, `cargo clippy --fix --allow-dirty` for clippy warnings, `typos -w` for typos).

2. **Analyze changes**: Look at the changes in the last git commit and current diff to identify any missing documentation on public functions or non-trivial inner functions.

3. **Review naming**: Check the naming of any new functions and classes for good practices (snake_case for functions, CamelCase for types, clear descriptive names).

4. **Orizi review**: Use `/orizi-review` on the changes in the last commit. Fix any issues that can be fixed automatically, and report the rest.

5. **Provide a minimal report**: Summarize any fixes made and provide suggestions for where fixes are still needed, including Orizi review findings.

Focus on being thorough but concise. Fix what you can automatically, and report what needs manual attention.
