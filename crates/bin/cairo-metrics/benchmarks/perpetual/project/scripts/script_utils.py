import os
import re
import subprocess
import sys
from enum import Enum
from typing import List
from pathlib import Path


PARENT_BRANCH = "dev"
BASH_COLORS = {
    "red": "31",
    "green": "32",
    "yellow": "33",
    "blue": "34",
    "white": "97",
    "cyan": "96",
}

exclude_src_folders = [
    "repos/starkware-public/build",
    "build",
    "bazel-out",
    "src/services/starkex/docs",
    ".*/node_modules",
    r".*\.tox",
    "src/third_party",
    ".*/third_party",
]


def color_txt(color, txt: str, bold: bool = True):
    bold_str = "1;" if bold else ""
    color_code = BASH_COLORS[color.lower()]
    lines = txt.splitlines()
    return "\n".join(f"\033[{bold_str}{color_code}m{line}\033[0m" for line in lines)


class Color(Enum):
    BLACK = 30
    RED = 31
    GREEN = 32
    YELLOW = 33
    BLUE = 34
    MAGENTA = 35
    CYAN = 36
    LIGHT_GRAY = 37
    DEFAULT = 39
    DARK_GRAY = 90
    LIGHT_RED = 91
    LIGHT_GREEN = 92
    LIGHT_YELLOW = 93
    LIGHT_BLUE = 94
    LIGHT_MAGENTA = 95
    LIGHT_CYAN = 96
    WHITE = 97


def color_print(
    message,
    color="",
    bg="",
    is_bold=False,
    file=sys.stdout,
    additional="",
    use_colors=True,
):
    if not use_colors:
        print(message)
        return

    if color:
        color = color.value
    if bg:
        bg = str(bg.value + 10) + ";"
    bold = ""
    if is_bold:
        bold = "1;"
    if additional:
        additional = str(additional) + ";"
    print(f"\033[{bold}{additional}{bg}{color}m{message}\033[m", file=file)


def get_parent_branch():
    return open(os.path.join(os.path.dirname(__file__), "parent_branch.txt")).read().strip()


def create_grep_pipe_command(extensions):
    if extensions is None:
        return ""
    return r' | { grep -E "\.(%s)$" || true; }' % ("|".join(extensions))


def git_files(extensions=None) -> List[str]:
    return get_files("git ls-tree -r --name-only HEAD", extensions)


def changed_files(extensions=None, with_excluded_files=False) -> List[str]:
    return get_files(
        f"git diff --name-only $(git merge-base origin/{get_parent_branch()} HEAD)",
        extensions=extensions,
        with_excluded_files=with_excluded_files,
    )


def get_files(git_cmd: str, extensions, with_excluded_files=False, cwd=None) -> List[str]:
    if cwd is None:
        cwd = os.getcwd()
    grep_cmd = create_grep_pipe_command(extensions)
    command = f"{git_cmd} {grep_cmd}"  # All git files (we'll filter later)
    files: List[str] = (
        subprocess.check_output(command, shell=True, cwd=cwd).decode("utf-8").splitlines()
    )

    # Filter out exclusion list.
    if not with_excluded_files:
        exclude_pattern = f'^({"|".join(exclude_src_folders)})'
        files = [f for f in files if not re.match(exclude_pattern, f)]

    # Filter to include only real files (exclude git output artifacts).
    files = [f for f in files if os.path.exists(os.path.join(cwd, f))]
    return files


def find_command(command_name: str) -> str:
    """
    Finds the path of a command by running `which`. Prints an error message upon failure.
    """
    try:
        return subprocess.check_output(["which", command_name]).decode("utf-8").splitlines()[0]
    except subprocess.CalledProcessError:
        print(color_txt("red", f"Failed to launch {command_name}"))
        raise Exception(f"{command_name} not installed")


def get_project_root() -> Path:
    """
    Returns the root directory of the project.
    Assumes this file is located at <project_root>/scripts/scripts_utils.py
    """
    return Path(__file__).resolve().parent.parent
