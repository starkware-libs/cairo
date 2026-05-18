#!/usr/bin/env python3
"""Wrapper: runs the real sandbox.py with this directory as the project root."""
import os
import sys

os.chdir(os.path.dirname(os.path.abspath(__file__)))
sys.argv[0] = os.path.join(os.path.dirname(os.path.abspath(__file__)), "sandbox.py")

exec(open("/home/ori/rust/claude-sandbox/sandbox.py").read())
