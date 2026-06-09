# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

"""This file provides utility functions to use external tools, either
available in the PATH or via PyInstaller."""

import os
from pathlib import Path
import shutil
import subprocess
import sys

# warnings about missing commands are disabled during testing
emit_warns = os.getenv("PTESTS_TESTING") is None

# Cache for get_command
cached_commands: dict[str, Path | None] = {}


def resource_path(relative_path: str) -> str:
    """Get absolute path to resource; only used by the pyinstaller standalone distribution"""
    base_path = getattr(sys, "_MEIPASS", os.path.dirname(os.path.abspath(__file__)))
    return os.path.join(base_path, relative_path)


def get_command(command: str, env_var_name: str) -> Path | None:
    """Returns a Path to the command; priority goes to the environment variable,
    then in the PATH, then in the resource directory (for a pyinstaller binary)."""
    if command in cached_commands:
        return cached_commands[command]
    p_str = os.getenv(env_var_name) or shutil.which(command)
    if p_str:
        p = Path(p_str)
    else:
        p = Path(resource_path(command))
        if not p.exists():
            if emit_warns:
                print(
                    f"info: optional external command '{command}' not found in PATH;",
                    f"consider installing it or setting environment variable {env_var_name}",
                )
            p = None
    cached_commands[command] = p
    return p


def run_and_check(command_and_args, input_data: str) -> str:
    try:
        return subprocess.check_output(
            command_and_args,
            input=input_data,
            stderr=None,
            encoding="ascii",
            errors="ignore",
        )
    except subprocess.CalledProcessError as e:
        sys.exit(f"error running command: {command_and_args}\n{e}")
