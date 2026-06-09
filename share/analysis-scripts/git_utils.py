##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

import subprocess


def rev_parse(gitdir, rev):
    res = subprocess.run(
        ["git", "rev-parse", rev],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        encoding="ascii",
        cwd=gitdir,
    )
    name = res.stdout.strip()
    return name if name else None


def name_rev(gitdir, rev):
    res = subprocess.run(
        ["git", "name-rev", "--name-only", rev],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        encoding="ascii",
        cwd=gitdir,
    )
    name = res.stdout.strip()
    return name if name else None


def current_rev(gitdir):
    return name_rev(gitdir, "HEAD")


def is_clean(gitdir):
    # git diff and diff-index are not working on some of our case studies to
    # decide whether the workingin dir is clean or not ; git status is more
    # reliable
    res = subprocess.run(
        ["git", "status", "--untracked-files=no", "--porcelain"],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        encoding="ascii",
        cwd=gitdir,
    )
    return res.returncode == 0 and not res.stdout
