#!/usr/bin/env python3
"""
Module providing pre-commit modification of staged files to comply with
code style as dictated by black.
"""
import logging
import shutil
import subprocess as sp
import sys
from pathlib import Path

LOG_FORMAT = "%(name)s - %(levelname)s - %(message)s"
logging.basicConfig(format=LOG_FORMAT, level="INFO")
logger = logging.getLogger("Code_style_check")
DIRS_BLACKENED = ["tests"]

# Changing this needs to be considered in the context of CI testing
BLACK_VERSION = "20.8b1"


def darken_staged_files():
    """
    This function is intended as a  pre-commit operation to 'darken' the commit:
    makes the diffs compliant with the black style. Files that have unstaged
    changes are ignored.
    """
    if not shutil.which("darker"):
        logger.warning(
            "Cannot find darker. darker is used to automatically fix "
            "the style of the committed code (in python files). You may fail CI testing "
            "if your changes do not conform to the style used by "
            f"black. Consider installing it using 'pip install darker black=={BLACK_VERSION}'"
        )
        sys.exit(1)

    # Get the current staged changes, these are what should be what end up being
    # staged if all else fails
    files_staged = (
        sp.check_output("git diff --cached --name-only".split()).decode().splitlines()
    )
    files_unstaged = (
        sp.check_output("git diff  --name-only".split()).decode().splitlines()
    )
    # See if we can cleanly make the diffs 'blackened' (this would entail
    # darkening the files and staging them). This can either be done by
    # finding corresponding patches or by A wrinkle is for stage files that
    # have unstaged changes. Easy solution is to  fail for any such files.
    files_for_darkening = []
    for fpath in files_staged:
        subdir = list(Path(fpath).parents)[-2].absolute().name
        if not any(is_relative_to(base, subdir) for base in DIRS_BLACKENED):
            logger.debug(f"not considering {fpath} because {subdir}")
            continue
        logger.debug(f"considering fpath: {fpath}")
        if fpath in files_unstaged:
            if sp.check_output(f"darker --diff {fpath}".split()).decode():
                logger.warning(
                    f"{fpath }has unstaged changes and so will not "
                    "be considered when fixing for automatic changes. "
                    "It appears to need some changes. This may cause CI to fail."
                )
            continue

        files_for_darkening.append(fpath)

    # Run darker on appropriate files and stage the changes
    if files_for_darkening:
        logger.debug(f"files_for_darkening:{files_for_darkening}")
        darker_files = " ".join(files_for_darkening)
        cmd = f"darker {darker_files}".split()
        logger.debug(f"cmd: {cmd}")
        # make files darker
        sp.check_output(cmd)
        if sp.check_output(f"git diff --name-only {darker_files}".split()):
            # Tell the user if any files were changed (could include non python files)
            logger.info("Some automatic reformatting occurred during this commit!")
            sp.check_output(f"git add {darker_files}".split())


def is_relative_to(basedir, subdir):
    try:
        # If this raises an error then the subdir is outside the basedir
        Path(basedir).relative_to(subdir)
        return True
    except ValueError:
        # not in basedir
        return False


if __name__ == "__main__":
    darken_staged_files()
