"""
Testing for git hook functionality
"""
from pathlib import Path
import os
import subprocess as sp
import tempfile

from pre_commit_darkening import darken_staged_files


SAMPLE_TEXT = """if True: print('hi')\nprint()\nif False:\n    print('there')\n"""


def make_sample_repo(sample_text):
    """
    Init a repo with sample_repo.py containing 'sample_text'
    """
    sp.check_output("git init".split())
    text_file = Path("sample_repo.py")
    text_file.write_text(sample_text)
    sp.check_output(f"git add {text_file}".split())
    sp.check_output("git commit -m demo".split())
    return text_file


def test_darken_staged_files_works():
    """
    A basic check that staged files are indeed darkened by darker.
    """
    # set up a repository to test
    os.chdir(tempfile.mkdtemp())
    text_file = make_sample_repo(SAMPLE_TEXT)

    # stage a change that is not compliant with black
    text_file.write_text(SAMPLE_TEXT.replace("print('hi')", "print('bye')"))
    sp.check_output(f"git add {text_file}".split())
    # execute the hook functionality with current python's module namespace and PATH
    darken_staged_files()

    # commit changes (should be darkened)
    sp.check_output("git commit -m demo_partial_rewrite".split())

    # Check that darkening occurred correctly: the modified line was changed
    # (and only that line)
    changes = sp.check_output("git diff HEAD~1 --unified=0".split()).decode()
    assert """print("bye")""" in changes
    assert "print('there')" not in changes


def test_darken_staged_files_skips_files_with_unstaged_changes():
    """
    If patches can unambiguously be matched pre/post darkening this
    functionality could be dropped in favor of the more desirable behavior of
    always succeeding to darken the commit.
    Until then we will confirm the expected behavior...
    """

    # set up a repository to test
    os.chdir(tempfile.mkdtemp())
    text_file = make_sample_repo(SAMPLE_TEXT)

    # stage a change that is not compliant with black
    text_file.write_text(SAMPLE_TEXT.replace("print('hi')", "print('bye')"))
    sp.check_output(f"git add {text_file}".split())
    text_file.write_text(text_file.read_text() + "\nprint('An unblackened line')\n")

    # execute the hook functionality with current python's module namespace and PATH
    darken_staged_files()

    # commit changes (will not be darkened but warning should be emitted)
    sp.check_output("git commit -m demo_partial_rewrite".split())

    # Until support darkening should not have happened... test this is the case
    changes = sp.check_output("git diff HEAD~1 --unified=0".split()).decode()
    assert """print("bye")""" not in changes
