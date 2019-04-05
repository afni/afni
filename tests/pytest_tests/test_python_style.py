from pathlib import Path
import subprocess

PYTESTS_DIR = [p for p in Path(__file__).parents if p.name == "pytest_tests"][0]
TEST_MODS = [str(p) for p in PYTESTS_DIR.glob("**/*.py")]

STYLE_TXT = (
    "The coding style in some of the python files does not conform with "
    "the projects. Specifically we use black and pep8 (but with a max "
    "line length of 88 characters). Files can be corrected with the "
    "tools autopep8 and black. For more info see "
    "https://github.com/ambv/black and "
    "https://github.com/hhatto/autopep8 "
)


def test_for_unblackened():
    for test_module in TEST_MODS:
        cmd = f"black --check {test_module}"
        proc = subprocess.run(
            cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        if proc.returncode != 0:
            raise ValueError(STYLE_TXT)


def test_pep8():
    for test_module in TEST_MODS:

        cmd = (
            "autopep8 --exit-code --diff --list-fixes -vvvv --ignore "
            f"E501 {test_module} "
        )

        proc = subprocess.run(
            cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        if proc.returncode == 2:
            raise ValueError(STYLE_TXT)
        proc.check_returncode()


if __name__ == "__main__":
    test_for_unblackened()
    test_pep8()
