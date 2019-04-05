import subprocess
from pathlib import Path


def test_3dAllineate():
    wd = Path(__file__).parent.as_posix()
    print(wd)
    cp = subprocess.run(
        f"cd {wd}/test_dirs/3dAllineate && tcsh runit",
        check=True,
        close_fds=True,
        shell=True,
    )
