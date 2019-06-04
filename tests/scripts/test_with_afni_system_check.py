import pytest
from .utils.tools import run_cmd


@pytest.mark.slow
def test_with_afni_system_check(data):
    cmd = "afni_system_check.py -check_all"
    run_cmd(data, cmd)
