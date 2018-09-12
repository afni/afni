import subprocess

def test_3dAllineate():
	subprocess.check_output('cd pytest_tests/test_dirs/3dAllineate && tcsh runit', shell=True)

def test_3dClustSim():
	subprocess.check_output('cd pytest_tests/test_dirs/3dClustSim && tcsh runit', shell=True)

