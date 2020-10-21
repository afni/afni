# this can be installed using:
# pip install -e .
# or by something similar to the following (though this is not currently
# supported):
# python setup.py sdist
# pip install dist/afni-test-utils-0.0.1.tar.gz
from setuptools import setup, find_packages
import sys
from distutils.version import LooseVersion

py_ver = ".".join(str(n) for n in sys.version_info[:3])
if LooseVersion(py_ver) < LooseVersion("3.6"):
    err = "The current interpretter {} ({}) is not supported.".format(
        sys.executable, py_ver
    )
    raise EnvironmentError(err)


setup(
    name="afni-test-utils",
    version="0.0.1",
    description="Python package for running tests of the afni suite.",
    url="git+https://github.com/afni/afni.git@master#egg=afni_test_utils&subdirectory=tests",
    author="John Lee",
    author_email="johnleenimh+afni_dev@gmail.com",
    license="Public Domain",
    package_dir={"afni_test_utils": ""},
    scripts=["run_afni_tests.py"],
    packages=find_packages(),
    zip_safe=False,
)
