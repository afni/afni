from setuptools import setup, find_packages
import sys
from distutils.version import LooseVersion
py_ver = '.'.join(str(n) for n in sys.version_info[:3])
if LooseVersion(py_ver) < LooseVersion("3.6"):
    err = "The current interpretter {} ({}) is not supported.".format(sys.executable,py_ver)
    raise EnvironmentError(err)


from pathlib import Path
SCRIPTS = [str(f) for f in Path('scripts').glob('*.py')]

setup(name='afnipy',
      version='0.0.1',
      description='AFNI python code installed as a package. Much of the functionality requires a working installation of AFNI.',
      url='git+https://github.com/afni/afni.git@master#egg=afnipy&subdirectory=src/python_scripts',
      author='AFNI team',
      author_email='afni.bootcamp@gmail.com',
      license='Public Domain',
      packages= find_packages(),
      install_requires=["numpy>=1.14.5", "matplotlib"],
      scripts=SCRIPTS,
      zip_safe=False)
