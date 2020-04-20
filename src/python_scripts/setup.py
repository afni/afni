from setuptools import setup
from pathlib import Path
SCRIPTS = [str(f) for f in Path('scripts').glob('*.py')]

setup(name='afnipy',
      version='0.0.1',
      description='AFNI python code installed as a package. Much of the functionality requires a working installation of AFNI.',
      url='git+https://github.com/afni/afni.git@master#egg=afnipy&subdirectory=src/python_scripts',
      author='AFNI team',
      author_email='afni.bootcamp@gmail.com',
      license='Public Domain',
      packages=['afnipy'],
      install_requires=["numpy", "matplotlib"],
      scripts=SCRIPTS,
      zip_safe=False)
