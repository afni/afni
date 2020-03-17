from setuptools import setup
from pathlib import Path
SCRIPTS = [str(f) for f in Path('scripts').glob('*.py')]

setup(name='afnipy',
      version='0.0.1',
      description='AFNI python packages',
      url='git+https://github.com/leej3/template_making.git',
      author='AFNI team',
      author_email='afni.bootcamp@gmail.com',
      license='Public Domain',
      packages=['afnipy'],
      install_requires=["numpy", "matplotlib"],
      scripts=SCRIPTS,
      zip_safe=False)
