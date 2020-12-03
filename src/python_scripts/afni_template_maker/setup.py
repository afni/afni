from setuptools import setup, find_packages

setup(name='afni-template-maker',
      version='0.0.1',
      description='Make a brain template. This "average" brain can be used for subsequent group analysis',
      url='git+https://github.com/afni/afni.git@master#egg=afni_template_maker&subdirectory=src/python_scripts/afni_template_maker',
      author='John Lee and Daniel Glen',
      author_email='johnleenimh@gmail.com',
      license='Public Domain',
      packages= find_packages(),
      scripts=["make_template_dask.py"],
      zip_safe=False)
