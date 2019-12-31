
import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="afniRTI",
    version="3.0.0",
    author="V. R-singh",
    author_email="for_usenet@yahoo.com",
    description="AFNI Real-Time Interface in Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/roopchansinghv/afni-real-time-interface-in-python",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.0',
)

