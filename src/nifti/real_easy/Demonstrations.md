# Demonstration Examples

This directory contains stand-alone demonstrations of common ways to
include `nifti_clib` integration with your tools.

These examples assume a modern ( > 3.0 ) cmake approach.

See https://pabloariasal.github.io/2018/02/19/its-time-to-do-cmake-right for details.

```bash
  git clone https://github.com/NIFTI-Imaging/nifti_clib.git
  mkdir -p build-nifti_clib
  cd build-nifti_clib
  cmake ../nifti_clib
  make -j 2
  ctest
  mkdir ../build-stand-alone
  cd ../build-stand-alone
  export STAND_ALONE_SOURCE_DIR=../nifti_clib/real_easy/stand_alone_app
  cmake -DNIFTI_DIR:PATH=../build-nifti_clib ${STAND_ALONE_SOURCE_DIR}
  make

```

## As an external separately built package

If `nifti_clib` (3.0.0 or greater) is installed as
part of your distribution (via homebrew/debian/rpm/tgz etc),
or as an independent build, then the most convenient way
to include nifti support is with the `find_package` support demonstrated
in the `stand_alone_app` directory.

## As in integrated part of the current project

In this example the `nifti_clib` source code is built as an
integral part of our package, and is very tightly integrated.
The `parent_project_demo` directory shows how to accomplish
the tight integration.

For the integrated example, it is recommended that the tight
integration use a prefix related to your parent project so that
future symbol collisions are avoided.

-  If you download the `nifti_clib` sourcecode inside your source tree,
   be sure to add `nifti_clib` to the `.gitignore` to ensure that it does
   not accidentally get committed to your source tree.

```bash
  git clone https://github.com/NIFTI-Imaging/nifti_clib.git
  mkdir -p build-test-integrated-demo
  cd build-test-integrated-demo
  cmake ../nifti_clib/real_easy/parent_project_demo
  make
```
