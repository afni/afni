# This file can be helpful to set up the compilers on OSX. An alternative
# perfectly usable strategy is to set the environment variables CC and CXX and
# possibly LDFLAGS (CXX is used for compiling dcm2niix, you can avoid this
# dependency by set USE_SYSTEM_DCM2NIIX=ON).  This file is used by calling
# cmake with the additional option "-DCMAKE_TOOLCHAIN_FILE=path/to/this/file".

# General notes:
# AppleClang does not have openmp support by default. You can install libomp
# with homebrew if you do want to use this compiler and build with OMP.
# On MacOS  /usr/bin/gcc is actually symlinked to clang. You have to go out of
# your way to use gcc (set the compiler path explicitly)
# Below is a list of working examples for the various compilers. You may have
# to modify the paths below to suit your system/compiler version.

# # The different compilers can be chosen by setting the AFNI_COMPILER_CHOICE variable
# See https://cmake.org/cmake/help/v3.18/variable/CMAKE_LANG_COMPILER_ID.html
set(ALLOWED_COMPILER_CHOICES "Clang;AppleClang;Intel;GNU")
if( EXISTS AFNI_COMPILER_CHOICE)
  if(NOT ${AFNI_COMPILER_CHOICE} IN_LIST ALLOWED_COMPILER_CHOICES)
    message(FATAL_ERROR "Please choose one of the following compilers: ${ALLOWED_COMPILER_CHOICES}")
  endif()
endif()

# Setup for the various compilers
if(AFNI_COMPILER_CHOICE STREQUAL Intel)
  # Can be downloaded from the intel website
  set(CMAKE_C_COMPILER /opt/intel/bin/icc)
  set(CMAKE_CXX_COMPILER /opt/intel/bin/icpc)
  set(CMAKE_EXE_LINKER_FLAGS "-L/opt/intel/lib")
elseif(AFNI_COMPILER_CHOICE STREQUAL AppleClang)
  # Comes with Xcode/apple command line tools
  # libomp is required for OMP support
  set(CMAKE_C_COMPILER clang)
  set(CMAKE_CXX_COMPILER clang++)
elseif(AFNI_COMPILER_CHOICE STREQUAL Clang)
  # Can be installed with "brew install llvm"
  set(CMAKE_C_COMPILER /usr/local/opt/llvm/bin/clang)
  set(CMAKE_CXX_COMPILER /usr/local/opt/llvm/bin/clang++)
  set(CMAKE_EXE_LINKER_FLAGS "-L/usr/local/opt/llvm/lib")
elseif(AFNI_COMPILER_CHOICE STREQUAL GNU)
  # Can be installed with "brew install gcc", will need to be modified for the
  # exact verion of gcc installed
  set(CMAKE_C_COMPILER /usr/local/bin/gcc-9)
  set(CMAKE_C_COMPILER /usr/local/bin/g++-9)
  set(CMAKE_EXE_LINKER_FLAGS "-L/usr/local/opt/gcc/lib")
endif()


# could use something like the follow but it likely has its own issues
# execute_process(COMMAND which icc  OUTPUT_VARIABLE  OUTPUT_STRIP_TRAILING_WHITESPACE CMAKE_C_COMPILER)
# execute_process(COMMAND which icpc OUTPUT_VARIABLE  OUTPUT_STRIP_TRAILING_WHITESPACE CMAKE_CXX_COMPILER)
