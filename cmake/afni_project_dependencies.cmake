include(CPack)
include(CMakePackageConfigHelpers)
find_package(Motif REQUIRED)
find_package(NetCDF REQUIRED)
include(FindX11)
include(FindStandardMathLibrary)
include(FindLibR)
include(BuildType)
find_package(ZLIB REQUIRED)

if(USE_OMP)
  find_package(OpenMP REQUIRED)
endif()

if(NOT USE_OMP)
  message(FATAL_ERROR REQUIRED)
endif()

set_if_not_defined(USE_SYSTEM_QHULL ON)
# Find or fetch nifti_clib a prefix can be used to avoid collisions with pre-existing
# installations of the nifti libraries
include(get_nifti_and_gifti_targets)

# set(Python_FIND_VIRTUALENV FIRST)
set(CMAKE_FIND_FRAMEWORK LAST)
find_package(Python 3 REQUIRED COMPONENTS Interpreter)
if(NOT ${Python_FOUND})
  message(FATAL_ERROR "Cannot find python interpreter (FOUND: ${Python_EXECUTABLE})")
endif()
