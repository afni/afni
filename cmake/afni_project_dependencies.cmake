include(CMakePackageConfigHelpers)
include(FetchContent)

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

# Declare the direct dependencies. Can be used to avoid collisions with pre-existing
# installations of the nifti libraries
if(USE_SYSTEM_NIFTI)
  find_package(NIFTI REQUIRED)
else()
FetchContent_Declare(
  fetch_nifti_clib_git_repo   
  GIT_REPOSITORY https://github.com/leej3/nifti_clib.git 
  GIT_TAG 2b61eb8960341f64d349b03c78c8dd609fa3cc09
  )
FetchContent_MakeAvailable(fetch_nifti_clib_git_repo)
endif()

if(USE_SYSTEM_GIFTI)
  find_package(GIFTI REQUIRED)
else()
FetchContent_Declare(
  gifti_clib   
  GIT_REPOSITORY https://github.com/leej3/gifti_clib.git 
  GIT_TAG 075b5933c29b259fff5ac8dddf5ce72d3f0f3f5c
  )
FetchContent_MakeAvailable(gifti_clib)
endif()


# set(Python_FIND_VIRTUALENV FIRST)
set(CMAKE_FIND_FRAMEWORK LAST)
find_package(Python 3 REQUIRED COMPONENTS Interpreter)
if(NOT ${Python_FOUND})
  message(FATAL_ERROR "Cannot find python interpreter (FOUND: ${Python_EXECUTABLE})")
endif()
