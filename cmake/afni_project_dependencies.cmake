include(CMakePackageConfigHelpers)
include(FetchContent)
find_package(Motif REQUIRED)
find_package(NetCDF REQUIRED)
include(FindStandardMathLibrary)
include(BuildType)
find_package(ZLIB REQUIRED)
find_package(GSL REQUIRED)
optional_bundle(src/volpack)
optional_bundle(src/f2c)
set_if_not_defined(USE_SYSTEM_QHULL ON)

if(USE_OMP)
  find_package(OpenMP REQUIRED)
endif()

if(ADD_RSTATS)
  include(FindLibR)
endif()

# set(Python_FIND_VIRTUALENV FIRST)
set(CMAKE_FIND_FRAMEWORK LAST)
find_package(Python 3 REQUIRED COMPONENTS Interpreter)
if(NOT ${Python_FOUND})
  message(FATAL_ERROR "Cannot find python interpreter (FOUND: ${Python_EXECUTABLE})")
endif()


if(NOT AFNI_BUILD_CORELIBS_ONLY)
  if(NOT USE_SYSTEM_QHULL)
    # Perhaps an error should be raised if the appropriate binaries are missing
    add_subdirectory(src/qhulldir)
  endif()
endif()

if(BUILD_X_DEPENDENT_GUI_PROGS)
  find_package(JPEG 62 REQUIRED)
  find_package(X11 REQUIRED)
  optional_bundle(src/XmHTML)
endif()

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


# SUMA dependency management
if(BUILD_OPENGL_DEPENDENT_GUI_PROGS)
  # Check for and configure for external dependencies
  find_package(OpenGL REQUIRED)
  find_package(GLib2)
  optional_bundle(src/SUMA/GLUT)
  optional_bundle(src/SUMA/gts)

  if(USE_SYSTEM_GLW)
    find_package(GLw REQUIRED)
  endif()
endif()