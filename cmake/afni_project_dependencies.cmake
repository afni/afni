include(CMakePackageConfigHelpers)
include(FetchContent)
include(FindStandardMathLibrary)
include(BuildType)
find_package(ZLIB REQUIRED)
optional_bundle(src/f2c)
set_if_not_defined(USE_SYSTEM_QHULL ON)

if(USE_OMP)
  find_package(OpenMP COMPONENTS C REQUIRED)
endif()

if(COMP_RSTATS)
    find_package(LibR)
    if(NOT LIBR_FOUND)
        message(FATAL_ERROR "Could not find R. Consider installing R, or setting COMP_RSTATS to OFF")
    endif()
endif()



# The python interpreter used for the build (and subsequent testing if
# FORCE_CURRENT_PY_INTERP_FOR_TESTS is not set) is the first one found that
# satisfies the version requirements. The PATH variable is used for this,
# and OSX framework python is found last. If an environment is used for
# software isolation then python is only searched for in this environment.
# For more details see:
# https://cmake.org/cmake/help/git-stage/module/FindPython.html
set(CMAKE_FIND_FRAMEWORK LAST)
set(Python_FIND_VIRTUALENV ONLY)
set(Python_FIND_STRATEGY LOCATION)

# python >=3.6 supported
find_package(Python 3.6 REQUIRED COMPONENTS Interpreter)
if(NOT ${Python_FOUND})
  message(FATAL_ERROR "Cannot find python interpreter (FOUND: ${Python_EXECUTABLE})")
endif()


if(NOT COMP_CORELIBS_ONLY)
  if(NOT USE_SYSTEM_QHULL)
    # Perhaps an error should be raised if the appropriate binaries are missing
    add_subdirectory(src/qhulldir)
  endif()


if(COMP_GUI)
  find_package(X11 REQUIRED)
  find_package(Motif REQUIRED)
  find_package(JPEG 62 REQUIRED)
  optional_bundle(src/XmHTML)
endif()


# SUMA dependency management
if(COMP_SUMA)
  # Check for and configure for external dependencies
  if(APPLE)
    find_package(XQuartzGL REQUIRED)
  else()
    find_package(OpenGL REQUIRED)
    optional_bundle(src/SUMA/GLUT)
  endif()
  find_package(GLib2)
  find_package(GSL REQUIRED)
  if(USE_SYSTEM_GLW)
      # Not that SUMA makes use of the glwDrawingAreaWidgetClass symbol that is
      # not externed in the version of glw distributed with most operating 
      # systems. Setting USE_SYSTEM_GLW to ON is generally a bad idea. The 
      # build system should hopefully detected the error it causes at build time 
      # though. By default a local version of glw is directly incorporated 
      # into libSUMA.so
    find_package(GLw REQUIRED)
  endif()

  if(USE_SYSTEM_GTS)
    find_package(GTS REQUIRED)
  else()
    FetchContent_Declare(
      gts   
      GIT_REPOSITORY https://github.com/leej3/gts
      GIT_TAG 962155a01f5a1b87bd64e3e3d880b4dbc2347ac7
      )
    FetchContent_MakeAvailable(gts)
  endif()
endif()


# Add AFNI atlases (they're not really just atlases but for legacy reasons
# we'll call them that)
if(COMP_ATLASES)
  find_program(DATALAD datalad)
  if(DATALAD_NOT_FOUND)
    message(FATAL_ERROR "The datalad executable could not be found. This is required for installation of atlases")
  endif()
  find_program(RSYNC rsync)
  if(RSYNC_NOT_FOUND)
    message(FATAL_ERROR "The rsync executable could not be found. This is required for installation of atlases")
  endif()



  # add_custom_target(
  #   fetch_atlases 
  #   ALL
  #   DEPENDS ${CMAKE_BINARY_DIR}/afni_data 
  #   COMMAND "echo" "Atlases/distribution data is present"
  #   )

file(
  GENERATE 
  OUTPUT ${CMAKE_BINARY_DIR}/fetch_atlases.sh
  CONTENT "\
    \n${DATALAD} install https://afni.nimh.nih.gov/pub/dist/data/afni_data\
    \ncd ${CMAKE_BINARY_DIR}/afni_data\
    \n${DATALAD} get .\
    \n${DATALAD} unlock .\
  "
  )
  add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/afni_data 
    DEPENDS ${CMAKE_BINARY_DIR}/fetch_atlases.sh
    COMMAND "bash" "fetch_atlases.sh"
    USES_TERMINAL
    )

    add_custom_target(
      atlases_dir 
      ALL
      VERBATIM
      COMMAND echo Data for distribution has been downloaded (ADD_ATLASES has been set to ON)
      DEPENDS ${CMAKE_BINARY_DIR}/afni_data 
      )

  install(
    DIRECTORY ${CMAKE_BINARY_DIR}/afni_data/atlases/
    COMPONENT atlases
    DESTINATION ${AFNI_INSTALL_ATLAS_DIR}
    PATTERN '*'
    PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
  )

endif()
endif() # end of non core libs

set_if_not_defined(DOWNLOAD_TEST_DATA OFF)
# Declare the direct dependencies. Can be used to avoid collisions with pre-existing
# installations of the nifti libraries
if(USE_SYSTEM_NIFTI)

  find_package(NIFTI REQUIRED)
else()
FetchContent_Declare(
  nifti_clib   
  GIT_REPOSITORY https://github.com/NIFTI-Imaging/nifti_clib.git 
  GIT_TAG 65f801b9c2f1f15f4de4a19d45e6595c25765632
  )
FetchContent_MakeAvailable(nifti_clib)
endif()

if(USE_SYSTEM_GIFTI)
  find_package(GIFTI REQUIRED)
else()
FetchContent_Declare(
  gifti_clib   
  GIT_REPOSITORY https://github.com/NIFTI-Imaging/gifti_clib.git 
  GIT_TAG 5eae81ba1e87ef3553df3b6ba585f12dc81a0030
  )
FetchContent_MakeAvailable(gifti_clib)
endif()


