include(CMakePackageConfigHelpers)
include(FetchContent)
find_package(Motif REQUIRED)
include(FindStandardMathLibrary)
include(BuildType)
find_package(ZLIB REQUIRED)
find_package(GSL REQUIRED)
optional_bundle(src/volpack)
optional_bundle(src/f2c)
set_if_not_defined(USE_SYSTEM_QHULL ON)

if(USE_OMP)
  find_package(OpenMP COMPONENTS C REQUIRED)
endif()

if(COMP_ADD_RSTATS)
  include(FindLibR)
endif()

# set(Python_FIND_VIRTUALENV FIRST)
set(CMAKE_FIND_FRAMEWORK LAST)
find_package(Python 3 REQUIRED COMPONENTS Interpreter)
if(NOT ${Python_FOUND})
  message(FATAL_ERROR "Cannot find python interpreter (FOUND: ${Python_EXECUTABLE})")
endif()


if(NOT COMP_CORELIBS_ONLY)
  if(NOT USE_SYSTEM_QHULL)
    # Perhaps an error should be raised if the appropriate binaries are missing
    add_subdirectory(src/qhulldir)
  endif()
endif()

if(COMP_X_DEPENDENT_GUI_PROGS)
  find_package(JPEG 62 REQUIRED)
  find_package(X11 REQUIRED)
  optional_bundle(src/XmHTML)
endif()


# SUMA dependency management
if(COMP_OPENGL_DEPENDENT_GUI_PROGS)
  # Check for and configure for external dependencies
  if(APPLE)
    find_package(XQuartzGL REQUIRED)
  else()
    find_package(OpenGL REQUIRED)
    optional_bundle(src/SUMA/GLUT)
  endif()
  find_package(GLib2)

  if(USE_SYSTEM_GLW)
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
if(COMP_ADD_ATLASES)
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
    PERMISSIONS "WORLD_READ"
  )

endif()



# Declare the direct dependencies. Can be used to avoid collisions with pre-existing
# installations of the nifti libraries
if(USE_SYSTEM_NIFTI)

  find_package(NIFTI REQUIRED)
else()
FetchContent_Declare(
  fetch_nifti_clib_git_repo   
  GIT_REPOSITORY https://github.com/NIFTI-Imaging/nifti_clib.git 
  GIT_TAG 9563fa4ae56140d8d3d268688ff2386bf2daf2c8
  )
FetchContent_MakeAvailable(fetch_nifti_clib_git_repo)
endif()

if(USE_SYSTEM_GIFTI)
  find_package(GIFTI REQUIRED)
else()
FetchContent_Declare(
  gifti_clib   
  GIT_REPOSITORY https://github.com/NIFTI-Imaging/gifti_clib.git 
  GIT_TAG 0f8b7c073d2c79a235687419a09523b203a4ced8
  )
FetchContent_MakeAvailable(gifti_clib)
endif()

if(USE_SYSTEM_NETCDF)
  find_package(NetCDF REQUIRED)
else()
message(FATAL_ERROR "building netcdf not currently supported")
FetchContent_Declare(
  netcdf_lib   
  GIT_REPOSITORY https://github.com/Unidata/netcdf-c.git
  GIT_TAG 2a34eb2ac5996dc23339bdb72918eb5503393d77
  )
FetchContent_MakeAvailable(netcdf_lib)
endif()

