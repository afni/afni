# Among other things, these options reconfigure what is built and installed.
# The cmake build partitions the programs/libraries/files into several
# components:

# corelibs: Always built. Many of the shared object libraries in this
# repo as well as the model files.

# afni_corebinaries: Built if COMP_ADD_BINARIES is set. Many of the c binaries
# have few dependencies outside of this codebase and are packaged with this
# component

# afni_tcsh: Built if COMP_ADD_TCSH is set to OFF. Tcsh scripts that
# make use of AFNI's core functionality.

# afni_python: This includes executables and modules for importing AFNI
# functionality written in python

# afni_rstats: R scripts to perform statistical analyses

# afni_gui: This includes the "afni" program as well as plugins and other
# binaries that are dependent on X and so have substantially more dependencies
# than the aforementioned programs written in C.

# afni_suma: Along with the "suma" GUI for surface analysis, this component
# includes programs that have more extensive dependencies associated with
# graphical processing.

# The installed files associated in the above components are stored in 
# packaging/installation_components.txt. This should be modified when new programs
# are added to the build. Alternatively it can be generated (or at least should be able to be generated)
# by using the script packaging/define_installation_components.py


option(BUILD_SHARED_LIBS "Toggle building shared libraries" ON)

option(COMP_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs" OFF
)
cmake_dependent_option(
  COMP_ADD_BINARIES "Build a large portion of the C executables" ON
  "NOT COMP_CORELIBS_ONLY" OFF
)

cmake_dependent_option(
  COMP_ADD_TCSH "Include tcsh scripts in installation" ON
  "NOT COMP_CORELIBS_ONLY;COMP_ADD_BINARIES" OFF
)
mark_as_advanced(COMP_ADD_TCSH)

cmake_dependent_option(
  COMP_ADD_PYTHON "Includes scripts and modules for imports in python." ON 
  "NOT COMP_CORELIBS_ONLY;COMP_ADD_BINARIES" OFF
)
mark_as_advanced(COMP_ADD_PYTHON)

cmake_dependent_option(
  COMP_ADD_RSTATS "Includes scripts and libraries only used for statistics in R." ON 
  "NOT COMP_CORELIBS_ONLY;" OFF
)
mark_as_advanced(COMP_ADD_RSTATS)

cmake_dependent_option(
  COMP_X_DEPENDENT_GUI_PROGS "Build GUI applications with plugins." ON
  "NOT COMP_CORELIBS_ONLY;COMP_ADD_BINARIES" OFF
)

cmake_dependent_option(
  COMP_ADD_PLUGINS "Build plugins for AFNI GUI." ON 
  "COMP_X_DEPENDENT_GUI_PROGS;" OFF
)
mark_as_advanced(COMP_ADD_PLUGINS)
if(NOT (COMP_ADD_PLUGINS) AND (COMP_X_DEPENDENT_GUI_PROGS))
  message(FATAL_ERROR "Building the plugins is currently a mandatory part of add the AFNI GUI to the build")
endif()


cmake_dependent_option(
  COMP_ADD_ALL_PLUGINS
  "Build all plugins." ON
  "COMP_ADD_PLUGINS" OFF
)

cmake_dependent_option(
  COMP_OPENGL_DEPENDENT_GUI_PROGS
  "Build OPEN_GL dependent GUI applications with plugins." ON
  "COMP_X_DEPENDENT_GUI_PROGS" OFF
)


# Define other customizations to the build-process
option(COMP_ADD_ATLASES "Use datalad to download data for distribution" OFF)
set_if_not_defined(COMP_ADD_ALL_PLUGINS "By default a core set of plugins are built." ON)
option(GENERATE_PACKAGING_COMPONENTS "For internal use only" OFF)
option(USE_OMP "Use OpenMP to enamble <omp.h>" ON)
option(USE_CPACK "CPack can be used to generate source and binary distributions" OFF)
option(ENABLE_TESTS "Enable tests" OFF)
option(RUN_PLUGIN_CHECK "Check that plugins build without any missing symbols" OFF)


# Defining "external" dependencies i.e. anything that can be installed as a
# system install. This list will also include all directories added with the
# optional_bundle macro. Values that can be easily overwritten without failure
# can be overwritten concisely USE_SYSTEM_ALL variable defined i.e.
# cmake -DUSE_SYSTEM_ALL=ON
# The above would set all options to all except for GLW on OSX because it
# fails, and netcdf and f2c because they are not currently supported.
# Also note that the USE_SYSTEM_ALL variable is not persisted in the cmake
# cache so passed this variable on the commandline will only have an effect
# during configure time
option(USE_SYSTEM_GLW "Do not build and use AFNI's local copy of libGLw" ON)
option(USE_SYSTEM_NETCDF "Do not build and use AFNI's local copy of netcdf" ON)
option(USE_SYSTEM_GIFTI "Link against a system installed gifti library." ON)
option(USE_SYSTEM_GTS "If building SUMA use a system installation of gts (does not work on MacOS)" OFF)
option(USE_SYSTEM_DCM2NIIX "Omit build and installation of dcm2niix_afni." OFF)
set(USE_SYSTEM_NIFTI ${USE_SYSTEM_GIFTI})

if(DEFINED USE_SYSTEM_ALL)
  # a list of libraries that shouldn't be counted in ALL
  set(SLIGHTLY_BROKEN "GLW;F2C;NETCDF;VOLPACK")
  
  foreach(OPTIONAL_LIB GLW NETCDF GIFTI GTS DCMNIIX VOLPACK F2C GLUT XMHTML)
    if("${OPTIONAL_LIB}" IN_LIST SLIGHTLY_BROKEN)
      continue()
    endif()

    message("Setting ${OPTIONAL_LIB} to ${USE_SYSTEM_ALL} because USE_SYSTEM_ALL is defined")
    set(USE_SYSTEM_${OPTIONAL_LIB} ${USE_SYSTEM_ALL})
  endforeach()
endif()