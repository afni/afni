# Among other things, these options reconfigure what is built and installed.
# The cmake build partitions the programs/libraries/files into several
# components:

# corelibs: Always built. Many of the shared object libraries in this
# repo as well as the model files.

# afni_corebinaries: Built if COMP_COREBINARIES is set. Many of the c binaries
# have few dependencies outside of this codebase and are packaged with this
# component

# afni_tcsh: Built if COMP_TCSH is set to OFF. Tcsh scripts that
# make use of AFNI's core functionality.

# afnipy: This includes executables and modules for importing AFNI
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

# Set installation directories
if(ABIN_INSTALL)
    set(CMAKE_INSTALL_PREFIX $ENV{HOME}/abin CACHE PATH "" FORCE)
endif()
include(GNUInstallDirs)
set_if_not_defined(AFNI_INSTALL_LIBRARY_DIR ${CMAKE_INSTALL_LIBDIR})
set_if_not_defined(AFNI_INSTALL_ARCHIVE_DIR ${AFNI_INSTALL_LIBRARY_DIR})
set_if_not_defined(AFNI_INSTALL_RUNTIME_DIR ${CMAKE_INSTALL_BINDIR})
set_if_not_defined(AFNI_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_INCLUDEDIR})
set_if_not_defined(AFNI_INSTALL_ATLAS_DIR ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/data)
set_if_not_defined(AFNI_INSTALL_FUNSTUFF_DIR ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/funstuff)
set_if_not_defined(AFNI_INSTALL_DOC_DIR ${CMAKE_INSTALL_DOCDIR})

# Set built targets in build tree
set_if_not_defined(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/targets_built)
set_if_not_defined(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/targets_built)

# Add a default attempt at setting the run path correctly for binaries
if(NOT CMAKE_SKIP_INSTALL_RPATH)
  file(
    RELATIVE_PATH
    relDir
    ${CMAKE_INSTALL_PREFIX}/${AFNI_INSTALL_RUNTIME_DIR}
    ${CMAKE_INSTALL_PREFIX}/${AFNI_INSTALL_LIBRARY_DIR}
  )
  if(APPLE)
    set_if_not_defined(CMAKE_INSTALL_RPATH  "@loader_path/${relDir}")
  else()
    set_if_not_defined(CMAKE_INSTALL_RPATH   "\$ORIGIN/${relDir}")
  endif()
endif()

option(BUILD_SHARED_LIBS "Toggle building shared libraries" ON)


# The following defines build components. Toggling the following variables can
# be used to tune what parts of the AFNI suite of tools are included in the
# build. By default, everything that is built is installed but for the
# purposes of packaging, one can define a list of components in the
# variable COMP_INSTALL_RESTRICTED_LIST to restrict what files are
# actually installed
option(COMP_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs" OFF
)
set(COMP_CORELIBS "Core C libraries" ON)

cmake_dependent_option(
  COMP_COREBINARIES "Build a large portion of the C executables" ON
  "NOT COMP_CORELIBS_ONLY" OFF
)

cmake_dependent_option(
  COMP_TCSH "Include tcsh scripts in installation" ON
  "NOT COMP_CORELIBS_ONLY;COMP_COREBINARIES" OFF
)
mark_as_advanced(COMP_TCSH)

cmake_dependent_option(
  COMP_PYTHON "Includes scripts and modules for imports in python." ON 
  "NOT COMP_CORELIBS_ONLY;COMP_COREBINARIES" OFF
)
mark_as_advanced(COMP_PYTHON)
set_if_not_defined(STANDARD_PYTHON_INSTALL "afnipy installation is required for running the tests suite" ON)

cmake_dependent_option(
    COMP_RSTATS "Includes scripts and libraries only used for statistics in R." OFF 
  "NOT COMP_CORELIBS_ONLY;" OFF
)

cmake_dependent_option(
  COMP_GUI "Build AFNI gui with plugins, and some other X-dependent applications." ON
  "NOT COMP_CORELIBS_ONLY;COMP_COREBINARIES" OFF
)

cmake_dependent_option(
  COMP_FUNSTUFF "Add poems and faces." ON
  "COMP_GUI;" OFF
)
mark_as_advanced(COMP_FUNSTUFF)
cmake_dependent_option(
  COMP_DOCS "Add docs." ON
  "COMP_GUI;" OFF
)
mark_as_advanced(COMP_DOCS)

cmake_dependent_option(
  COMP_PLUGINS "Build plugins for AFNI GUI." ON
  "COMP_GUI;" OFF
)
mark_as_advanced(COMP_PLUGINS)
if(NOT (COMP_PLUGINS) AND (COMP_GUI))
  message(FATAL_ERROR "Building the plugins is currently a mandatory part of add the AFNI GUI to the build")
endif()


cmake_dependent_option(
  COMP_ALL_PLUGINS
  "Build all plugins." ON
  "COMP_PLUGINS" OFF
)

cmake_dependent_option(
  COMP_SUMA
  "Build OPEN_GL dependent progs, most notably the suma suite of programs." ON
  "COMP_GUI;" OFF
)

# Manage restricted installation of specific components. This can be used for
# packaging purposes. If a specific component needs to be installed
# individually this can be used. So for example all of the X dependent
# programs. These will then fail at runtime unless the sub-packages it depends
# on (corelibs,etc) have also been installed. Components can be inferred from
# the COMP_... variables. i.e. PYTHON,TCSH,GUI,OPENGL_DEPENDENT_GUI_PROGS.
# This variable can be set on the cmake invocation by defining the variable.
# e.g. cmake -DCOMP_INSTALL_RESTRICTED_LIST='PYTHON;TCSH' $SRC_DIR
set_if_not_defined(COMP_INSTALL_RESTRICTED_LIST "")
mark_as_advanced(COMP_INSTALL_RESTRICTED_LIST)
set(ALLOWED_INSTALL_COMPS "SUMA;GUI;PYTHON;TCSH;COREBINARIES;CORELIBS;RSTATS")
check_afni_install_components("${ALLOWED_INSTALL_COMPS}" "${COMP_INSTALL_RESTRICTED_LIST}")


# Define other customizations to the build-process
option(COMP_ATLASES "Use datalad to download data for distribution" OFF)
set_if_not_defined(COMP_ALL_PLUGINS "By default a core set of plugins are built." ON)
option(GENERATE_PACKAGING_COMPONENTS "For internal use only" OFF)
option(REMOVE_BUILD_PARITY_CHECKS "For internal use only" OFF)
option(USE_OMP "Use OpenMP to enamble <omp.h>" ON)
option(USE_CPACK "CPack can be used to generate source and binary distributions" OFF)
option(ENABLE_TESTS "Enable tests" OFF)
option(RUN_PLUGIN_CHECK "Check that plugins build without any missing symbols" OFF)
set_if_not_defined(FETCHCONTENT_QUIET ON CACHE BOOL "Quietly fetch")
set_if_not_defined(STANDARD_PYTHON_INSTALL ON)

# Defining "external" dependencies i.e. anything that can be installed as a
# system install. This list will also include all directories added with the
# optional_bundle macro. Values that can be easily overwritten without failure
# can be overwritten concisely USE_SYSTEM_ALL variable defined i.e.
# cmake -DUSE_SYSTEM_ALL=ON
# The above would set all options to ON except for GLW on OSX because it
# fails, and f2c because they are not currently supported.
include(afni_system_dependency_overwrites)
set_if_not_defined(USE_SYSTEM_ALL 0)
foreach(OPTIONAL_LIB GLW GIFTI NIFTI GTS DCMNIIX F2C GLUT XMHTML)
    set_if_not_defined(USE_SYSTEM_${OPTIONAL_LIB} ${USE_SYSTEM_ALL})
endforeach()
