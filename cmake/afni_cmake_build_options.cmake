# Among other things, these options reconfigure what is built and installed.
# The cmake build partitions the programs/libraries/files into several
# components:

# afni_corelibs: Always built. Many of the shared object libraries in this
# repo as well as the model files.

# afni_corebinaries: Built if BUILD_BINARIES is set. Many of the c binaries
# have few dependencies outside of this codebase and are packaged with this
# component

# afni_tcsh: Built if DO_NOT_INSTALL_SCRIPTS is set to OFF. Tcsh scripts that
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

option(AFNI_BUILD_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs" OFF
)
cmake_dependent_option(
  BUILD_BINARIES "Build a large portion of the C executables" ON
  "NOT AFNI_BUILD_CORELIBS_ONLY" OFF
)

cmake_dependent_option(
  DO_NOT_INSTALL_SCRIPTS "Omits script installation" OFF
  "NOT AFNI_BUILD_CORELIBS_ONLY;BUILD_BINARIES" ON
)
mark_as_advanced(DO_NOT_INSTALL_SCRIPTS)

cmake_dependent_option(
  ADD_PYTHON "Includes scripts and modules for imports in python." ON 
  "NOT AFNI_BUILD_CORELIBS_ONLY;BUILD_BINARIES" OFF
)
mark_as_advanced(ADD_PYTHON)

cmake_dependent_option(
  ADD_RSTATS "Includes scripts and libraries only used for statistics in R." ON 
  "NOT AFNI_BUILD_CORELIBS_ONLY;" OFF
)
mark_as_advanced(ADD_RSTATS)

cmake_dependent_option(
  BUILD_X_DEPENDENT_GUI_PROGS "Build GUI applications with plugins." ON
  "NOT AFNI_BUILD_CORELIBS_ONLY;BUILD_BINARIES" OFF
)

cmake_dependent_option(
  BUILD_PLUGINS "Build plugins for AFNI GUI." ON 
  "BUILD_X_DEPENDENT_GUI_PROGS;" OFF
)
mark_as_advanced(BUILD_PLUGINS)

cmake_dependent_option(
  BUILD_OPENGL_DEPENDENT_GUI_PROGS
  "Build OPEN_GL dependent GUI applications with plugins." ON
  "BUILD_X_DEPENDENT_GUI_PROGS" OFF
)


# Define other customizations to the build-process
set_if_not_defined(BUILD_COREPLUGINS "By default a core set of plugins are built." ON)
option(GENERATE_PACKAGING_COMPONENTS "For internal use only" OFF)
option(USE_OMP "Use OpenMP to enamble <omp.h>" ON)
option(USE_CPACK "CPack can be used to generate source and binary distributions" OFF)
option(ENABLE_TESTS "Enable tests" OFF)



# Defining "external" dependencies i.e. anything that can be installed as a system install
option(USE_SYSTEM_GLW "Build and use AFNI's local copy of libGLw" ON)
option(USE_SYSTEM_GIFTI "Link against a system installed gifti library." ON)
set(USE_SYSTEM_NIFTI ${USE_SYSTEM_GIFTI})

