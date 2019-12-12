option(BUILD_SHARED_LIBS "Toggle building shared libraries" ON)

option(AFNI_BUILD_CORELIBS_ONLY
       "Only build core libraries, no SUMA, plugins or programs" OFF
)
cmake_dependent_option(
  BUILD_BINARIES "Build a large portion of the C executables" ON
  "NOT AFNI_BUILD_CORELIBS_ONLY" OFF
)
cmake_dependent_option(
  DO_NOT_INSTALL_SCRIPTS "Omits script installation" OFF "NOT AFNI_BUILD_CORELIBS_ONLY"
  OFF
)
mark_as_advanced(DO_NOT_INSTALL_SCRIPTS)
cmake_dependent_option(
  BUILD_X_DEPENDENT_GUI_PROGS "Build GUI applications with plugins." ON
  "NOT AFNI_BUILD_CORELIBS_ONLY" OFF
)
cmake_dependent_option(
  BUILD_PLUGINS "Build plugins for AFNI GUI." OFF "BUILD_X_DEPENDENT_GUI_PROGS;" ON
)
mark_as_advanced(BUILD_PLUGINS)
set_if_not_defined(BUILD_COREPLUGINS "By default a core set of plugins are built." ON)
cmake_dependent_option(
  BUILD_OPENGL_DEPENDENT_GUI_PROGS
  "Build OPEN_GL dependent GUI applications with plugins." ON
  "NOT AFNI_BUILD_CORELIBS_ONLY;BUILD_X_DEPENDENT_GUI_PROGS" OFF
)
# Define options to customize the build-process
option(USE_SYSTEM_GLW "Build and use AFNI's local copy of libGLw" ON)


option(USE_OMP "Use OpenMP to enamble <omp.h>" ON)
option(ENABLE_TESTS "Enable tests" OFF)
