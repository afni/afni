# ##### Add core targets from elsewhere
add_subdirectory(rickr)
add_subdirectory(coxplot)
add_subdirectory(f2c)
add_subdirectory(niml)
add_subdirectory(eispack)
add_subdirectory(Audio)
add_subdirectory(volpack)
add_subdirectory(3DEdge)
add_subdirectory(ptaylor)
add_subdirectory(mpeg_encodedir)
include(CMakeLists_mri.txt)

# 3dinfo should be in the binaries cmake file but provides a test of linking in corelibs
# configuration
add_executable(3dinfo 3dinfo.c)
target_link_libraries(3dinfo AFNI::mri)

# ##### Add rio
set_source_files_properties(R_io.c PROPERTIES COMPILE_DEFINITIONS "std=gnu99")
add_library(rio R_io.c)
target_include_directories(
  rio
  PRIVATE ${LIBR_INCLUDE_DIRS}
)
target_link_libraries(rio ${LIBR_LIBRARIES} AFNI::mri)
add_afni_target_properties(rio)

# ##### Add libraries outside of corelibs

if(NOT AFNI_BUILD_CORELIBS_ONLY)
  add_subdirectory(crorden)
  # not sure how dcm2niix_afni will be installed... install(TARGETS dcm2niix  RUNTIME
  # DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}  RENAME woohoo)

  add_subdirectory(qhulldir)
  add_subdirectory(avovk)
endif()

# ##### Add many other binaries
if(BUILD_BINARIES)
  include(CMakeLists_binaries.txt)
endif()

# ##### Add scripts
if(NOT DO_NOT_INSTALL_SCRIPTS)
  add_subdirectory(scripts_install)
endif()

# ##### Add targets with a dependency on X
if(BUILD_X_DEPENDENT_GUI_PROGS)
  include(CMakeLists_x_dependent.txt)
  set(PROGRAMS_BUILT mrix)
endif()

# ##### Add suma
if(BUILD_OPENGL_DEPENDENT_GUI_PROGS)
  add_subdirectory(SUMA)
endif()

# ##### Add_plugin targets
if(BUILD_PLUGINS)
  include(CMakeLists_plugins.txt)
endif()

# ##### Create list of programs built
set(PROGRAMS_BUILT ${PROGRAM_BUILT})
assemble_target_list(PROGRAMS_BUILT SHOW_UNBUILT_PROGS)


# # First, make an install targets file (very similar to the one you made in the build
# directory): install(EXPORT AFNITargets FILE AFNITargets.cmake NAMESPACE AFNI::
# DESTINATION lib/cmake/AFNI )

# export(TARGETS afni SUMA NAMESPACE AFNI:: FILE AFNITargets.cmake) export(PACKAGE AFNI)
# find_package(AFNI should work because of the above)

# check the AFNITargets file and make sure you don't have the include directory included
# twice if you want clean cmake files : https://cliutils.gitlab.io/modern-
# cmake/chapters/install/installing.html install(TARGETS afni afobj EXPORT AFNITargets
# LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR} ARCHIVE DESTINATION
# ${AFNI_INSTALL_LIBRARY_DIR} RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR} INCLUDES
# DESTINATION ${AFNI_INSTALL_INCLUDE_DIR} )
