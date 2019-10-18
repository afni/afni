add_subdirectory(rickr)
add_subdirectory(coxplot)
add_subdirectory(f2cdir)
add_subdirectory(niml)
add_subdirectory(eispack)
add_subdirectory(Audio)
add_subdirectory(volpack)
add_subdirectory(3DEdge)

include(CMakeLists_mri.txt)

# should be in binraries but provides a test of linking in corelibs configuration
add_executable(3dinfo 3dinfo.c)
target_link_libraries(3dinfo AFNI::mri)


include(FindLibR)
set_source_files_properties(R_io.c PROPERTIES COMPILE_DEFINITIONS "std=gnu99")
add_library(rio R_io.c)
target_include_directories(rio 
  PRIVATE ${LIBR_INCLUDE_DIRS} 
  PUBLIC $<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>)
target_link_libraries(rio ${LIBR_LIBRARIES} AFNI::mri)
add_afni_target_properties(rio)


# TODO: /afni/afni_codebase/src/Audio/@sound_to_header.csh;

if(NOT AFNI_BUILD_CORELIBS_ONLY)
  ########## Additional binaries and libraries
  # XXX remove dlcompat
  # XXX remove jpeg-6b
  add_subdirectory(crorden)
  add_subdirectory(qhulldir)
 
  ## Install scripts
  if (NOT DO_NOT_INSTALL_SCRIPTS)
    add_subdirectory(scripts_install)
  endif()
  ################# Plugins
  
  # Install many other binaries
  if (BUILD_BINARIES)
  # message(FATAL_ERROR "Not currently supported")
  include(CMakeLists_binaries.txt)
  endif()
endif()

if(BUILD_X_PROGS)
  add_subdirectory(svm)
  add_subdirectory(XmHTML)

  set(MRIX_OBJS display.c imseq.c bbox.c xim.c xutil.c xutil_webber.c LiteClue.c coxplot/plot_motif.c coxplot/plot_ts.c coxplot/plot_x11.c coxplot/plot_strip.c suma_help.c)
  add_library(mrix ${MRIX_OBJS})
  add_afni_target_properties(mrix)
  set_target_properties(mrix PROPERTIES PUBLIC_HEADER "imseq.h;display.h;xim.h;bbox.h;xutil.h")
  target_include_directories(mrix PUBLIC $<TARGET_PROPERTY:AFNI::XmHTML,INTERFACE_INCLUDE_DIRECTORIES> )
  target_link_libraries(mrix 
    PUBLIC AFNI::mri
    PRIVATE AFNI::coxplot   AFNI::XmHTML ${X11_LIBRARIES} ${MOTIF_LIBRARIES} ${X11_Xt_LIB}  libheaders)
  add_afni_target_properties(mrix)
  add_library(AFNI::mrix ALIAS mrix)

endif()
if (BUILD_APPS)
  # Add_plugin targets
  include(CMakeLists_plugins.txt)

  ############ Apps
  include(CMakeLists_apps.txt)
endif()
# Install makefile programs that are built by this project.
execute_process(
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
     COMMAND bash "-c" "make -s -f Makefile.INCLUDE prog_list_bin")
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/prog_list_bin.txt ALL_PROGRAMS)
string(STRIP ${ALL_PROGRAMS} ALL_PROGRAMS)
string(REPLACE "\n" " "  ALL_PROGRAMS ${ALL_PROGRAMS})
separate_arguments(ALL_PROGRAMS)
set(PROGRAMS_BUILT "")
set(NOT_BUILT "")
set(installable_targets ${ALL_PROGRAMS} mri rio)


# remove some programs that should not be here
foreach(other_project_target gifti_tool giftiio gifti_test nifti_tool niftiio)
message("Removing ${other_project_target} because it is not built by this project")
list(REMOVE_ITEM installable_targets ${other_project_target})
endforeach()

foreach(program ${installable_targets})
  if(TARGET ${program})
    list(APPEND PROGRAMS_BUILT ${program})
    add_afni_target_properties(${program})
  else()
    list(APPEND NOT_BUILT ${program})
  endif()
endforeach()
# message("progs_built:'${PROGRAMS_BUILT}'")
message("programs not yet built:'${NOT_BUILT}'")

message("programs  built:'${PROGRAMS_BUILT}'")
install(TARGETS ${PROGRAMS_BUILT}
       RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
       LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
)  

# # First, make an install targets file (very similar to the one you made in the build directory):
# install(EXPORT AFNITargets
#         FILE AFNITargets.cmake
#         NAMESPACE AFNI::
#         DESTINATION lib/cmake/AFNI
#          )


# export(TARGETS afni SUMA NAMESPACE AFNI:: FILE AFNITargets.cmake)
# export(PACKAGE AFNI)
# find_package(AFNI should work because of the above)

# check the AFNITargets file and make sure you don't have the include directory included twice if you want clean cmake files : https://cliutils.gitlab.io/modern-cmake/chapters/install/installing.html
# install(TARGETS afni afobj
#         EXPORT AFNITargets
#         LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
#         ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
#         RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
#         INCLUDES DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
#         )
