# ##### Create global entity in which to store installed targets
set_property(GLOBAL PROPERTY INSTALLED_PROGS "")

# ##### Add core targets from elsewhere
add_subdirectory(rickr)
add_subdirectory(niml)
add_subdirectory(eispack)
add_subdirectory(Audio)
add_subdirectory(3DEdge)
add_subdirectory(mpeg_encodedir)
include(CMakeLists_mri.txt)

# f2c.h is required for some libraries (local or system f2c library can likely
# be used). Additionally, parser.f is sometimes modified and reconverted to c
# source code. In this case the header needs to match that of the f2c used for
# conversion. This build system system supports the latter in an integrated
# manner if a system f2c library and binary are found. This configuration can
# be triggered by calling cmake with the definitions -DUSE_SYSTEM_F2C=ON and
# -DUSE_F2C=ON

if (USE_F2C AND NOT F2C_EXECUTABLE)
  message(FATAL_ERROR "f2c should be found: ${F2C_EXECUTABLE}")
endif()

# ##### Add models
foreach(
  target
  model_beta
  model_constant
  model_conv_PRF
  model_conv_PRF_6
  model_conv_PRF_DOG
  model_conv_cosine4
  model_conv_diffgamma
  model_convgamma
  model_convgamma2a
  model_demri_3
  model_diffexp
  model_diffusion
  model_exp
  model_expMEMRI
  model_expMEMRI3
  model_expr2
  model_gammavar
  model_linear
  model_linplusort
  model_michaelis_menton
  model_null
  model_quadratic
  model_sinewave_ap
  model_sinewave_apf
  model_squarewave_ap
  model_squarewave_apf
  model_trnglwave_ap
  model_trnglwave_apf
  model_zero
  )
  add_afni_library(${target} ${target}.c)
  target_link_libraries(
    ${target}
    PRIVATE
            AFNI::mri
            NIFTI::nifti2
            NIFTI::nifticdf
  )
    endforeach()

# ##### Add libraries and programs outside of corelibs
if(NOT COMP_CORELIBS_ONLY)
  if(USE_SYSTEM_DCM2NIIX)
    # Hack to bypass, target checks because this should be a symlink to a the
    # runtime dependency dcm2niix
    # log_target_as_installed(dcm2niix)
  else()
    add_subdirectory(crorden)
    set_target_properties(dcm2niix PROPERTIES OUTPUT_NAME dcm2niix_afni)
    # add_afni_target_properties(dcm2niix)
  endif()
  # not sure how dcm2niix_afni will be installed... install(TARGETS dcm2niix  RUNTIME
  # DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}  RENAME woohoo)
endif()

# Add majority of binaries
if(COMP_COREBINARIES)
  include(CMakeLists_binaries.txt)
endif()

# Add scripts (includes shell and R scripts)
if(COMP_TCSH)
  add_subdirectory(scripts_install)
endif()

# Add python code
if(COMP_PYTHON)
  add_subdirectory(python_scripts)
endif()

# Add targets with a dependency on X
if(COMP_GUI)
  add_subdirectory(coxplot)
  include(CMakeLists_x_dependent.txt)
  set(PROGRAMS_BUILT mrix)
endif()

# Add suma
if(COMP_SUMA)
  # segtools/avovk only used by suma so add it here
  add_subdirectory(ptaylor)
  add_subdirectory(avovk)
  add_subdirectory(SUMA)
endif()

# Add_plugin targets
if(COMP_PLUGINS)
  include(CMakeLists_plugins.txt)
endif()

if(COMP_RSTATS)
  # Add rio
  set_source_files_properties(R_io.c PROPERTIES COMPILE_DEFINITIONS "std=gnu99")
  add_afni_library(rio R_io.c)
  target_include_directories(
    rio
    PRIVATE ${LIBR_INCLUDE_DIRS}
  )
  target_link_libraries(rio PUBLIC ${LIBR_LIBRARIES} PRIVATE AFNI::mri)
endif()

if(COMP_FUNSTUFF)
install(
    DIRECTORY
        ${CMAKE_CURRENT_SOURCE_DIR}/faces/
        ${CMAKE_CURRENT_SOURCE_DIR}/poems/
    DESTINATION ${AFNI_INSTALL_FUNSTUFF_DIR})
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
