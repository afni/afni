macro(optional_bundle name)
  string(TOUPPER ${name} upper)

  option(USE_SYSTEM_${upper} "use system ${name} instead of bundled")
  if(USE_SYSTEM_${upper})
    find_package(${name} REQUIRED)
  else(USE_SYSTEM_${upper})
    add_subdirectory(${name})
  endif(USE_SYSTEM_${upper})
endmacro(optional_bundle)

function(assemble_target_list PROGRAMS_BUILT SHOW_UNBUILT_PROGS)
  # ##### Read list of makefile programs that are built by this project.
  execute_process(
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND bash "-c" "make -s -f Makefile.INCLUDE prog_list_bin"
  )

  file(READ ${CMAKE_CURRENT_SOURCE_DIR}/prog_list_bin.txt ALL_PROGRAMS)
  string(STRIP ${ALL_PROGRAMS} ALL_PROGRAMS)
  string(REPLACE "\n" " " ALL_PROGRAMS ${ALL_PROGRAMS})
  separate_arguments(ALL_PROGRAMS)
  set(PROGRAMS_BUILT "")
  set(NOT_BUILT "")
  set(installable_targets ${ALL_PROGRAMS})

  # remove some programs that should not be here
  foreach(other_project_target gifti_tool giftiio gifti_test nifti_tool niftiio)
    # message("Removing ${other_project_target} because it is not built by this
    # project")
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

  if(SHOW_UNBUILT_PROGS)
    message("programs not built: '${NOT_BUILT}'")
    message("programs built: '${PROGRAMS_BUILT}'")
  endif()
endfunction()

macro(set_os_flags src_file)
  # sets os specific compile definintions for source files. This is currently
  # superfluous because debugging without these global flags set has not been done
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    set_source_files_properties(
      ${src_file} PROPERTIES COMPILE_DEFINITIONS "LINUX;LINUX2"
    )
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    remove_definitions("-DLINUX")
    set_source_files_properties(
      ${src_file} PROPERTIES COMPILE_DEFINITIONS "LINUX;DARWIN"
    )
    message(STATUS "Configuring on/for macOS")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    message(FATAL_ERROR "Windows is not supported for this build")
  else()
    message(
      STATUS "Configuring on/for ${CMAKE_SYSTEM_NAME}. Not sure if this will work..."
    )
  endif()
endmacro()

function(quotize input basename)
  set(output "${basename}.h")
  add_custom_command(
    OUTPUT "${output}"
    COMMAND quotize "${basename}" < "${input}" > "${output}"
    MAIN_DEPENDENCY quotize
  )
endfunction(quotize)

macro(get_afni_rpath)
  # Needed for correct linking when installing. see
  # https://gitlab.kitware.com/cmake/community/wikis/doc/cmake/RPATH-handling
  # get_property(_nifti TARGET NIFTI::niftiio PROPERTY LOCATION)
  # get_filename_component(_nifti_libraries ${_nifti} DIRECTORY) get_property(_gifti
  # TARGET GIFTI::giftiio PROPERTY LOCATION) get_filename_component(_gifti_libraries
  # ${_gifti} DIRECTORY)
  if(TARGET_TYPE MATCHES "LIBRARY")
    set(INSTALL_PATH ${AFNI_INSTALL_LIBRARY_DIR})
  else()
    set(INSTALL_PATH ${AFNI_INSTALL_RUNTIME_DIR})
  endif()
  # Prepare RPATH
  file(RELATIVE_PATH _rel_afni ${INSTALL_PATH} ${AFNI_INSTALL_LIBRARY_DIR})
  # file(RELATIVE_PATH _rel_nifti ${INSTALL_PATH} ${_nifti_libraries})
  # file(RELATIVE_PATH _rel_gifti ${INSTALL_PATH} ${_gifti_libraries})
  if(APPLE)
    set(afni_target_RPATH
        "@loader_path/${_rel_afni}"

        # "@loader_path/${_rel_nifti}" "@loader_path/${_rel_gifti}"
    )
  else()
    set(afni_target_RPATH
        "\$ORIGIN/${_rel_afni}"
        # "\$ORIGIN/${_rel_nifti}" "\$ORIGIN/${_rel_gifti}"
    )
  endif()
  # file(TO_NATIVE_PATH "${_rpath}" afni_target_RPATH)
endmacro()

macro(add_afni_target_properties target)
  # this macro sets some default properties for targets in this project
  get_target_property(TARGET_TYPE ${target} TYPE)
  get_afni_rpath()

  # Set the target properties
  if(NOT DEFINED ENV{CONDA_BUILD})
    set_target_properties(
      ${target}
      PROPERTIES SKIP_BUILD_RPATH
                 OFF
                 BUILD_WITH_INSTALL_RPATH
                 OFF
                 INSTALL_RPATH
                 "${afni_target_RPATH}"
                 INSTALL_RPATH_USE_LINK_PATH
                 ON
    )
  endif()
  set_target_properties(${target} PROPERTIES MACOSX_RPATH ON)

  if(CONDA_BUILD)
    set_target_properties(${target} PROPERTIES INSTALL_RPATH "${afni_target_RPATH}")
  endif()

  install(
  TARGETS ${target}
  RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
  LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
  PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
)

  # INSTALL_RPATH_USE_LINK_PATH ON SKIP_BUILD_RPATH OFF BUILD_WITH_INSTALL_RPATH OFF
endmacro()

function(check_header_has_been_created HEADER_PATH)
  if(NOT EXISTS "${HEADER_PATH}")
    file(
      WRITE ${HEADER_PATH}
      "\
    #define AFNI_VERSION_LABEL    \"AFNI_${GIT_REPO_VERSION}\"
    #define AFNI_VERSION_PLATFORM \"${CMAKE_SYSTEM_NAME}_cmake\" \
    "
    )
  endif()
endfunction()

macro(set_if_not_defined var defaultvalue)
  # Macro allowing to set a variable to its default value if not already defined. The
  # default value is set with: (1) if set, the value environment variable <var>. (2) if
  # set, the value of local variable variable <var>. (3) if none of the above, the value
  # passed as a parameter. Setting the optional parameter 'OBFUSCATE' will display
  # 'OBFUSCATED' instead of the real value.
  set(_obfuscate FALSE)
  foreach(arg ${ARGN})
    if(arg STREQUAL "OBFUSCATE")
      set(_obfuscate TRUE)
    endif()
  endforeach()
  if(DEFINED ENV{${var}} AND NOT DEFINED ${var})
    set(_value "$ENV{${var}}")
    if(_obfuscate)
      set(_value "OBFUSCATED")
    endif()
    message(
      STATUS "Setting '${var}' variable with environment variable value '${_value}'"
    )
    set(${var} $ENV{${var}})
  endif()
  if(NOT DEFINED ${var})
    set(_value "${defaultvalue}")
    if(_obfuscate)
      set(_value "OBFUSCATED")
    endif()
    message(STATUS "Setting '${var}' variable with default value '${_value}'")
    set(${var} "${defaultvalue}")
  endif()
endmacro()
