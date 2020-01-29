# Macro to turn a list into a string (why doesn't CMake have this built-in?)
macro(list_TO_STRING _string _list)
  set(${_string})
  foreach(_item ${_list})
    set(${_string} "${${_string}} ${_item}")
  endforeach(_item)
endmacro(list_TO_STRING)

macro(optional_bundle subdir_path)
  get_filename_component(name ${subdir_path} NAME)
  # this must be a macro for scoping reasons. Otherwise LIB_FOUND is not set
  string(TOUPPER ${name} upper)

  option(USE_SYSTEM_${upper} "use system ${name} instead of bundled" ON)
  if(USE_SYSTEM_${upper})
    find_package(${name} REQUIRED)
  else(USE_SYSTEM_${upper})
    add_subdirectory(${subdir_path})
  endif(USE_SYSTEM_${upper})
endmacro()

function(filter_out_components mapping components  targets out_var)
  # get all targets associated with the components
  set(comp_targs "")
  foreach(component ${components})
    set(temp_mapping ${mapping})
    list(FILTER temp_mapping INCLUDE REGEX ", ${component}$")
    list(APPEND comp_targs ${temp_mapping})
  endforeach()
  list(TRANSFORM comp_targs REPLACE ", .*" "" )

  # Filter out targets associated with components
  list(REMOVE_ITEM targets ${comp_targs})
  # message(" targets: ${targets}")
  

  # Set input variable to the filtered list
  set(${out_var} "${targets}" PARENT_SCOPE)
endfunction()
  
function(get_expected_target_list mapping targets_label)
  set(filtered_list "${mapping}")
  list(TRANSFORM filtered_list REPLACE ", .*" "" )
  if(AFNI_BUILD_CORELIBS_ONLY)
    # only corelibs will be installed
    filter_for_components(mapping "corelibs" "${filtered_list}" filtered_list)
  endif()

  # Remove components that are largely scripts or external
  filter_out_components(
    "{${mapping}}"
    "python;tcsh;rstats;external_dependencies"
    "${filtered_list}"
    filtered_list
    )

  if(NOT (BUILD_BINARIES))
    filter_out_components( mapping "corebinaries" "${filtered_list}" filtered_list)
  endif()


  if(NOT (BUILD_X_DEPENDENT_GUI_PROGS))
    filter_out_components( mapping "gui" "${filtered_list}" filtered_list)
  endif()

  if(NOT (BUILD_OPENGL_DEPENDENT_GUI_PROGS))
    filter_out_components( mapping "suma" "${filtered_list}" filtered_list)
  endif()

  set(${targets_label} "${filtered_list}" PARENT_SCOPE)
endfunction()

function(append_make_component component varlabel var)
  set(out_list ${var})

  execute_process(
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      COMMAND bash "-c" "make -sf Makefile.INCLUDE list_${component}"
      OUTPUT_VARIABLE comp_list
      RESULT_VARIABLE status
    )
  if(NOT status STREQUAL "0")
    message(FATAL_ERROR "Failed attempting to get the component \
      '${component}' from the make build")
  endif()
  # message(${comp_list})
  # string(STRIP ${comp_list} comp_list)
  # string(REGEX REPLACE "\n" ";" comp_list ${comp_list})
  string(REGEX REPLACE "\n" ";" comp_list ${comp_list})
  # Filter out lib prefixes and library extensions
  list(
    TRANSFORM comp_list 
    REPLACE "lib(.*)(.so|.a)$" "\\1"
    REGEX "lib.*(.so|.a)")
  # Filter out library extension stubs
  list(
    TRANSFORM comp_list 
    REPLACE "\.$" ""
    REGEX ".*[.]$")
  list(APPEND out_list ${comp_list})
  set(${varlabel} ${out_list} PARENT_SCOPE)
endfunction()

function(assemble_target_list PROGRAMS_BUILT SHOW_UNBUILT_PROGS)
  
  # Filter expected targets based on build configuration
  get_expected_target_list("${CMPNT_MAPPING}" expected_targets)

  # Get list of installed targets from this project build...
  get_property(installed_targets GLOBAL PROPERTY INSTALLED_PROGS)
  # message("Installed:${installed_targets}")
  list(REMOVE_ITEM expected_targets ${installed_targets})
  if(NOT "${expected_targets}" STREQUAL "")
    message(FATAL_ERROR "The build has not built all the targets expected. It is\
     missing the following targets:${expected_targets}!!!!")
  endif()
  
  # ##### assessing parity with the make build
  set(make_targ_list "")
  append_make_component("plugins" make_targ_list "${make_targ_list}")
  append_make_component("models" make_targ_list "${make_targ_list}")
  append_make_component("afni_progs" make_targ_list "${make_targ_list}")
  append_make_component("libraries" make_targ_list "${make_targ_list}")
  append_make_component("suma_progs" make_targ_list "${make_targ_list}")
  filter_out_components(
      "{${CMPNT_MAPPING}}"
      "external_dependencies"
      "${make_targ_list}"
      make_targ_list
      )


  # Compute targets not present in the make build
  set(CMAKE_LESS_MAKE_BUILT ${installed_targets})
  list(REMOVE_ITEM CMAKE_LESS_MAKE_BUILT ${make_targ_list})

  # Compute targets only in the make build
  set(MAKE_BUILD_LESS_CMAKE ${make_targ_list})
  list(REMOVE_ITEM MAKE_BUILD_LESS_CMAKE ${installed_targets})


  # Provide feedback on difference between the builds
  message("Comparison with build using the make system:")
  message("programs not built: '${MAKE_BUILD_LESS_CMAKE}'")
  message("extra programs built: '${CMAKE_LESS_MAKE_BUILT}'")
  
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
  # message(FATAL_ERROR "${afni_target_RPATH}----- ${AFNI_INSTALL_LIBRARY_DIR}--- ${_rel_afni}------${INSTALL_PATH} -------${TARGET_TYPE}")
endmacro()

function(get_component_name component cmpnt_mapping targ_in)
  set(${component} "not found" PARENT_SCOPE)
  # Escape "+" character for regex
  get_target_regex(${targ_in} regex_pat)
  # Filter for current target
  list(FILTER cmpnt_mapping INCLUDE REGEX "^${regex_pat},")

  # Extract component for target
  string(REGEX REPLACE "[@a-zA-Z0-9_.+]*, " "" output "${cmpnt_mapping}")
  # Raise an error if the target is not in the list of project targets
  if(output)
    set(${component} "${output}" PARENT_SCOPE)
  else()
    message(FATAL_ERROR 
      "########################################################################## \
      ERROR: Installation component not found for ${targ_in}. This is             \
      assessed by checking the target name against the saved list of              \
      target/component pairs in the packaging directory of the AFNI               \
      repository. This saved list can be modified manually or regenerated by      \
      executing the 'define_installation_components.py' script contained in that  \
      directory                                                                   \
      ##########################################################################  
      "
      )
   endif()
endfunction()

function(get_target_regex target pattern)
# convert target string to regex pattern
string(REGEX REPLACE "([+])" [[\\+]] with_subs "${target}")
set(${pattern} "${with_subs}" PARENT_SCOPE)
endfunction()

function(log_target_as_installed target)
  GET_PROPERTY(temp_list GLOBAL PROPERTY INSTALLED_PROGS)
  get_target_regex(${target} regex_pat)
  list(APPEND temp_list "${target}")
  SET_PROPERTY(GLOBAL PROPERTY INSTALLED_PROGS "${temp_list}")
endfunction()

function(add_afni_library target_in)
  add_library(${ARGV})
  add_library(AFNI::${target_in} ALIAS ${target_in})
  add_afni_target_properties(${target_in})
endfunction()

function(add_afni_executable target_in)
  add_executable(${ARGV})
  add_afni_target_properties(${target_in})
endfunction()

function(add_afni_target_properties target)
  # this macro sets some default properties for targets in this project
  get_target_property(TARGET_TYPE ${target} TYPE)
  get_afni_rpath()
  if(NOT (GENERATE_PACKAGING_COMPONENTS))
    get_component_name(component "${CMPNT_MAPPING}" ${target})
  endif()
  log_target_as_installed(${target})
  # message("${target} -------${component}")
  
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
    COMPONENT ${component}
    RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
    LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
    ARCHIVE DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
    PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
    # PRIVATE_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
  )
endfunction()

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
