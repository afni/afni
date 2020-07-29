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
  if(NOT DEFINED USE_SYSTEM_${upper})
    option(USE_SYSTEM_${upper} "use system ${name} instead of bundled" ON)
  endif()  
  if(USE_SYSTEM_${upper})
    find_package(${name} REQUIRED)
  else(USE_SYSTEM_${upper})
    add_subdirectory(${subdir_path})
  endif(USE_SYSTEM_${upper})
endmacro()

function(filter_for_components mapping components  targets out_var)
  # get all targets associated with the components. Append all targets
  # to the comp_targs list and return it.
  set(comp_targs "")
  foreach(component ${components})
    set(temp_mapping ${mapping})
    list(FILTER temp_mapping INCLUDE REGEX ", ${component}$")
    list(APPEND comp_targs ${temp_mapping})
  endforeach()
  list(TRANSFORM comp_targs REPLACE ", .*" "" )

  # Set input variable to the filtered list
  set(${out_var} "${comp_targs}" PARENT_SCOPE)
endfunction()

function(filter_out_components mapping components  targets out_var)
  # get all targets associated with the components

  filter_for_components("${mapping}" "${components}" "${targets}" comp_targs)
  # Filter out targets associated with components
  if(NOT ("" STREQUAL "${comp_targs}"))
    # message(" components that have been filtered for: ${comp_targs}")
    list(REMOVE_ITEM targets ${comp_targs})
  endif()
  # Set input variable to the filtered list
  set(${out_var} "${targets}" PARENT_SCOPE)
endfunction()
  
function(get_expected_target_list mapping targets_label)
  set(filtered_list "${mapping}")
  list(TRANSFORM filtered_list REPLACE ", .*" "" )
  if(COMP_CORELIBS_ONLY)
    # only corelibs will be installed
    filter_for_components("${mapping}" "corelibs" "${filtered_list}" filtered_list)
    set(${targets_label} "${filtered_list}" PARENT_SCOPE)
    return()
  endif()

  # Remove components that are largely scripts or external
  filter_out_components(
    "${mapping}"
    "python;tcsh;rstats;external_dependencies"
    "${filtered_list}"
    filtered_list
    )
  # message(" filtered_list: ${filtered_list}")

  if(NOT (COMP_ADD_BINARIES))
    filter_out_components( "${mapping}" "corebinaries" "${filtered_list}" filtered_list)
  endif()


  if(NOT (COMP_X_DEPENDENT_GUI_PROGS))
    filter_out_components( "${mapping}" "gui" "${filtered_list}" filtered_list)
  endif()

  if(NOT (COMP_OPENGL_DEPENDENT_GUI_PROGS))
    filter_out_components( "${mapping}" "suma" "${filtered_list}" filtered_list)
  endif()

  if(NOT (COMP_ADD_PLUGINS))
    filter_out_components( "${mapping}" "plugins" "${filtered_list}" filtered_list)
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
  get_property(afni_installed_targets GLOBAL PROPERTY INSTALLED_PROGS)
  # message("Installed:${afni_installed_targets}")
  list(REMOVE_ITEM expected_targets ${afni_installed_targets})
  if(NOT "${expected_targets}" STREQUAL "")
  if(NOT REMOVE_BUILD_PARITY_CHECKS)
    message(FATAL_ERROR "The build has not built all the targets expected. It is\
     missing the following targets:${expected_targets}!!!!")
  endif()
  endif()
  
  # ##### assessing parity with the make build
  set(make_targ_list "")
  append_make_component("plugins" make_targ_list "${make_targ_list}")
  append_make_component("models" make_targ_list "${make_targ_list}")
  append_make_component("afni_progs" make_targ_list "${make_targ_list}")
  append_make_component("libraries" make_targ_list "${make_targ_list}")
  append_make_component("suma_progs" make_targ_list "${make_targ_list}")
  filter_out_components(
      "${CMPNT_MAPPING}"
      "external_dependencies"
      "${make_targ_list}"
      make_targ_list
      )


  # Compute targets not present in the make build
  set(CMAKE_LESS_MAKE_BUILT ${afni_installed_targets})
  list(REMOVE_ITEM CMAKE_LESS_MAKE_BUILT ${make_targ_list})

  # Compute targets only in the make build
  set(MAKE_BUILD_LESS_CMAKE ${make_targ_list})
  list(REMOVE_ITEM MAKE_BUILD_LESS_CMAKE ${afni_installed_targets})


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
    if(REMOVE_BUILD_PARITY_CHECKS)
      return()
    endif()
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
  target_link_options(${target_in}
  PRIVATE
  $<$<C_COMPILER_ID:AppleClang>:LINKER:-undefined,error>
  $<$<C_COMPILER_ID:Clang>:LINKER:-undefined,error>
  $<$<C_COMPILER_ID:GNU>:LINKER:--as-needed>
  $<$<C_COMPILER_ID:GNU>:LINKER:--no-undefined>
  )
  add_library(AFNI::${target_in} ALIAS ${target_in})
  add_afni_target_properties(${target_in})
endfunction()

function(add_afni_executable target_in)
  add_executable(${ARGV})
  target_link_options(${target_in}
  PRIVATE 
  $<$<C_COMPILER_ID:AppleClang>:LINKER:-undefined,error>
  $<$<C_COMPILER_ID:Clang>:LINKER:-undefined,error>
  $<$<C_COMPILER_ID:GNU>:LINKER:--no-undefined>
    )
  target_link_options(${target_in}
  PRIVATE 
  $<$<C_COMPILER_ID:GNU>:LINKER:--as-needed>
    )
  add_afni_target_properties(${target_in})
endfunction()

function(add_afni_plugin target_in)
  set(CMAKE_C_FLAGS_DEBUG
    "${CMAKE_C_FLAGS_DEBUG} -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG"
)
  add_library(${ARGV})
  add_afni_target_properties(${target_in})
  target_link_options(${target_in}
  PRIVATE
  $<$<C_COMPILER_ID:AppleClang>:LINKER:-undefined,dynamic_lookup>
  $<$<C_COMPILER_ID:Clang>:LINKER:-undefined,dynamic_lookup>
    )
  if(RUN_PLUGIN_CHECK)
    add_library(checking_${target_in} $<TARGET_PROPERTY:${target_in},SOURCES>)
    # The following links against all dependencies of plugins so that all
    # symbols are resolved at link time. It serves as a build timen check for
    # missing symbols in the plugins (as in symbols not provided by afni at
    # runtime).
    target_link_libraries(
        checking_${target_in}
        PUBLIC
        afni_all_objects
        mrix
        whats_my_exepath
        NIFTI::nifti2
        NIFTI::nifticdf
      )
    target_link_options(
      checking_${target_in}
      PRIVATE
      $<$<C_COMPILER_ID:AppleClang>:LINKER:-undefined,error>
      $<$<C_COMPILER_ID:Clang>:LINKER:-undefined,error>
      $<$<C_COMPILER_ID:GNU>:LINKER:--no-undefined>
      )
  endif()
endfunction()

function(add_afni_target_properties target)
  # this macro sets some default properties for targets in this project
  get_target_property(TARGET_TYPE ${target} TYPE)
  if(NOT (GENERATE_PACKAGING_COMPONENTS))
    get_component_name(component "${CMPNT_MAPPING}" ${target})
  endif()
  log_target_as_installed(${target})
  # message("${target} -------${component}")

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

function(check_suma_binary)
add_custom_command(TARGET suma
                     COMMAND bash ${CMAKE_SOURCE_DIR}/cmake/check_suma_binary_linking.sh $<TARGET_FILE:suma>
                     WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
                     COMMENT check that the suma binary linking has not run into the motif/xt issue
                     USES_TERMINAL
                     )
endfunction()
