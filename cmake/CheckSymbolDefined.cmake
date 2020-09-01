# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
CheckSymbolDefined
-----------------

Provides a macro to check if a symbol is not NULL. 

.. command:: check_symbol_defined

  .. code-block:: cmake

    check_symbol_defined(<symbol> <files> <val_defined>)

  Check that the ``<symbol>`` is available and defined after including given
  header ``<files>`` and store the result in a ``<val_defined>``.  Specify the
  list of files in one argument as a semicolon-separated list. ``<val_defined>``
  will be created as an internal cache variable. This code is adapted from the
  cmake code for CheckSymbolExists. Check that documentation for some more
  potentially relevant information.

The following variables may be set before calling this macro to modify
the way the check is run:

``CMAKE_REQUIRED_FLAGS``
  string of compile command line flags.
``CMAKE_REQUIRED_DEFINITIONS``
  a :ref:`;-list <CMake Language Lists>` of macros to define (-DFOO=bar).
``CMAKE_REQUIRED_INCLUDES``
  a :ref:`;-list <CMake Language Lists>` of header search paths to pass to
  the compiler.
``CMAKE_REQUIRED_LINK_OPTIONS``
  a :ref:`;-list <CMake Language Lists>` of options to add to the link command.
``CMAKE_REQUIRED_LIBRARIES``
  a :ref:`;-list <CMake Language Lists>` of libraries to add to the link
  command. See policy :policy:`CMP0075`.
``CMAKE_REQUIRED_QUIET``
  execute quietly without messages.

For example:

.. code-block:: cmake

  include(CheckSymbolDefined)

  # Check for macro SEEK_SET
  check_symbol_defined(SEEK_SET "stdio.h" HAVE_SEEK_SET)
  # Check for function fopen
  check_symbol_defined(fopen "stdio.h" HAVE_FOPEN)
#]=======================================================================]

include_guard(GLOBAL)
cmake_policy(PUSH)
cmake_policy(SET CMP0054 NEW) # if() quoted variables not dereferenced

function(check_execution STDERR_VAR BIN_EXIT_CODE)

    # Check that binary executes successfully
    execute_process(
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/test_GLw_symbol
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    RESULT_VARIABLE resultVar
    ERROR_VARIABLE errorVar
    )
    message("Exit status for execution of test binary:  ${resultVar}")
    set(${BIN_EXIT_CODE} ${resultVar} PARENT_SCOPE)
    set(${STDERR_VAR} ${errorVar} PARENT_SCOPE)
endfunction()

macro(CHECK_SYMBOL_DEFINED SYMBOL FILES VAL_DEFINED)
  if(CMAKE_C_COMPILER_LOADED)
    __CHECK_SYMBOL_DEFINED_IMPL("${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/CheckSymbolDefined.c" "${SYMBOL}" "${FILES}" "${VAL_DEFINED}" )
  elseif(CMAKE_CXX_COMPILER_LOADED)
    __CHECK_SYMBOL_DEFINED_IMPL("${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/CheckSymbolDefined.cxx" "${SYMBOL}" "${FILES}" "${VAL_DEFINED}" )
  else()
    message(FATAL_ERROR "CHECK_SYMBOL_DEFINED needs either C or CXX language enabled")
  endif()
endmacro()

function(__CHECK_SYMBOL_DEFINED_IMPL SOURCEFILE SYMBOL FILES VAL_DEFINED)
  if(NOT DEFINED "${VAL_DEFINED}" OR "x${${VAL_DEFINED}}" STREQUAL "x${VAL_DEFINED}")
    set(CMAKE_CONFIGURABLE_FILE_CONTENT "/* */\n")
    set(MACRO_CHECK_SYMBOL_DEFINED_FLAGS ${CMAKE_REQUIRED_FLAGS})
    if(CMAKE_REQUIRED_LINK_OPTIONS)
      set(CHECK_SYMBOL_DEFINED_LINK_OPTIONS
        LINK_OPTIONS ${CMAKE_REQUIRED_LINK_OPTIONS})
    else()
      set(CHECK_SYMBOL_DEFINED_LINK_OPTIONS)
    endif()
    if(CMAKE_REQUIRED_LIBRARIES)
      set(CHECK_SYMBOL_DEFINED_LIBS
        LINK_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    else()
      set(CHECK_SYMBOL_DEFINED_LIBS)
    endif()
    if(CMAKE_REQUIRED_INCLUDES)
      set(CMAKE_SYMBOL_EXISTS_INCLUDES
        "-DINCLUDE_DIRECTORIES:STRING=${CMAKE_REQUIRED_INCLUDES}")
    else()
      set(CMAKE_SYMBOL_EXISTS_INCLUDES)
    endif()
    string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT "#include <stdio.h>\n")
    string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT "#include <stdlib.h>\n")
    foreach(FILE ${FILES})
      string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT
        "#include <${FILE}>\n")
    endforeach()
    string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT "
int main(int argc, char** argv)
{
  
  (void)argv;")
    set(_CSE_CHECK_NON_MACRO "return ((int*)(&${SYMBOL}))[argc];")
    if("${SYMBOL}" MATCHES "^[a-zA-Z_][a-zA-Z0-9_]*$")
      # The SYMBOL has a legal macro name.  Test whether it exists as a macro.
      string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT "
if( ${SYMBOL} == NULL ) {

  fprintf(stderr,\"${SYMBOL} is not defined in header file. Check the header files ${FILES}\");
  exit(1);
  ${_CSE_CHECK_NON_MACRO}
}
")
    else()
      # The SYMBOL cannot be a macro (e.g., a template function).
      string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT "
  ${_CSE_CHECK_NON_MACRO}")
    endif()
    string(APPEND CMAKE_CONFIGURABLE_FILE_CONTENT "
}")
    unset(_CSE_CHECK_NON_MACRO)

    configure_file("${CMAKE_ROOT}/Modules/CMakeConfigurableFile.in"
      "${SOURCEFILE}" @ONLY)

    if(NOT CMAKE_REQUIRED_QUIET)
      message("Looking for ${SYMBOL} in ${SOURCEFILE}:including ${CMAKE_SYMBOL_EXISTS_INCLUDES}")
    endif()
    try_compile(COMPILE_SUCCESS
      ${CMAKE_BINARY_DIR}
      "${SOURCEFILE}"
      COPY_FILE ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/test_GLw_symbol
      COMPILE_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS}
      ${CHECK_SYMBOL_DEFINED_LINK_OPTIONS}
      ${CHECK_SYMBOL_DEFINED_LIBS}
      CMAKE_FLAGS
      -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_SYMBOL_DEFINED_FLAGS}
      "${CMAKE_SYMBOL_EXISTS_INCLUDES}"
      OUTPUT_VARIABLE OUTPUT)

    # Log compilation success     
    if(COMPILE_SUCCESS)
      if(NOT CMAKE_REQUIRED_QUIET)
        message("Test binary compiled.")
      endif()
      
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Test compilation succeeded with the following output:\n"
        "${OUTPUT}\nFile ${SOURCEFILE}:\n"
        "${CMAKE_CONFIGURABLE_FILE_CONTENT}\n")
    else()
      if(NOT CMAKE_REQUIRED_QUIET)
        message(CHECK_FAIL "The test binary failed to compile")
      endif()
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Test binary compilation failed with the following output:\n"
        "${OUTPUT}\nFile ${SOURCEFILE}:\n"
        "${CMAKE_CONFIGURABLE_FILE_CONTENT}\n")
      return()
    endif()

    # Check executable
    check_execution(EXEC_ERROR EXIT_STATUS)

    # Log compilation success     
    if(EXIT_STATUS )
      # execution failed...
      if(NOT CMAKE_REQUIRED_QUIET)
        message("The test binary does not have ${SYMBOL} defined")
      endif()
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Test binary execution had a non-zero exit status which may indicate a missing symbol. The output was:\n"
        "${EXEC_ERROR}\nFile ${SOURCEFILE}:\n"
        "${CMAKE_CONFIGURABLE_FILE_CONTENT}\n")
      set(${VAL_DEFINED} 0 CACHE INTERNAL "Symbol ${SYMBOL} undefined")
    else()
      # execution succeeded
      if(NOT CMAKE_REQUIRED_QUIET)
        message("Test binary executed successfully.")
      endif()
      
      set(${VAL_DEFINED} 1 CACHE INTERNAL "Symbol ${SYMBOL} defined")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Test compilation succeeded with the following output:\n"
        "${OUTPUT}\nFile ${SOURCEFILE}:\n"
        "${CMAKE_CONFIGURABLE_FILE_CONTENT}\n")
    endif()
  endif()
    unset(CMAKE_CONFIGURABLE_FILE_CONTENT)
  
endfunction()

cmake_policy(POP)