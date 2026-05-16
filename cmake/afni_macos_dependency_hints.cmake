# macOS dependency hints for the CMake build.
#
# This file is intentionally included after project().  Compiler, SDK,
# architecture, and deployment-target choices belong in a toolchain file or in
# explicit CMake cache variables supplied by the user.  This module only adds
# dependency search hints that need compiler/platform information.
#
# Current responsibilities:
#   * expose AFNI_IS_MACOS and AFNI_IS_ARM for target-specific build logic
#   * prefer the active Homebrew prefix when resolving dependencies on macOS
#   * find Homebrew Mesa and mesa-glu prefixes for Apple Silicon SUMA builds
#   * provide AppleClang OpenMP hints without overriding an explicit user cache

set(AFNI_IS_MACOS FALSE)
set(AFNI_IS_ARM FALSE)
set(AFNI_BREW_PREFIX "")

function(afni_check_macos_dylib_deployment_target dylib_path)
  if(NOT APPLE OR NOT CMAKE_OSX_DEPLOYMENT_TARGET OR NOT EXISTS "${dylib_path}")
    return()
  endif()

  execute_process(
    COMMAND otool -l "${dylib_path}"
    OUTPUT_VARIABLE _afni_otool_output
    RESULT_VARIABLE _afni_otool_result
    ERROR_QUIET
  )
  if(NOT _afni_otool_result EQUAL 0)
    return()
  endif()

  string(
    REGEX MATCH
    "minos[ \t]+([0-9]+(\\.[0-9]+)*)"
    _afni_minos_match
    "${_afni_otool_output}"
  )
  if(NOT _afni_minos_match)
    string(
      REGEX MATCH
      "LC_VERSION_MIN_MACOSX[\r\n\t ]+cmdsize[^\r\n]*[\r\n\t ]+version[ \t]+([0-9]+(\\.[0-9]+)*)"
      _afni_minos_match
      "${_afni_otool_output}"
    )
  endif()

  if(_afni_minos_match)
    set(_afni_dylib_minos "${CMAKE_MATCH_1}")
    if(CMAKE_OSX_DEPLOYMENT_TARGET VERSION_LESS "${_afni_dylib_minos}")
      message(FATAL_ERROR
        "Deployment target mismatch for '${dylib_path}'. The library was built "
        "for macOS ${_afni_dylib_minos} or newer, but "
        "CMAKE_OSX_DEPLOYMENT_TARGET is ${CMAKE_OSX_DEPLOYMENT_TARGET}. "
        "Use a deployment target of at least ${_afni_dylib_minos}, install a "
        "library built for an older macOS version, or point CMake at a different "
        "dependency prefix."
      )
    endif()
  endif()
endfunction()

if(NOT APPLE)
  return()
endif()

set(AFNI_IS_MACOS TRUE)

set(_afni_macos_arches "${CMAKE_OSX_ARCHITECTURES}")
if(NOT _afni_macos_arches)
  set(_afni_macos_arches "${CMAKE_SYSTEM_PROCESSOR}")
endif()

if(_afni_macos_arches MATCHES "(^|;)arm64($|;)" OR
   _afni_macos_arches MATCHES "(^|;)aarch64($|;)")
  set(AFNI_IS_ARM TRUE)
endif()

set(
  AFNI_HOMEBREW_PREFIX
  ""
  CACHE PATH
  "Homebrew prefix used as a dependency search hint on macOS. Leave empty to ask brew."
)

find_program(AFNI_BREW_EXECUTABLE brew)
if(NOT AFNI_HOMEBREW_PREFIX AND AFNI_BREW_EXECUTABLE)
  execute_process(
    COMMAND "${AFNI_BREW_EXECUTABLE}" --prefix
    OUTPUT_VARIABLE _afni_brew_prefix
    RESULT_VARIABLE _afni_brew_result
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(_afni_brew_result EQUAL 0 AND IS_DIRECTORY "${_afni_brew_prefix}")
    set(
      AFNI_HOMEBREW_PREFIX
      "${_afni_brew_prefix}"
      CACHE PATH
      "Homebrew prefix used as a dependency search hint on macOS. Leave empty to ask brew."
      FORCE
    )
  endif()
endif()

if(AFNI_HOMEBREW_PREFIX)
  set(AFNI_BREW_PREFIX "${AFNI_HOMEBREW_PREFIX}")
  list(PREPEND CMAKE_PREFIX_PATH "${AFNI_HOMEBREW_PREFIX}")
  message(STATUS "macOS Homebrew dependency prefix: ${AFNI_HOMEBREW_PREFIX}")
endif()

set(AFNI_MESA_ROOT "" CACHE PATH "Mesa prefix to use for SUMA GL on macOS")
set(AFNI_GLU_ROOT "" CACHE PATH "GLU prefix to use for SUMA on macOS")
set(AFNI_MESA_INCLUDE_DIR "" CACHE PATH "Mesa include directory used with AFNI_MESA_ROOT")

function(_afni_brew_formula_prefix formula out_var description)
  if(${out_var} OR NOT AFNI_BREW_EXECUTABLE)
    return()
  endif()

  execute_process(
    COMMAND "${AFNI_BREW_EXECUTABLE}" --prefix "${formula}"
    OUTPUT_VARIABLE _afni_formula_prefix
    RESULT_VARIABLE _afni_formula_result
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )

  if(_afni_formula_result EQUAL 0 AND IS_DIRECTORY "${_afni_formula_prefix}")
    set("${out_var}" "${_afni_formula_prefix}" CACHE PATH "${description}" FORCE)
  endif()
endfunction()

if(COMP_SUMA AND AFNI_IS_ARM)
  _afni_brew_formula_prefix(
    mesa
    AFNI_MESA_ROOT
    "Mesa prefix to use for SUMA GL on macOS"
  )
  _afni_brew_formula_prefix(
    mesa-glu
    AFNI_GLU_ROOT
    "GLU prefix to use for SUMA on macOS"
  )

  if(AFNI_MESA_ROOT AND EXISTS "${AFNI_MESA_ROOT}/include/GL/gl.h")
    set(AFNI_MESA_INCLUDE_DIR "${AFNI_MESA_ROOT}/include" CACHE PATH "" FORCE)
  endif()
endif()

if(CMAKE_C_COMPILER_ID STREQUAL "AppleClang" AND AFNI_HOMEBREW_PREFIX)
  set(_afni_libomp_root "${AFNI_HOMEBREW_PREFIX}/opt/libomp")
  if(EXISTS "${_afni_libomp_root}")
    list(PREPEND CMAKE_PREFIX_PATH "${_afni_libomp_root}")
    if(NOT DEFINED OpenMP_C_FLAGS)
      set(
        OpenMP_C_FLAGS
        "-Xpreprocessor -fopenmp -I${_afni_libomp_root}/include"
        CACHE STRING
        "OpenMP C flags for AppleClang with Homebrew libomp"
      )
    endif()
    if(NOT DEFINED OpenMP_C_LIB_NAMES)
      set(OpenMP_C_LIB_NAMES "omp" CACHE STRING "OpenMP C library names")
    endif()
    if(NOT DEFINED OpenMP_omp_LIBRARY)
      set(
        OpenMP_omp_LIBRARY
        "${_afni_libomp_root}/lib/libomp.dylib"
        CACHE FILEPATH
        "Homebrew libomp library"
      )
    endif()
  endif()
endif()

unset(_afni_brew_prefix)
unset(_afni_brew_result)
unset(_afni_libomp_root)
unset(_afni_macos_arches)
