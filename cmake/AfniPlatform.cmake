# =============================================================================
# AfniPlatform.cmake
# Platform detection, ARM build support, and Mesa path override for AFNI.
#
# Inspired by Makefile.macos_13_ARM_clang (drg, 2021).
#
# HOW TO USE
# ----------
# Include early in the top-level CMakeLists.txt, before any find_package calls:
#
#   include(cmake/AfniPlatform.cmake)
#
# Then reference the variables / imported targets it exports:
#
#   AFNI_ARCH            - "arm64" | "x86_64" | "linux_x86_64" | etc.
#   AFNI_IS_ARM          - TRUE when building for Apple Silicon / Linux ARM64
#   AFNI_IS_MACOS        - TRUE on Darwin
#   AFNI_BREW_PREFIX     - Homebrew prefix (ARM: /opt/homebrew, Intel: /usr/local)
#   AFNI_MESA_HINT       - directory passed to FindOpenGL / find_library for Mesa
#
# Cache variables (all overridable on the cmake command line):
#
#   AFNI_MESA_ROOT       - explicit Mesa install prefix.
#                          Set to bypass the automatic system search.
#                          Example: -DAFNI_MESA_ROOT=/opt/homebrew
#
#   AFNI_LOCAL_CC_PATH   - path to the C compiler to use (mirrors LOCAL_CC_PATH
#                          in the legacy Makefile).
#                          Example: -DAFNI_LOCAL_CC_PATH=/opt/homebrew/bin/gcc-13
#
#   AFNI_BUILD_ARM       - ON/OFF — force an ARM64 build on macOS regardless of
#                          what cmake auto-detects (useful for cross-compiling or
#                          when building under Rosetta).
# =============================================================================

cmake_minimum_required(VERSION 3.16)

# ---------------------------------------------------------------------------
# 1.  User-facing cache variables
# ---------------------------------------------------------------------------

# AFNI_MESA_ROOT and AFNI_GLU_ROOT are intentionally NOT put in the cache here.
# Section 4 auto-detects them via 'brew --prefix mesa' / 'brew --prefix mesa-glu'
# once AFNI_BREW_PREFIX is known, so macOS gets the correct path for whatever
# version is installed.  A -D override on the command line always wins because
# cmake processes -D values before include() scripts run, and set() without
# FORCE never overwrites an existing cache entry.
# On non-Mac systems they remain empty unless explicitly passed via -D.
if(NOT DEFINED AFNI_MESA_ROOT)
    set(AFNI_MESA_ROOT "")
endif()
if(NOT DEFINED AFNI_GLU_ROOT)
    set(AFNI_GLU_ROOT "")
endif()

set(AFNI_LOCAL_CC_PATH ""
    CACHE FILEPATH
    "Path to the C compiler (mirrors LOCAL_CC_PATH in the legacy ARM Makefile). \
Leave empty to use the system default.")

option(AFNI_BUILD_ARM
    "Force an arm64 build on macOS (useful when running under Rosetta or cross-compiling)"
    OFF)

# ---------------------------------------------------------------------------
# 2.  Platform / architecture detection
# ---------------------------------------------------------------------------

set(AFNI_IS_MACOS FALSE)
set(AFNI_IS_ARM   FALSE)
set(AFNI_ARCH     "unknown")

if(APPLE)
    set(AFNI_IS_MACOS TRUE)

    # Detect the *host* architecture.
    execute_process(
        COMMAND uname -m
        OUTPUT_VARIABLE _UNAME_M
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    if(_UNAME_M STREQUAL "arm64" OR AFNI_BUILD_ARM)
        set(AFNI_IS_ARM TRUE)
        set(AFNI_ARCH "arm64")
        message(STATUS "[AfniPlatform] macOS ARM64 build (Apple Silicon / M-series)")
    else()
        set(AFNI_ARCH "x86_64")
        message(STATUS "[AfniPlatform] macOS x86_64 build")
    endif()

elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    execute_process(
        COMMAND uname -m
        OUTPUT_VARIABLE _UNAME_M
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(_UNAME_M MATCHES "aarch64|arm64")
        set(AFNI_IS_ARM TRUE)
        set(AFNI_ARCH "linux_arm64")
        message(STATUS "[AfniPlatform] Linux ARM64 build")
    else()
        set(AFNI_ARCH "linux_x86_64")
        message(STATUS "[AfniPlatform] Linux x86_64 build")
    endif()
endif()

# ---------------------------------------------------------------------------
# 3.  macOS deployment target (keep in sync with existing CMake default)
# ---------------------------------------------------------------------------

if(AFNI_IS_MACOS)
    if(NOT CMAKE_OSX_DEPLOYMENT_TARGET)
        set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15" CACHE STRING
            "Minimum macOS deployment version" FORCE)
    endif()

    # On ARM we must target at least 11.0 (Big Sur).
    if(AFNI_IS_ARM AND CMAKE_OSX_DEPLOYMENT_TARGET VERSION_LESS "15.0")
        message(WARNING
            "[AfniPlatform] ARM Homebrew libraries require macOS 15.0+. "
            "Raising CMAKE_OSX_DEPLOYMENT_TARGET from "
            "${CMAKE_OSX_DEPLOYMENT_TARGET} to 15.0.")
        set(CMAKE_OSX_DEPLOYMENT_TARGET "15.0" CACHE STRING
            "Minimum macOS deployment version" FORCE)
    endif()

    # Set the universal / single-arch slice.
    if(AFNI_IS_ARM)
        set(CMAKE_OSX_ARCHITECTURES "arm64" CACHE STRING "Build architecture" FORCE)
    endif()
endif()

# ---------------------------------------------------------------------------
# 4.  Homebrew prefix
# ---------------------------------------------------------------------------

set(AFNI_BREW_PREFIX "")

if(AFNI_IS_MACOS)
    if(AFNI_IS_ARM)
        # ARM homebrew lives in /opt/homebrew
        if(EXISTS "/opt/homebrew")
            set(AFNI_BREW_PREFIX "/opt/homebrew")
        endif()
    else()
        # Intel homebrew lives in /usr/local
        if(EXISTS "/usr/local/Cellar")
            set(AFNI_BREW_PREFIX "/usr/local")
        endif()
    endif()

    if(AFNI_BREW_PREFIX)
        message(STATUS "[AfniPlatform] Homebrew prefix: ${AFNI_BREW_PREFIX}")
        # Prepend so cmake finds ARM-native libraries before Intel ones.
        list(PREPEND CMAKE_PREFIX_PATH "${AFNI_BREW_PREFIX}")

        # Derive Mesa/GLU defaults from Homebrew if the user has not provided
        # them via -D.  We only set the cache variable when brew --prefix
        # succeeds AND the path exists, so a missing formula is a soft failure
        # (warning) rather than a hard error.  set() without FORCE means a
        # -D override or an already-cached value from a previous run always wins.
        if(NOT AFNI_MESA_ROOT)
            execute_process(
                COMMAND brew --prefix mesa
                OUTPUT_VARIABLE _brew_mesa_prefix
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
            if(_brew_mesa_prefix AND EXISTS "${_brew_mesa_prefix}")
                set(AFNI_MESA_ROOT "${_brew_mesa_prefix}"
                    CACHE PATH
                    "Explicit Mesa install prefix (libs in <prefix>/lib, headers \
in <prefix>/include). Auto-detected from 'brew --prefix mesa'. \
Pass -DAFNI_MESA_ROOT=<path> to override.")
                message(STATUS "[AfniPlatform] Mesa default : ${_brew_mesa_prefix}")
            else()
                message(WARNING
                    "[AfniPlatform] 'brew --prefix mesa' failed or path does not exist. "
                    "Set -DAFNI_MESA_ROOT=<path> explicitly.")
            endif()
            unset(_brew_mesa_prefix)
        endif()

        if(NOT AFNI_GLU_ROOT)
            execute_process(
                COMMAND brew --prefix mesa-glu
                OUTPUT_VARIABLE _brew_glu_prefix
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
            if(_brew_glu_prefix AND EXISTS "${_brew_glu_prefix}")
                set(AFNI_GLU_ROOT "${_brew_glu_prefix}"
                    CACHE PATH
                    "Explicit libGLU install prefix (libs in <prefix>/lib, headers \
in <prefix>/include). Auto-detected from 'brew --prefix mesa-glu'. \
Pass -DAFNI_GLU_ROOT=<path> to override.")
                message(STATUS "[AfniPlatform] GLU default  : ${_brew_glu_prefix}")
            else()
                message(WARNING
                    "[AfniPlatform] 'brew --prefix mesa-glu' failed or path does not exist. "
                    "Set -DAFNI_GLU_ROOT=<path> explicitly.")
            endif()
            unset(_brew_glu_prefix)
        endif()
    endif()
endif()

# ---------------------------------------------------------------------------
# 5.  Compiler selection (mirrors LOCAL_CC_PATH / CCMIN in the Makefile)
# ---------------------------------------------------------------------------

if(AFNI_LOCAL_CC_PATH AND NOT CMAKE_C_COMPILER)
    if(EXISTS "${AFNI_LOCAL_CC_PATH}")
        set(CMAKE_C_COMPILER "${AFNI_LOCAL_CC_PATH}" CACHE FILEPATH
            "C compiler" FORCE)
        message(STATUS "[AfniPlatform] Using compiler: ${AFNI_LOCAL_CC_PATH}")
    else()
        message(WARNING
            "[AfniPlatform] AFNI_LOCAL_CC_PATH='${AFNI_LOCAL_CC_PATH}' does not exist. "
            "Falling back to system default.")
    endif()
endif()

# ---------------------------------------------------------------------------
# 6.  ARM compile / link flags
# ---------------------------------------------------------------------------

if(AFNI_IS_MACOS AND AFNI_IS_ARM)
    # Equivalent of -arch arm64 -DDARWIN -DARM_M1 in the Makefile.
    add_compile_options(-arch arm64)
    add_link_options(-arch arm64)
    add_compile_definitions(DARWIN ARM_M1)
    message(STATUS "[AfniPlatform] Added -arch arm64, -DDARWIN, -DARM_M1")
endif()

# ---------------------------------------------------------------------------
# 7.  Mesa override / discovery
# ---------------------------------------------------------------------------
#
# Strategy
# --------
# a) If AFNI_MESA_ROOT is set (either via -D override or the macOS default from
#    section 4), inject it at the front of every relevant search path, run lib
#    and header discovery, and disable the system OpenGL/GLU so CMake won't
#    accidentally pick up /System/Library/Frameworks on macOS.
# b) Otherwise (non-Mac with no -D override) let the existing find_package(OpenGL)
#    / find_package(OpenGL GLU) in the tree run normally (no change to existing
#    behaviour).
#
# After this block the following variables are available:
#   AFNI_MESA_HINT          - hint directory (empty when not overriding)
#   AFNI_MESA_GL_LIBRARY    - full path to libGL  (empty when not overriding)
#   AFNI_MESA_GLU_LIBRARY   - full path to libGLU (empty when not overriding)
#   AFNI_MESA_INCLUDE_DIR   - Mesa include directory (empty when not overriding)

set(AFNI_MESA_HINT        "")
set(AFNI_MESA_GL_LIBRARY  "")
set(AFNI_MESA_GLU_LIBRARY "")
set(AFNI_MESA_INCLUDE_DIR "")

if(AFNI_MESA_ROOT)
    if(NOT EXISTS "${AFNI_MESA_ROOT}")
        message(FATAL_ERROR
            "[AfniPlatform] AFNI_MESA_ROOT='${AFNI_MESA_ROOT}' does not exist.")
    endif()

    message(STATUS "[AfniPlatform] Using explicit Mesa root: ${AFNI_MESA_ROOT}")
    set(AFNI_MESA_HINT "${AFNI_MESA_ROOT}")

# Locate libGL — direct path check (find_library misses Homebrew symlinks)
    if(EXISTS "${AFNI_MESA_ROOT}/lib/libGL.dylib")
        set(AFNI_MESA_GL_LIBRARY "${AFNI_MESA_ROOT}/lib/libGL.dylib"
            CACHE FILEPATH "" FORCE)
    elseif(EXISTS "${AFNI_MESA_ROOT}/lib/libGL.1.dylib")
        set(AFNI_MESA_GL_LIBRARY "${AFNI_MESA_ROOT}/lib/libGL.1.dylib"
            CACHE FILEPATH "" FORCE)
    endif()
    if(NOT AFNI_MESA_GL_LIBRARY)
        message(WARNING
            "[AfniPlatform] libGL not found under ${AFNI_MESA_ROOT}. "
            "Check that Mesa is installed there.")
    else()
        message(STATUS "[AfniPlatform] Mesa GL  : ${AFNI_MESA_GL_LIBRARY}")
    endif()
    
    # Locate libGLU — check AFNI_GLU_ROOT first, then fall back to AFNI_MESA_ROOT.
    # This allows GL and GLU to come from different installations, e.g.:
    #   -DAFNI_MESA_ROOT=/opt/homebrew/opt/mesa-xquartz-shm-fix
    #   -DAFNI_GLU_ROOT=/opt/homebrew
    set(_glu_search_roots "")
    if(AFNI_GLU_ROOT)
        if(NOT EXISTS "${AFNI_GLU_ROOT}")
            message(FATAL_ERROR
                "[AfniPlatform] AFNI_GLU_ROOT='${AFNI_GLU_ROOT}' does not exist.")
        endif()
        message(STATUS "[AfniPlatform] Using explicit GLU root: ${AFNI_GLU_ROOT}")
        list(APPEND _glu_search_roots "${AFNI_GLU_ROOT}")
    endif()
    # Always also search AFNI_MESA_ROOT as fallback
    list(APPEND _glu_search_roots "${AFNI_MESA_ROOT}")

    foreach(_glu_root IN LISTS _glu_search_roots)
        if(EXISTS "${_glu_root}/lib/libGLU.dylib")
            set(AFNI_MESA_GLU_LIBRARY "${_glu_root}/lib/libGLU.dylib"
                CACHE FILEPATH "" FORCE)
            break()
        elseif(EXISTS "${_glu_root}/lib/libGLU.so")
            set(AFNI_MESA_GLU_LIBRARY "${_glu_root}/lib/libGLU.so"
                CACHE FILEPATH "" FORCE)
            break()
        elseif(EXISTS "${_glu_root}/lib/libGLU.1.dylib")
            set(AFNI_MESA_GLU_LIBRARY "${_glu_root}/lib/libGLU.1.dylib"
                CACHE FILEPATH "" FORCE)
            break()
        endif()
    endforeach()
    unset(_glu_search_roots)
    unset(_glu_root)

    if(NOT AFNI_MESA_GLU_LIBRARY)
        message(WARNING
            "[AfniPlatform] libGLU not found under AFNI_GLU_ROOT or AFNI_MESA_ROOT. "
            "Set -DAFNI_GLU_ROOT=<prefix> to specify explicitly.")
    else()
        message(STATUS "[AfniPlatform] Mesa GLU : ${AFNI_MESA_GLU_LIBRARY}")
    endif()
    
    # Locate headers
    if(EXISTS "${AFNI_MESA_ROOT}/include/GL/gl.h")
        set(AFNI_MESA_INCLUDE_DIR "${AFNI_MESA_ROOT}/include"
            CACHE PATH "" FORCE)
    endif()
    if(NOT AFNI_MESA_INCLUDE_DIR)
        message(WARNING
            "[AfniPlatform] GL headers not found under ${AFNI_MESA_ROOT}/include.")
    else()
        message(STATUS "[AfniPlatform] Mesa inc : ${AFNI_MESA_INCLUDE_DIR}")
    endif()

    # Inject the Mesa prefix so that any downstream find_package(OpenGL) that
    # runs *after* this include will prefer it.
    list(PREPEND CMAKE_PREFIX_PATH "${AFNI_MESA_ROOT}")

    # On macOS, prevent CMake from picking the system framework OpenGL instead.
    if(AFNI_IS_MACOS)
        set(OpenGL_GL_PREFERENCE LEGACY CACHE STRING "" FORCE)
        set(CMAKE_FIND_FRAMEWORK LAST CACHE STRING "" FORCE)
    endif()

    # Create an INTERFACE imported target so downstream targets can simply do:
    #   target_link_libraries(my_target PRIVATE AFNI::Mesa)
    if(NOT TARGET AFNI::Mesa)
        add_library(AFNI::Mesa INTERFACE IMPORTED)
        if(AFNI_MESA_GL_LIBRARY)
            set_property(TARGET AFNI::Mesa APPEND PROPERTY
                INTERFACE_LINK_LIBRARIES "${AFNI_MESA_GL_LIBRARY}")
        endif()
        if(AFNI_MESA_GLU_LIBRARY)
            set_property(TARGET AFNI::Mesa APPEND PROPERTY
                INTERFACE_LINK_LIBRARIES "${AFNI_MESA_GLU_LIBRARY}")
        endif()
        if(AFNI_MESA_INCLUDE_DIR)
            set_property(TARGET AFNI::Mesa APPEND PROPERTY
                INTERFACE_INCLUDE_DIRECTORIES "${AFNI_MESA_INCLUDE_DIR}")
        endif()
    endif()

else()
    message(STATUS
        "[AfniPlatform] No AFNI_MESA_ROOT set — using system OpenGL/Mesa discovery.")
endif()

# ---------------------------------------------------------------------------
# 8.  OpenMP  (mirrors OMPFLAG in the Makefile)
# ---------------------------------------------------------------------------

# On macOS, Apple clang ships libomp via Homebrew as a separate package
# (/opt/homebrew/opt/libomp).  CMake's FindOpenMP does not always find it.
# We nudge it along when Homebrew is present.

if(AFNI_IS_MACOS AND AFNI_BREW_PREFIX)
    set(_libomp_root "${AFNI_BREW_PREFIX}/opt/libomp")
    if(EXISTS "${_libomp_root}")
        list(PREPEND CMAKE_PREFIX_PATH "${_libomp_root}")
        # Required by Apple clang OpenMP support
        set(OpenMP_C_FLAGS
            "-Xpreprocessor -fopenmp -I${_libomp_root}/include"
            CACHE STRING "OpenMP C flags for Apple clang" FORCE)
        set(OpenMP_C_LIB_NAMES "omp" CACHE STRING "" FORCE)
        set(OpenMP_omp_LIBRARY "${_libomp_root}/lib/libomp.dylib"
            CACHE FILEPATH "" FORCE)
        message(STATUS "[AfniPlatform] libomp hint: ${_libomp_root}")
    endif()
    unset(_libomp_root)
endif()

# ---------------------------------------------------------------------------
# 9.  Summary
# ---------------------------------------------------------------------------

message(STATUS "------------------------------------------------------------")
message(STATUS "  AFNI platform summary")
message(STATUS "  Arch              : ${AFNI_ARCH}")
message(STATUS "  ARM build         : ${AFNI_IS_ARM}")
message(STATUS "  macOS             : ${AFNI_IS_MACOS}")
message(STATUS "  Homebrew prefix   : ${AFNI_BREW_PREFIX}")
message(STATUS "  AFNI_MESA_ROOT    : '${AFNI_MESA_ROOT}'")
message(STATUS "  AFNI_GLU_ROOT     : '${AFNI_GLU_ROOT}'")
message(STATUS "  Mesa GL lib       : '${AFNI_MESA_GL_LIBRARY}'")
message(STATUS "  Mesa GLU lib      : '${AFNI_MESA_GLU_LIBRARY}'")
message(STATUS "  Mesa include dir  : '${AFNI_MESA_INCLUDE_DIR}'")
message(STATUS "  Compiler          : ${CMAKE_C_COMPILER}")
message(STATUS "------------------------------------------------------------")
