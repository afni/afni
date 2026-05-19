# Toolchain for building AFNI on Apple Silicon with Homebrew GCC.
#
# Usage:
#   cmake -S . -B build-gcc13-arm64 -G Ninja \
#     -DCMAKE_TOOLCHAIN_FILE=cmake/macos_homebrew_gcc13_arm64_toolchain.cmake \
#     -DAFNI_HOMEBREW_GCC_VERSION=13
#
# This file intentionally sets the macOS SDK explicitly. Homebrew GCC can have
# a stale built-in sysroot after macOS or Command Line Tools upgrades, which can
# otherwise show up as link errors such as "library not found for -lSystem".
# By default, the deployment target is set to the active SDK's major version.
# That matches current Homebrew bottles on Apple Silicon and avoids link-time
# warnings from mixing an older compiler default with newer Homebrew dylibs.
# Set CMAKE_OSX_DEPLOYMENT_TARGET on the cmake command line when a different
# minimum supported macOS version is required.

set(CMAKE_SYSTEM_NAME Darwin)
set(CMAKE_SYSTEM_PROCESSOR arm64)

set(AFNI_HOMEBREW_PREFIX "/opt/homebrew" CACHE PATH "Homebrew prefix")
set(AFNI_HOMEBREW_GCC_VERSION "13" CACHE STRING "Homebrew GCC major version")

set(_afni_homebrew_gcc "${AFNI_HOMEBREW_PREFIX}/bin/gcc-${AFNI_HOMEBREW_GCC_VERSION}")
set(_afni_homebrew_gxx "${AFNI_HOMEBREW_PREFIX}/bin/g++-${AFNI_HOMEBREW_GCC_VERSION}")

if(NOT EXISTS "${_afni_homebrew_gcc}" OR NOT EXISTS "${_afni_homebrew_gxx}")
  message(FATAL_ERROR
    "Could not find Homebrew GCC ${AFNI_HOMEBREW_GCC_VERSION}. Expected:\n"
    "  ${_afni_homebrew_gcc}\n"
    "  ${_afni_homebrew_gxx}\n"
    "Install a versioned formula such as 'brew install gcc@13', install the "
    "current GCC with 'brew install gcc', or pass "
    "-DAFNI_HOMEBREW_GCC_VERSION=<version>."
  )
endif()

set(CMAKE_C_COMPILER "${_afni_homebrew_gcc}" CACHE FILEPATH "C compiler")
set(CMAKE_CXX_COMPILER "${_afni_homebrew_gxx}" CACHE FILEPATH "CXX compiler")

execute_process(
  COMMAND xcrun --show-sdk-path
  OUTPUT_VARIABLE AFNI_MACOS_SDK
  ERROR_VARIABLE AFNI_MACOS_SDK_ERROR
  RESULT_VARIABLE AFNI_MACOS_SDK_RESULT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND xcrun --show-sdk-version
  OUTPUT_VARIABLE AFNI_MACOS_SDK_VERSION
  ERROR_VARIABLE AFNI_MACOS_SDK_VERSION_ERROR
  RESULT_VARIABLE AFNI_MACOS_SDK_VERSION_RESULT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(NOT AFNI_MACOS_SDK_RESULT EQUAL 0 OR NOT EXISTS "${AFNI_MACOS_SDK}")
  message(FATAL_ERROR
    "Could not find a usable macOS SDK via 'xcrun --show-sdk-path'. "
    "Install or repair Xcode Command Line Tools. xcrun error: ${AFNI_MACOS_SDK_ERROR}"
  )
endif()

if(NOT AFNI_MACOS_SDK_VERSION_RESULT EQUAL 0)
  message(FATAL_ERROR
    "Could not read the macOS SDK version via 'xcrun --show-sdk-version'. "
    "xcrun error: ${AFNI_MACOS_SDK_VERSION_ERROR}"
  )
endif()

string(REGEX MATCH "^[0-9]+" AFNI_MACOS_SDK_MAJOR "${AFNI_MACOS_SDK_VERSION}")

execute_process(
  COMMAND "${_afni_homebrew_gcc}" -v
  OUTPUT_VARIABLE _afni_gcc_stdout
  ERROR_VARIABLE _afni_gcc_stderr
  RESULT_VARIABLE _afni_gcc_result
)
if(NOT _afni_gcc_result EQUAL 0)
  message(FATAL_ERROR "Could not query '${_afni_homebrew_gcc} -v'.")
endif()

set(_afni_gcc_v "${_afni_gcc_stdout}\n${_afni_gcc_stderr}")
string(
  REGEX MATCH
  "--with-sysroot=([^ \n]*MacOSX([0-9]+)(\\.[0-9]+)?\\.sdk)"
  _afni_gcc_sysroot_match
  "${_afni_gcc_v}"
)
if(_afni_gcc_sysroot_match)
  set(_afni_gcc_configured_sysroot "${CMAKE_MATCH_1}")
  set(_afni_gcc_sdk_major "${CMAKE_MATCH_2}")
  if(AFNI_MACOS_SDK_MAJOR VERSION_LESS "${_afni_gcc_sdk_major}")
    message(FATAL_ERROR
      "Homebrew GCC ${AFNI_HOMEBREW_GCC_VERSION} was built against "
      "MacOSX${_afni_gcc_sdk_major}.sdk:\n"
      "  ${_afni_gcc_configured_sysroot}\n"
      "but the active Command Line Tools SDK is ${AFNI_MACOS_SDK_VERSION}:\n"
      "  ${AFNI_MACOS_SDK}\n"
      "Update or repair Xcode Command Line Tools, or use a Homebrew GCC built "
      "for the installed SDK. Otherwise CMake may fail later with the much less "
      "clear linker error 'library not found for -lSystem'."
    )
  endif()
endif()

set(CMAKE_OSX_SYSROOT "${AFNI_MACOS_SDK}" CACHE PATH "macOS SDK")
set(CMAKE_OSX_ARCHITECTURES arm64 CACHE STRING "Build architecture")

if(NOT CMAKE_OSX_DEPLOYMENT_TARGET)
  set(
    CMAKE_OSX_DEPLOYMENT_TARGET
    "${AFNI_MACOS_SDK_MAJOR}.0"
    CACHE STRING
    "Minimum macOS deployment version"
  )
endif()

# Prefer Apple cctools over anything that may be earlier in PATH, such as conda.
set(CMAKE_AR /usr/bin/ar CACHE FILEPATH "Archiver")
set(CMAKE_RANLIB /usr/bin/ranlib CACHE FILEPATH "Ranlib")
set(CMAKE_NM /usr/bin/nm CACHE FILEPATH "NM")
set(CMAKE_LINKER /usr/bin/ld CACHE FILEPATH "Linker")
set(CMAKE_INSTALL_NAME_TOOL /usr/bin/install_name_tool CACHE FILEPATH "install_name_tool")
set(CMAKE_STRIP /usr/bin/strip CACHE FILEPATH "Strip")

unset(_afni_gcc_configured_sysroot)
unset(_afni_gcc_result)
unset(_afni_gcc_sdk_major)
unset(_afni_gcc_stderr)
unset(_afni_gcc_stdout)
unset(_afni_gcc_sysroot_match)
unset(_afni_gcc_v)
unset(_afni_homebrew_gcc)
unset(_afni_homebrew_gxx)
