# Toolchain for building AFNI on Apple Silicon with Homebrew LLVM clang.
#
# Usage:
#   cmake -S . -B build-llvm-arm64 -G Ninja \
#     -DCMAKE_TOOLCHAIN_FILE=cmake/macos_homebrew_llvm_arm64_toolchain.cmake
#
# This was useful locally when Homebrew GCC 13 could not compile against the
# installed Command Line Tools SDK. It still uses the active SDK reported by
# xcrun, and keeps Apple cctools ahead of tools from conda or other environments.
# By default, the deployment target is set to the active SDK's major version.
# That matches current Homebrew bottles on Apple Silicon and avoids link-time
# warnings from mixing an older compiler default with newer Homebrew dylibs.
# Set CMAKE_OSX_DEPLOYMENT_TARGET on the cmake command line when a different
# minimum supported macOS version is required.

set(CMAKE_SYSTEM_NAME Darwin)
set(CMAKE_SYSTEM_PROCESSOR arm64)

set(AFNI_HOMEBREW_PREFIX "/opt/homebrew" CACHE PATH "Homebrew prefix")
set(_afni_brew_executable "${AFNI_HOMEBREW_PREFIX}/bin/brew")
if(NOT EXISTS "${_afni_brew_executable}")
  set(_afni_brew_executable brew)
endif()

execute_process(
  COMMAND "${_afni_brew_executable}" --prefix llvm
  OUTPUT_VARIABLE _afni_llvm_prefix
  RESULT_VARIABLE _afni_llvm_result
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)

if(NOT _afni_llvm_result EQUAL 0 OR NOT EXISTS "${_afni_llvm_prefix}")
  message(FATAL_ERROR
    "Could not find Homebrew LLVM via 'brew --prefix llvm'. "
    "Install it with 'brew install llvm', or use the GCC toolchain file instead: "
    "cmake/macos_homebrew_gcc13_arm64_toolchain.cmake"
  )
endif()

set(_afni_llvm_clang "${_afni_llvm_prefix}/bin/clang")
set(_afni_llvm_clangxx "${_afni_llvm_prefix}/bin/clang++")
if(NOT EXISTS "${_afni_llvm_clang}" OR NOT EXISTS "${_afni_llvm_clangxx}")
  message(FATAL_ERROR
    "Homebrew LLVM was found at '${_afni_llvm_prefix}', but the expected "
    "compiler pair is missing:\n"
    "  ${_afni_llvm_clang}\n"
    "  ${_afni_llvm_clangxx}"
  )
endif()

set(CMAKE_C_COMPILER   "${_afni_llvm_clang}"   CACHE FILEPATH "C compiler")
set(CMAKE_CXX_COMPILER "${_afni_llvm_clangxx}" CACHE FILEPATH "CXX compiler")

unset(_afni_brew_executable)
unset(_afni_llvm_clang)
unset(_afni_llvm_clangxx)
unset(_afni_llvm_prefix)
unset(_afni_llvm_result)

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

set(CMAKE_OSX_SYSROOT "${AFNI_MACOS_SDK}" CACHE PATH "macOS SDK")
set(CMAKE_OSX_ARCHITECTURES arm64 CACHE STRING "Build architecture")

if(NOT CMAKE_OSX_DEPLOYMENT_TARGET)
  if(NOT AFNI_MACOS_SDK_VERSION_RESULT EQUAL 0)
    message(FATAL_ERROR
      "Could not read the macOS SDK version via 'xcrun --show-sdk-version'. "
      "xcrun error: ${AFNI_MACOS_SDK_VERSION_ERROR}"
    )
  endif()
  string(REGEX MATCH "^[0-9]+" AFNI_MACOS_SDK_MAJOR "${AFNI_MACOS_SDK_VERSION}")
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
