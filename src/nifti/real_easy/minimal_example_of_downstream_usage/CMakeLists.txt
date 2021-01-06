cmake_minimum_required(VERSION 3.11.0 FATAL_ERROR)
project(PARENTDEMO
        VERSION 0.0.0.1
        DESCRIPTION "A demonstration of how to incorporate nifti as a dependency"
        LANGUAGES C)

# Try to find a system version of nifti
find_package(NIFTI 3)
if(NOT NIFTI_FOUND)
  set(DOWNLOAD_TEST_DATA OFF)
  include(FetchContent)
  FetchContent_Declare( nifti_clib
          GIT_REPOSITORY https://github.com/NIFTI-Imaging/nifti_clib.git
          GIT_TAG        master # or v3.0.0, or <HASH>
          )
  if("${CMAKE_VERSION}" VERSION_LESS_EQUAL "3.14")
    FetchContent_GetProperties(nifti_clib)
    if(NOT nifti_clib_POPULATED)
      set(FETCHCONTENT_QUIET OFF)
      message(STATUS "Downloading nifti_clib from github ... please wait")
      FetchContent_Populate( nifti_clib )
      message(STATUS "download complete.")
      add_subdirectory(${nifti_clib_SOURCE_DIR} ${nifti_clib_BINARY_DIR})
    endif()
  else()
    FetchContent_MakeAvailable(nifti_clib)
  endif()
endif()

#  Example of linking project executables against nifti_clib defined targets
add_executable(parent_exe src/parent_project_exe.c)
target_link_libraries(parent_exe PRIVATE NIFTI::nifticdf)