# a prefix can be used to avoid collisions with pre-existsing installations of
# the nifti libraries. The same prefix will be used for gifti. The prefix is
# prepended to the exported targets so that if NIFTI_PACKAGE_PREFIX is specified
# then the imported target is no longer NIFTI::znz but prefixNIFTI::prefixznz.

############ NIFTI ##########
# Try to find nifti and check it has the appropriate target prefix imported
  # set(NIFTI_PACKAGE_PREFIX "" CACHE INTERNAL "This variable should not be set for the AFNI build.")
  # find_package(NIFTI 3 QUIET)
  # if ("${NIFTI_FOUND}")
  #   set(EXPECTED_TARGET NIFTI::znz)
  #   if (NOT TARGET ${EXPECTED_TARGET})
  #     message(FATAL_ERROR
  #       "An appropriate NIFTI library was found at"
  #       " ${NIFTI_DIR} but the expected target"
  #       " ${EXPECTED_TARGET} was not imported. Instead a version of nifti_clib will"
  #       " be built specifically for this project."
  #       )
  #     set(NIFTI_FOUND 0)
  #   endif()
  # endif()

  # # If not found or missing appropriate target format rebuild nifti:
  # if (NOT "${NIFTI_FOUND}")
  #   include(FetchContent)
  #   FetchContent_Declare( fetch_nifti_clib_git_repo
  #           # GIT_REPOSITORY https://github.com/NIFTI-Imaging/nifti_clib
  #           GIT_REPOSITORY /afni/nifti_clib
  #           # GIT_REPOSITORY https://github.com/leej3/nifti_clib.git
  #           # GIT_TAG        v3.0.0 # or git <HASH> or git <branch>
  #           GIT_TAG        update_installation # or git <HASH> or git <branch>
  #           )
  #   FetchContent_GetProperties(fetch_nifti_clib_git_repo)
  #     message(STATUS "Fetching info for nifti_clib.")
  #     FetchContent_Populate( fetch_nifti_clib_git_repo )
  #     add_subdirectory(${fetch_nifti_clib_git_repo_SOURCE_DIR} ${fetch_nifti_clib_git_repo_BINARY_DIR})
  # endif()

############ GIFTI ##########

# Find or fetch gifti_clib
find_package(GIFTI 3 QUIET)
if ("${GIFTI_FOUND}")
  set(EXPECTED_TARGET GIFTI::giftiio)
  if (NOT TARGET ${EXPECTED_TARGET})
    message(STATUS
      "An appropriate GIFTI library was found at"
      " ${GIFTI_DIR} but the expected target"
      " ${EXPECTED_TARGET} was not imported. Instead a version of gifti_clib will"
      " be built specifically for this project."
      )
    set(GIFTI_FOUND 0)
  endif()
endif()


# If not found or missing appropriate target format rebuild gifti:
if (NOT "${GIFTI_FOUND}")
  include(FetchContent)
  FetchContent_Declare( gifti_clib
          # GIT_REPOSITORY https://github.com/NIFTI-Imaging/gifti_clib
          GIT_REPOSITORY https://github.com/leej3/gifti_clib.git
          # GIT_REPOSITORY /afni/gifti_clib
          # GIT_TAG        v3.0.0 # or git <HASH> or git <branch>
          GIT_TAG        v1.8.1 # or git <HASH> or git <branch>
          )
  FetchContent_GetProperties(gifti_clib)
  message(STATUS "Fetching info for gifti_clib.")
  FetchContent_Populate( gifti_clib )
  add_subdirectory(${gifti_clib_SOURCE_DIR} ${gifti_clib_BINARY_DIR})
endif()
