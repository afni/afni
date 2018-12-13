# This trivial CMakeLists.txt file stands alone from the
# rest of the build system.  It is intended represent
# a down-stream package that depends on the NIFIT
# development libraries.
#
# to configure this stand-alone package
# run "cmake -DNIFTI_DIR:PATH=${build_or_install_dir}/NIFTI ${THIS_SOURCE_DIR}"
#

cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)

project(real_easy)
find_package(NIFTI 2.1.0 CONFIG REQUIRED)

add_executable(real_easy nifti1_read_write.c)
target_link_libraries(real_easy PUBLIC NIFTI::niftiio)

add_executable(nifti_cdf_program nifti_cdf_program.c)
target_link_libraries(nifti_cdf_program NIFTI::nifticdf)
