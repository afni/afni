# A small object library to attempt to determine the path of a running
# executable.
add_library(whats_my_exepath OBJECT whats_my_exepath.c)
set_target_properties(whats_my_exepath PROPERTIES PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/whats_my_exepath.h)
target_include_directories(
  whats_my_exepath INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                  $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR}>
)
if(APPLE)
target_compile_definitions(whats_my_exepath PRIVATE DARWIN)
endif()
target_compile_definitions(whats_my_exepath INTERFACE WHATS_MY_EXEPATH)

