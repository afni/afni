# TODO: install prefix should be used to distinguish from external library
cmake_minimum_required(VERSION 3.13)
project(XmHTML)
set(CMAKE_BUILD_TYPE "Release")

set(XMHTML_VERSION "1107")
set(XMHTML_ARCHIVE "1.1.7")

set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -DNDEBUG")

# Construct XmHTML  library
add_afni_library(XmHTML "")
target_compile_definitions(
  XmHTML PRIVATE "VERSION=${XMHTML_VERSION}" Motif HAVE_LIBJPEG HAVE_REGEX_H
)
# add headers to XmHTML target_sources
add_subdirectory(include)

# add the c files to XmHTML target sources
add_subdirectory(lib)

target_include_directories(
  XmHTML PRIVATE ${X11_INCLUDE_DIR} $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                 $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR}>
)
target_link_libraries(
  XmHTML PRIVATE Motif::Motif ${X11_Xt_LIB} ${X11_Xpm_LIB} ${X11_LIBRARIES} JPEG::JPEG
)
