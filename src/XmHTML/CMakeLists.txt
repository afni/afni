# TODO: install prefix should be used to distinguish from external library
cmake_minimum_required(VERSION 3.13)
project(XmHTML)

set(XMHTML_VERSION "1107")
set(XMHTML_ARCHIVE "1.1.7")
if (NOT ${X11_FOUND})
  find_package(X11 REQUIRED)
endif()
find_package(JPEG 62 REQUIRED)

# TODO: set target specific compile_definitions
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DVERSION=${XMHTML_VERSION} -DMotif -DHAVE_LIBJPEG -DHAVE_REGEX_H")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -DNDEBUG")

# Construct XmHTML  library
add_library(XmHTML "")
target_compile_definitions(XmHTML PRIVATE "VERSION=${XMHTML_VERSION}" Motif HAVE_LIBJPEG HAVE_REGEX_H)
# add headers to XmHTML target_sources
add_subdirectory(include)

# add the c files to XmHTML target sources
add_subdirectory(lib)

target_include_directories(XmHTML 
  PRIVATE 
  ${X11_INCLUDE_DIR}
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR})

target_link_libraries(XmHTML ${X11_Xpm_LIB} JPEG::JPEG)


# TODO: need to install include to XmHTML
install(TARGETS XmHTML
       EXPORT AFNITargets
       RUNTIME DESTINATION ${AFNI_INSTALL_RUNTIME_DIR}
       LIBRARY DESTINATION ${AFNI_INSTALL_LIBRARY_DIR}
       ARCHIVE DESTINATION ${AFNI_INSTALL_ARCHIVE_DIR}
       PUBLIC_HEADER DESTINATION ${AFNI_INSTALL_INCLUDE_DIR}
) 
add_library(AFNI::XmHTML ALIAS XmHTML)
