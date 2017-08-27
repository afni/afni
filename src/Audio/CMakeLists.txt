add_library(audio INTERFACE)
target_sources(audio INTERFACE 
  ${CMAKE_CURRENT_SOURCE_DIR}/cs_sounds.h 
  ${CMAKE_CURRENT_SOURCE_DIR}/smb3_coin.h 
  ${CMAKE_CURRENT_SOURCE_DIR}/smb3_fireball.h)
target_include_directories(audio INTERFACE 
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${AFNI_INSTALL_INCLUDE_DIR})
