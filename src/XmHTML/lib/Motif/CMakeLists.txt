# not using getps.c
target_sources(XmHTML PRIVATE Balloon.c  XmHTML.c  debug_menu.c
  frames.c  motif.c   plc.c   textsel.c XCC.c   XmImage.c forms.c  output.c
  strings.c warnings.c)
target_include_directories(XmHTML PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})