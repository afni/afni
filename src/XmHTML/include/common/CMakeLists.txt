target_sources(XmHTML PUBLIC LZWStream.h XmHTMLconf.h escapes.h
  icons.h   plc.h xpmtags.h HTMLWarnings.h  XmHTMLI.h debug.h hash.h
  miniparse.h stack.h)
target_include_directories(XmHTML PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} )

