# not using debug.c object.c and psfonts.c

target_sources(XmHTML 
  PRIVATE HTTP.c LZWStream.c StringUtil.c callbacks.c colors.c cookie.c error.c
  events.c fonts.c format.c hash.c images.c layout.c map.c paint.c parse.c
  private.c psoutput.c public.c quantize.c readBitmap.c readFLG.c readGIF.c
  readGIFplc.c readJPEG.c readJPEGplc.c readPNG.c readXPM.c stack.c)


target_include_directories(XmHTML PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})