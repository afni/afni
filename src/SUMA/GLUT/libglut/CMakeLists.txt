# # Copyright (c) Mark J. Kilgard, 1995. #
add_afni_library(GLUT glut_8x13.c glut_9x15.c glut_bitmap.c glut_bwidth.c
glut_cindex.c glut_cursor.c glut_dials.c glut_event.c glut_ext.c
glut_fullscrn.c glut_get.c glut_hel10.c glut_hel12.c glut_hel18.c glut_init.c
glut_input.c glut_menu.c glut_modifier.c glut_mroman.c glut_overlay.c
glut_roman.c glut_shapes.c glut_space.c glut_stroke.c glut_swidth.c
glut_tablet.c glut_teapot.c glut_tr10.c glut_tr24.c glut_util.c glut_win.c
glut_winmisc.c layerutil.c ) 
target_include_directories(GLUT PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/..)
set_target_properties(GLUT PROPERTIES PRIVATE_HEADER "GL/glut.h") 
target_link_libraries(GLUT
  PUBLIC 
    m
    X11::Xmu
  PRIVATE
    X11::Xi
    $<IF:$<TARGET_EXISTS:XQuartzGL::GLU>,XQuartzGL::GLU,OpenGL::GLU>
   )

# CFLAGS = -I.. -O ${SUMA_INCLUDE_PATH}

# OBJS = \

# libglut.a : $(OBJS) ar cr libglut.a $(OBJS)

# clean: -rm -f -rm -f libglut.a
