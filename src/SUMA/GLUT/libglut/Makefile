#
# Copyright (c) Mark J. Kilgard, 1995.
#

all : libglut.a

CFLAGS = -I.. -O

OBJS = \
	glut_bitmap.o \
	glut_bwidth.o \
	glut_cindex.o \
	glut_cursor.o \
	glut_dials.o \
	glut_event.o \
	glut_ext.o \
	glut_fullscrn.o \
	glut_get.o \
	glut_init.o \
	glut_input.o \
	glut_menu.o \
	glut_modifier.o \
	glut_overlay.o \
	glut_shapes.o \
	glut_space.o \
	glut_stroke.o \
	glut_swidth.o \
	glut_tablet.o \
	glut_teapot.o \
	glut_util.o \
	glut_win.o \
	glut_winmisc.o \
	glut_8x13.o \
	glut_9x15.o \
	glut_hel10.o \
	glut_hel12.o \
	glut_hel18.o \
	glut_tr10.o \
	glut_tr24.o \
	glut_roman.o \
	glut_mroman.o \
	layerutil.o

libglut.a : $(OBJS)
	ar cr libglut.a $(OBJS)

clean:
	-rm -f *.o
	-rm -f libglut.a

