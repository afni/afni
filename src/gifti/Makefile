
# quick and dirty (getting dirtier) for now...

# might not have zlib
APPLY_ZLIB = -DHAVE_ZLIB

NIFTI_DIR = ../nifti

LIBTOOL = libtool

C_LIBFLAGS = -03 -fPIC -DPIC $(APPLY_ZLIB)
INST_DIR = /usr/lib
VER = 1.0.0

# CFLAGS = -Wall -Wextra -g -pedantic -std=c99 $(APPLY_ZLIB)
CFLAGS = -O3 $(APPLY_ZLIB)
IFLAGS = -I$(NIFTI_DIR)/include
LFLAGS = -L$(NIFTI_DIR)/lib

CC = gcc $(CFLAGS)

# for macs (getting expat from fink)
# IFLAGS = -I/sw/include $(IFLAGS)
# LFLAGS = -L/sw/lib $(LFLAGS)

gifti_tool: gifti_tool.o gifti_io.o gifti_xml.o
	$(RM) $@
	$(CC) -o $@ gifti_tool.o gifti_io.o gifti_xml.o \
	      $(LFLAGS) -lexpat -lniftiio -lznz -lz -lm

gifti_test: gifti_test.o gifti_io.o gifti_xml.o
	$(RM) $@
	$(CC) -o $@ gifti_test.o gifti_io.o gifti_xml.o \
	      $(LFLAGS) -lexpat -lniftiio -lznz -lz -lm

libgiftiio_la:
	$(LIBTOOL) --mode=compile $(CC) $(IFLAGS) -o gifti_io.lo -c gifti_io.c
	$(LIBTOOL) --mode=compile $(CC) $(IFLAGS) -o gifti_xml.lo -c gifti_xml.c
	$(LIBTOOL) --mode=link $(CC) -release $(VER) -o libgiftiio.la gifti_io.lo gifti_xml.lo -rpath $(INST_DIR) $(LFLAGS) -lexpat -lniftiio -lznz -lz -lm

libgiftiio: libgiftiio_la
	$(LIBTOOL) --mode=install install -c libgiftiio.la $(INST_DIR)

all: gifti_tool gifti_test libgiftiio

clean:
	$(RM) gifti_tool *.o

clean_all:
	$(RM) gifti_test gifti_tool *.o
	$(RM) gifti*.lo*
	$(RM) libgifti*.la
	$(RM) *.so

%.o: %.c %.h
	$(RM) $@
	$(CC) $(IFLAGS) -c $<

