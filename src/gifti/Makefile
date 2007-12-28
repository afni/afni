
# quick and dirty (getting dirtier) for now...

# might not have zlib
APPLY_ZLIB = -DHAVE_ZLIB

NIFTI_DIR = ../nifti/Clibs

# CFLAGS = -Wall -g -pedantic -std=c99 $(APPLY_ZLIB)
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
	      $(LFLAGS) -lexpat -lz 

gifti_test: gifti_test.o gifti_io.o gifti_xml.o
	$(RM) $@
	$(CC) -o $@ gifti_test.o gifti_io.o gifti_xml.o \
	      $(LFLAGS) -lexpat -lz 

all: gifti_tool gifti_test

clean:
	$(RM) gifti_test gifti_tool *.o

%.o: %.c %.h
	$(RM) $@
	$(CC) $(IFLAGS) -c $<

