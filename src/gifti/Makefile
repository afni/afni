
# quick and dirty for now...

CC = gcc

# CFLAGS = -Wall -g -pedantic -std=c99
CFLAGS = -O3

# for macs (getting expat from fink)
# IFLAGS = -I/sw/include
# LFLAGS = -L/sw/lib

gifti_test: gifti_test.o gifti_io.o gifti_xml.o
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(LFLAGS) -lexpat	-lz \
		gifti_test.o gifti_io.o gifti_xml.o

clean:
	$(RM) gifti_test *.o

%.o: %.c %.h
	$(RM) $@
	$(CC) $(CFLAGS) $(IFLAGS) -c $<

