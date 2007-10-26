
# quick and dirty for now...

# CFLAGS = -Wall -g
# CFLAGS = -O3

# IFLAGS = -I/sw/include
# LFLAGS = -L/sw/lib

gifti_test: gifti_test.o gifti_io.o gifti_xml.o
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(LFLAGS) -lexpat	\
		gifti_test.o gifti_io.o gifti_xml.o

clean:
	$(RM) gifti_test *.o

%.o: %.c %.h
	$(RM) $@
	$(CC) $(CFLAGS) $(IFLAGS) -c $<

