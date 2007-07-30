
# quick and dirty for now...

# CFLAGS = -Wall -g

gtest: gtest.o gifti.o gifti_xml.o
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(LFLAGS) -lexpat gtest.o gifti.o gifti_xml.o

%.o: %.c %.h
	$(RM) $@
	$(CC) $(CFLAGS) $(IFLAGS) -c $<

