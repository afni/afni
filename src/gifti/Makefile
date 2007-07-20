
# quick and dirty for now...

# CFLAGS = -Wall -g
CC     = gcc

gtest: gtest.o gifti.o gifti_xml.o
	$(RM) $@
	$(CC) -o $@ $(LFLAGS) -lexpat gtest.o gifti.o gifti_xml.o

%.o: %.c %.h
	$(RM) $@
	$(CC) $(IFLAGS) -c $<

