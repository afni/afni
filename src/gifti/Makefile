
# quick and dirty for now...

CFLAGS = -Wall -g
CC     = gcc

gtest: gtest.o gifti.o gifti_xml.o
	$(CC) $(CFLAGS) -o gtest -lexpat gtest.o gifti.o gifti_xml.o

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

