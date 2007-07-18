
# quick and dirty for now...

CFLAGS = -Wall -g
CC     = gcc

etest: etest.o gifti.o gifti_xml.o
	$(CC) $(CFLAGS) -o etest -lexpat etest.o gifti.o gifti_xml.o

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

