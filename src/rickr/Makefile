
RICKR_OBJS = r_idisp.o r_misc.o r_new_resam_dset.o

RICKR_HDRS = r_idisp.h r_misc.h r_new_resam_dset.h
RICKR_SRCS = r_idisp.c r_misc.c r_new_resam_dset.c

librickr.a: $(RICKR_OBJS)
	rm -f librickr.a
	ar q librickr.a $(RICKR_OBJS)
	$(RANLIB) librickr.a


$(RICKR_OBJS): $(RICKR_HDRS) $(RICKR_SRCS)


clean:
	$(RM) *.o

.c.o:
	$(RM) $@
	$(CC) -c $*.c -I. -I.. $(IFLAGS)

