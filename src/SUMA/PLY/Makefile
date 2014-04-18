clean:
	$(RM) *.o *.a
   
objs:
	$(CC) ${CCOPT} ${WARNOPT} -I./ -c plyfile.c
	$(CC) ${CCOPT} ${WARNOPT} -I./ -c obj_io.c
   
libply.a:  objs
	ar q $@ plyfile.o obj_io.o
	$(RANLIB) $@
   
plytest: 
	cc -I./ -o plytest plytest.c libply.a
