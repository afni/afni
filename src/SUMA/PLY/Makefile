clean:
	$(RM) *.o *.a
   
libply.a: 
	$(CC) ${CCOPT} ${WARNOPT} -I./ -c plyfile.c
	ar q $@ plyfile.o
	$(RANLIB) $@
   
plytest: 
	cc -I./ -o plytest plytest.c libply.a
