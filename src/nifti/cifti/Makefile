
# note the TARFILE_NAME embeds the release version number
TARFILE_NAME	= nifti2clib-0.0.1

USEZLIB         = -DHAVE_ZLIB
GDIR            = ../../gifti

## Compiler  defines
CC		= gcc
IFLAGS          = -I. 
CFLAGS          = -Wall -std=gnu99 -pedantic
NIFLAGS         = -I$(GDIR) -I../nifti2 -I../niftilib -I../znzlib $(USEZLIB)

OBJS            = afni_xml.o
LLIBS 		= -lz -lm

# --------------------------------------------------
# default compile for C files
%.o : %.c %.h
	$(CC) -c $(CFLAGS) $(IFLAGS) $< -o $@

# --------------------------------------------------
# main targets (primary is nifti_tool, for now)


afni_xml_tool: afni_xml_tool.c $(OBJS)
	$(CC) -o $@ $(CFLAGS) $(IFLAGS) $< $(OBJS)      \
        -lexpat $(LLIBS)

clean:
	$(RM) *.o afni_xml_tool

afni_xml.o: afni_xml.c afni_xml.h
	$(CC) -c $(CFLAGS) -I. $< 

# --------------------------------------------------
# targets from source residing elsewhere
gifti_xml.o:
	$(CC) -O3 -DHAVE_ZLIB -c $(CFLAGS) $(IFLAGS) $(GDIR)/gifti_xml.c -o $@

