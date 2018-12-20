
# note the TARFILE_NAME embeds the release version number
TARFILE_NAME	= nifti2clib-0.0.1

USEZLIB         = -DHAVE_ZLIB
N2DIR           = ../nifti2

## Compiler  defines
CC		= gcc -g
IFLAGS          = -I.
CFLAGS          = -Wall -std=gnu99 -pedantic
NIFLAGS         = $(IFLAGS) -I$(N2DIR) -I../niftilib -I../znzlib $(USEZLIB)

LLIBS 		= -lz -lm
OBJS            = afni_xml.o afni_xml_io.o
NOBJS           = nifti2_io.o nifticdf.o znzlib.o

# --------------------------------------------------
# default compile for C files
%.o : %.c %.h
	$(CC) -c $(CFLAGS) $(NIFLAGS) $< -o $@

# --------------------------------------------------
# main targets (primary is nifti_tool, for now)


afni_xml_tool: afni_xml_tool.c $(OBJS)
	$(CC) -o $@ $(CFLAGS) $(IFLAGS) $< $(OBJS) -lexpat $(LLIBS)

clean:
	$(RM) *.o afni_xml_tool cifti_tool

#afni_xml.o: afni_xml.c afni_xml.h
#	$(CC) -c $(CFLAGS) -I. $<

cifti_tool.o: cifti_tool.c
	$(CC) -c $(CFLAGS) $(NIFLAGS) $<

cifti_tool: $(OBJS) $(NOBJS) cifti_tool.o
	$(CC) -o $@ $(CFLAGS) $(NIFLAGS) \
              cifti_tool.o $(OBJS) $(NOBJS) -lexpat $(LLIBS)

# --------------------------------------------------
# targets from source residing elsewhere

nifti2_io.o:
	$(CC) -c -DHAVE_ZLIB $(CFLAGS) $(NIFLAGS) $(N2DIR)/nifti2_io.c

nifticdf.o:
	$(CC) -c -DHAVE_ZLIB $(CFLAGS) $(NIFLAGS) ../nifticdf/nifticdf.c

znzlib.o:
	$(CC) -c -DHAVE_ZLIB $(CFLAGS) $(NIFLAGS) ../znzlib/znzlib.c
