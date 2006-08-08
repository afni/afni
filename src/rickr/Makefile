
RICKR_OBJS = r_idisp.o r_misc.o r_new_resam_dset.o

RICKR_HDRS = r_idisp.h r_misc.h r_new_resam_dset.h
RICKR_SRCS = r_idisp.c r_misc.c r_new_resam_dset.c

IMON_OBJS  = l_mcw_glob.o realtime.o ../thd_iochan.o

AFNI_WARP_OBJS = ../afni_warp.o ../afni_slice_byte.o ../afni_slice_short.o  \
		 ../afni_slice_float.o ../afni_slice_complex.o              \
		 ../afni_slice_rgbyte.o

WARN_OPT = 

librickr.a: $(RICKR_OBJS)
	rm -f librickr.a
	ar q librickr.a $(RICKR_OBJS)
	$(RANLIB) librickr.a


$(RICKR_OBJS): $(RICKR_HDRS) $(RICKR_SRCS)


clean:
	$(RM) *.o

3dresample: 3dresample.o
	$(RM) $@
	$(CC) -o 3dresample 3dresample.o $(AFNI_WARP_OBJS)	\
		 $(LFLAGS) -L.. -lrickr $(LLIBS) -lm

file_tool: file_tool.o file_tool.h
	$(RM) $@
	$(CC) -o file_tool file_tool.o ../ge4_header.o -lm

Imon: Imon.o $(IMON_OBJS) Imon.h realtime.h
	$(RM) $@
	$(CC) -o Imon Imon.o $(IMON_OBJS) $(LFLAGS) -L.. -lm $(EXTRA_LIBS_2)

Dimon: Dimon.o dimon_afni.o mri_dicom_hdr.o $(IMON_OBJS)
	$(RM) $@
	$(CC) -o Dimon Dimon.o dimon_afni.o mri_dicom_hdr.o $(IMON_OBJS) $(LFLAGS) -L.. -lm $(EXTRA_LIBS_2)

serial_helper: serial_helper.o
	$(RM) $@
	$(CC) -o serial_helper serial_helper.o -L.. -lm $(EXTRA_LIBS_2)

# ----------------------------------------------------------------------
# local Imon objects need mcw_glob, but without MCW_MALLOC

Imon.o: Imon.c Imon.h
	$(RM) $@
	$(CC) $(WARN_OPT) -DDONT_USE_MCW_MALLOC -c Imon.c -I.. $(IFLAGS)

Dimon.o: Dimon.c Imon.h
	$(RM) $@
	$(CC) $(WARN_OPT) -DDONT_USE_MCW_MALLOC -c Dimon.c -I.. $(IFLAGS)

dimon_afni.o: dimon_afni.c ../mri_image.h
	$(RM) $@
	$(CC) $(WARN_OPT) -DDONT_USE_MCW_MALLOC -c dimon_afni.c -I.. $(IFLAGS)

l_mcw_glob.o: l_mcw_glob.c l_mcw_glob.h
	$(RM) $@
	$(CC) $(WARN_OPT) -DDONT_USE_MCW_MALLOC -c l_mcw_glob.c -I.. $(IFLAGS)

mri_dicom_hdr.o: ../mri_dicom_hdr.c
	$(RM) $@
	$(CC) $(WARN_OPT) -DDONT_USE_MCW_MALLOC -DFOR_DIMON -c ../mri_dicom_hdr.c -I.. $(IFLAGS)

realtime.o: realtime.c realtime.h
	$(RM) $@
	$(CC) $(WARN_OPT) -DDONT_USE_MCW_MALLOC -c realtime.c -I.. $(IFLAGS)
# -------------- end local Imon objects --------------------------------

.c.o:
	$(RM) $@
	$(CC) $(WARN_OPT) -c $*.c -I.. $(IFLAGS)

