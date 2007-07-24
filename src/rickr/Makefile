
IMON_OBJS  = l_mcw_glob.o realtime.o

WARN_OPT = 


clean:
	$(RM) *.o

file_tool: file_tool.o file_tool.h fields.o fields.h
	$(RM) $@
	$(CC) -o file_tool file_tool.o fields.o ../ge4_header.o -lm

Imon: Imon.o $(IMON_OBJS) Imon.h realtime.h
	$(RM) $@
	$(CC) -o Imon Imon.o $(IMON_OBJS) ../thd_iochan.o $(LFLAGS) -L.. -lm $(EXTRA_LIBS_2)

Dimon: Dimon.o dimon_afni.o mri_dicom_hdr.o $(IMON_OBJS)
	$(RM) $@
	$(CC) -o Dimon Dimon.o dimon_afni.o mri_dicom_hdr.o ../thd_iochan.o $(IMON_OBJS) $(LFLAGS) -L.. -lm $(EXTRA_LIBS_2)

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

