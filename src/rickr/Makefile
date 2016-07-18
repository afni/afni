
IMON_OBJS  = l_mcw_glob.o realtime.o

WARN_OPT = 


clean:
	$(RM) *.o

file_tool: file_tool.o file_tool.h fields.o fields.h
	$(RM) $@
	$(CC) -o file_tool file_tool.o fields.o ../ge4_header.o -lm

tokens: tokens.o
	$(RM) $@
	$(CC) -o tokens tokens.o

Imon: Imon.o $(IMON_OBJS) Imon.h realtime.h
	$(RM) $@
	$(CC) -o Imon Imon.o $(IMON_OBJS) ../thd_iochan.o -lm

Dimon: Dimon.o realtime.o
	$(RM) $@
	$(CC) -o Dimon Dimon.o realtime.o $(LFLAGS) -L.. $(LLIBS)

Dimon1: Dimon1.o realtime1.o
	$(RM) $@
	$(CC) -o Dimon1 Dimon1.o realtime1.o $(LFLAGS) -L.. $(LLIBS)

serial_helper: serial_helper.o
	$(RM) $@
	$(CC) -o serial_helper serial_helper.o -lm

# ----------------------------------------------------------------------
# local Imon objects need mcw_glob, but without MCW_MALLOC

Imon.o: Imon.c Imon.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c Imon.c -I.. $(IFLAGS)

Dimon.o: Dimon.c Dimon.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c Dimon.c -I.. $(IFLAGS)

Dimon1.o: Dimon1.c Imon.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c Dimon1.c -I.. $(IFLAGS)

# no longer needed, but let's make sure...
dimon_afni.o: dimon_afni.c ../mri_image.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c dimon_afni.c -I.. $(IFLAGS)

l_mcw_glob.o: l_mcw_glob.c l_mcw_glob.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c l_mcw_glob.c -I.. $(IFLAGS)

mri_dicom_hdr.o: ../mri_dicom_hdr.c
	$(RM) $@
	$(CC) $(WARN_OPT)  -DFOR_DIMON -c ../mri_dicom_hdr.c -I.. $(IFLAGS)

realtime.o: realtime.c realtime.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c realtime.c -I.. $(IFLAGS)

realtime1.o: realtime1.c realtime.h
	$(RM) $@
	$(CC) $(WARN_OPT)  -c realtime1.c -I.. $(IFLAGS)

# -------------- end local Imon objects --------------------------------

.c.o:
	$(RM) $@
	$(CC) $(WARN_OPT) -c $*.c -I.. $(IFLAGS)

