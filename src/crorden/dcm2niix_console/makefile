
# make an afni version, so we do not mess up users
# (otherwise, this is unchanged)
TARGET=dcm2niix_afni

CFLAGS=-s
ifneq ($(OS),Windows_NT)
	OS = $(shell uname)
 	ifeq "$(OS)" "Darwin"
		CFLAGS=-dead_strip
	endif
endif
all:
	g++ $(CFLAGS) -O3 -I. main_console.cpp nii_foreign.cpp nii_dicom.cpp jpg_0XC3.cpp ujpeg.cpp nifti1_io_core.cpp nii_ortho.cpp nii_dicom_batch.cpp  -o $(TARGET) -DmyDisableOpenJPEG -DmyDisableJasper
