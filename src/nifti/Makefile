
SHELL		=	csh

## Projects
NIFTI		=	niftilib
NIFTICDF	=	nifticdf
ZNZ		=	znzlib
FSLIO		=	fsliolib
TESTING		=	Testing
UTILS_PROGS	=	nifti_stats nifti_tool nifti1_test
THIS_DIR	=	`basename ${PWD}`

## Note the TARFILE_NAME embeds the release version number
TARFILE_NAME	=	nifticlib-2.0.0


## Compiler  defines
cc		=	gcc
CC		=	gcc
AR		=	ar
RANLIB  = ranlib
DEPENDFLAGS	=	-MM
GNU_ANSI_FLAGS	= 	-Wall -ansi -pedantic
ANSI_FLAGS	= 	${GNU_ANSI_FLAGS}
CFLAGS		=	$(ANSI_FLAGS)

## Command defines
## gmake does not work on MacOSX or some versions of linux MAKE  = gmake
RM		=	rm
MV		=	mv
CP		=	cp
TAR		=	/usr/local/pkg/gnu/bin/tar


## Installation
INSTALL_BIN_DIR	=	bin
INSTALL_LIB_DIR	=	lib
INSTALL_INC_DIR	=	include


## Zlib defines
ZLIB_INC	=	-I/usr/include
ZLIB_PATH	=	-L/usr/lib
ZLIB_LIBS 	= 	$(ZLIB_PATH) -lm -lz

##############################################################
# platform specific redefines (to use, set ARCH appropriately)

##ARCH = X86_64

ifeq ($(ARCH),SGI) ## SGI 32bit
ZLIB_INC	=	-I/usr/freeware/include
ZLIB_PATH	=	-L/usr/freeware/lib32
RANLIB		=	echo "ranlib not needed"
else
ifeq ($(ARCH),I386) ## 32-bit Linux
ZLIB_INC	=	-I/usr/include
ZLIB_PATH	=	-L/usr/lib
TAR		=	/bin/tar
else
ifeq ($(ARCH),X86_64) ## 64-bit Linux
ZLIB_INC	=
ZLIB_PATH	=
ZLIB_LIBS 	= 	$(ZLIB_PATH) -lm -lz
TAR		=	/bin/tar
endif
endif
endif


#################################################################

## ZNZ defines
ZNZ_INC		=	-I../$(ZNZ)
ZNZ_PATH	=	-L../$(ZNZ)
ZNZ_LIBS	=	$(ZNZ_PATH)  -lznz
USEZLIB         =       -DHAVE_ZLIB

## NIFTI defines
NIFTI_INC	=	-I../$(NIFTI)
NIFTI_PATH	=	-L../$(NIFTI)
NIFTI_LIBS	=	$(NIFTI_PATH) -lniftiio

## NIFTICDF defines
NIFTICDF_INC	=	-I../$(NIFTICDF)
NIFTICDF_PATH	=	-L../$(NIFTICDF)
NIFTICDF_LIBS	=	$(NIFTICDF_PATH) -lnifticdf

## FSLIO defines
FSLIO_INC	=	-I../$(FSLIO)
FSLIO_PATH	=	-L../$(FSLIO)
FSLIO_LIBS	=	$(FSLIO_PATH) -lfslio



## Rules

.SUFFIXES: .c .o

.c.o:
	$(CC) -c $(CFLAGS) $(INCFLAGS) $<

## Targets

all:	   znz nifti nifticdf fslio install mention_cmake

install:   znz_install nifti_install nifticdf_install fslio_install 

clean:	   znz_clean nifti_clean nifticdf_clean fslio_clean

clean_all: clean install_clean doc_clean


znz:
	echo "arch is $(ARCH)"
	(cd $(ZNZ); $(MAKE) depend; $(MAKE) lib;)
	@echo " ----------- $(ZNZ) build completed."
	@echo ""

nifti:	znz
	(cd $(NIFTI); $(MAKE) depend; $(MAKE) lib;)
	@echo " ----------- $(NIFTI) build completed."
	@echo ""

nifticdf:nifti
	(cd $(NIFTICDF); $(MAKE) depend; $(MAKE) lib;)
	@echo " ----------- $(NIFTICDF) build completed."
	@echo ""

fslio:	nifti
	(cd $(FSLIO); $(MAKE) depend; $(MAKE) lib;)
	@echo " ----------  $(FSLIO) build completed."
	@echo ""

example:nifti
	(cd $(EXAMPLES); $(MAKE) all;)
	@echo Example programs built.
	@echo ""


doc:
	(cd docs; doxygen Doxy_nifti.txt;)
	@echo "doxygen doc rebuilt, run netscape docs/html/index.html to view."
	@echo ""

$(INSTALL_BIN_DIR):
	mkdir -p $@

$(INSTALL_INC_DIR):
	mkdir -p $@

$(INSTALL_LIB_DIR):
	mkdir -p $@

znz_install: $(INSTALL_INC_DIR) $(INSTALL_LIB_DIR)
	($(CP) $(ZNZ)/*.a $(INSTALL_LIB_DIR); $(CP) $(ZNZ)/*.h $(INSTALL_INC_DIR);)
	$(RANLIB) $(INSTALL_LIB_DIR)/*.a
	@echo " $(ZNZ) installed."
	@echo ""

nifti_install: $(INSTALL_INC_DIR) $(INSTALL_LIB_DIR)
	($(CP) $(NIFTI)/*.a $(INSTALL_LIB_DIR); $(CP) $(NIFTI)/*.h $(INSTALL_INC_DIR);)
	$(RANLIB) $(INSTALL_LIB_DIR)/*.a
	@echo " $(NIFTI) installed."
	@echo ""

nifticdf_install: $(INSTALL_INC_DIR) $(INSTALL_LIB_DIR)
	($(CP) $(NIFTICDF)/*.a $(INSTALL_LIB_DIR); $(CP) $(NIFTICDF)/*.h $(INSTALL_INC_DIR);)
	$(RANLIB) $(INSTALL_LIB_DIR)/*.a
	@echo " $(NIFTI) installed."
	@echo ""

fslio_install: $(INSTALL_INC_DIR) $(INSTALL_LIB_DIR)
	($(CP) $(FSLIO)/*.a $(INSTALL_LIB_DIR); $(CP) $(FSLIO)/*.h $(INSTALL_INC_DIR);)
	$(RANLIB) $(INSTALL_LIB_DIR)/*.a
	@echo " $(FSLIO) installed."
	@echo ""

install_clean:
	($(RM) -f $(INSTALL_INC_DIR)/* $(INSTALL_LIB_DIR)/* $(INSTALL_BIN_DIR)/*;)

znz_clean:
	(cd $(ZNZ); $(RM) -f *.o *.a core; $(RM) -f depend.mk;)

nifti_clean:
	(cd $(NIFTI); $(RM) -f *.o *.a core; $(RM) -f depend.mk;)

nifticdf_clean:
	(cd $(NIFTICDF); $(RM) -f *.o *.a core; $(RM) -f depend.mk;)

fslio_clean:
	(cd $(FSLIO); $(RM) -f *.o *.a core; $(RM) -f depend.mk;)

doc_clean:
	($(RM) -fr docs/html;)

tar:
	(cd .. ; ln -s $(THIS_DIR) ${TARFILE_NAME} ; \
	$(TAR) --exclude=CVS -cf ${TARFILE_NAME}.tar ${TARFILE_NAME}/*; \
	rm -f ${TARFILE_NAME});
	@echo ''
	@echo 'tar file ../${TARFILE_NAME}.tar has been created.'
	@echo ''

mention_cmake:
	@echo ""
	@echo "---------------------------------------------------------"
	@echo "This packages has changed to rely more primarily on cmake,"
	@echo "and the Makefile system needs a little love."
	@echo ""
	@echo "Consider the steps for cmake, if they would be helpful:"
	@echo ""
	@echo "   mkdir cmake_build"
	@echo "   cd cmake_build"
	@echo "   cmake -DBUILD_SHARED_LIBS=ON .."
	@echo "   make"
	@echo ""

help:
	@echo ""
	@echo "all:           build and install znz, nifti1 and fslio libraries"
	@echo "install:       install znz, nifti1, and fslio libraries"
	@echo "doc:           build the doxygen docs for this project."
	@echo "               (Need doxygen installed, see"
	@echo "               http://www.stack.nl/~dimitri/doxygen/index.html"
	@echo "tar:           make a tar file of all nifti_io code, put in .."
	@echo ""
	@echo "clean:         remove .o .a etc. files from source directories"
	@echo "clean_all:     make clean and delete all installed files in bin,"
	@echo "               include and lib, and remove any autogenerated"
	@echo "               docs (docs/html)"
	@echo ""
	@echo "znz:              build the znz library"
	@echo "znz_install:      install the znz library"
	@echo "nifti:            build the nifti1 library"
	@echo "nifti_install:    install the nifti1 library"
	@echo "nifticdf:         build the nifti1 library"
	@echo "nifticdf_install: install the nifti1 library"
	@echo "fslio:            build the fslio library"
	@echo "fslio_install:    install the fslio library"
	@echo "example:          make the example program(s)"
	@echo ""
