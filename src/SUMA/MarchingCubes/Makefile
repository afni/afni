# SUMA modifications:
# Comment out CC
# remove all -pg options for Sun's pleasure

##############################################################################
#
# Makefile for Marching Cubes examples
#
##############################################################################

###################### GENERAL SETTINGS ############################

## LIBS      = -L/usr/lib/ -lm 
LIBS      = $(SUMA_LINK_PATH) -lm 

########################### Programs #################################

#CC        = gcc -Wall ---- Passed from SUMA

########################### Flags #################################

FLAGS     = -I. -I/usr/include

ifeq ($(MAKECMDGOALS),mc)
  OBJDIR  = debug
  CFLAGS  = -g -ggdb $(FLAGS) -DDEBUG
endif
ifeq ($(MAKECMDGOALS),mcR)
  OBJDIR  = release
  CFLAGS  = -O3 $(FLAGS)
endif
ifeq ($(MAKECMDGOALS),mcGL)
  OBJDIR  = debug_GL
  CFLAGS  = -g -ggdb $(FLAGS) -DDEBUG
endif
ifeq ($(MAKECMDGOALS),mcGLR)
  OBJDIR  = release_GL
  CFLAGS  = -O3 $(FLAGS)
endif

$(OBJDIR) :
	mkdir -p $(OBJDIR)


############################  mc  #################################

MC_OBJECTS = \
  $(OBJDIR)/MarchingCubes.o \
  $(OBJDIR)/main.o

mc : $(OBJDIR) $(MC_OBJECTS)
	$(CC) -g -ggdb -o $@ $(MC_OBJECTS) $(LIBS)

mcR : $(OBJDIR) $(MC_OBJECTS)
	$(CC) -O3 -o $@ $(MC_OBJECTS)


###########################  mcGL  ################################

MCGL_OBJECTS = \
  $(OBJDIR)/fparser.o \
  $(OBJDIR)/gl2ps.o \
  $(OBJDIR)/ply.o \
  $(OBJDIR)/MarchingCubes.o \
  $(OBJDIR)/mainGLUI.o


mcGL : $(OBJDIR) $(MCGL_OBJECTS)
	$(CC) -g -ggdb -o $@ $(MCGL_OBJECTS) $(LIBS)

mcGLR : $(OBJDIR) $(MCGL_OBJECTS)
	$(CC) -O3 -o $@ $(MCGL_OBJECTS) $(LIBS)


###########################  luttest  ################################

luttest : LookUpTableTest.cpp LookUpTable.h
	$(CC) -g -ggdb -o $@ $< -I. $(LIBS)

luttestR : LookUpTableTest.cpp LookUpTable.h
	$(CC) -O3 -o $@ $< -I. $(LIBS)


########################### Commands #################################


$(OBJDIR)/%.o : %.c
	$(CC) -c $< -o $@ $(CFLAGS)

########################### Tar file #################################

DIRNAME = $(notdir ${PWD})
TARNAME = ../$(shell echo $(DIRNAME) | tr [:upper:] [:lower:]).tgz

$(TARNAME) : README Makefile *.h *.cpp *.c *.dsw *.dsp
	tar -C.. -c -v -z -f $@ $(foreach file,$^,$(DIRNAME)/$(file)) > /dev/null


########################### Directives #################################

.PHONY: all clean depend tar

tar : $(TARNAME)

depend:
	makedepend -p"\$$(OBJDIR)/" -Y -I. $(wildcard *.cpp) $(wildcard *.c) >& /dev/null

clean:
	rm -f */*.o core *~ gmon.out

all :
	make -k mc
	make -k mcGL
	make -k luttest
	make -k mcR
	make -k mcGLR
	make -k luttestR
	make tar


# DO NOT DELETE

$(OBJDIR)/fparser.o: fparser.h
$(OBJDIR)/LookUpTableTest.o: LookUpTable.h
$(OBJDIR)/main.o: MarchingCubes.h
$(OBJDIR)/mainGLUI.o: csg.h fparser.h MarchingCubes.h
$(OBJDIR)/MarchingCubes.o: MarchingCubes.h LookUpTable.h
$(OBJDIR)/gl2ps.o: gl2ps.h
$(OBJDIR)/ply.o: ply.h
