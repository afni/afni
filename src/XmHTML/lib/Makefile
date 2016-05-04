##############################################################################
#                                                                            #
# Makefile.motif: libXmHTML, Motif version                                   #
# Adjust at will                                                             #
#                                                                            #
##############################################################################
#                                                                            #
# (C)Copyright 1995-1997 Ripley Software Development                         #
# All Rights Reserved                                                        #
#                                                                            #
# This file is part of the XmHTML Widget Library.                            #
#                                                                            #
# This library is free software; you can redistribute it and/or              #
# modify it under the terms of the GNU Library General Public                #
# License as published by the Free Software Foundation; either               #
# version 2 of the License, or (at your option) any later version.           #
#                                                                            #
# This library is distributed in the hope that it will be useful,            #
# but WITHOUT ANY WARRANTY; without even the implied warranty of             #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          #
# Library General Public License for more details.                           #
#                                                                            #
# You should have received a copy of the GNU Library General Public          #
# License along with this library; if not, write to the Free                 #
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         #
##############################################################################
#
LIBRARY=libXmHTML.a

# Target to make
TARGET=$(LIBRARY)

# Subdirectories to visit
SUBDIRS= common $(PLATFORM)

# To guarantee robust "set -x" operation
export PS4=+

# Target rules
all:: $(TARGET)

# Separate rules for all targets 
$(PLATFORM):: 
	@(set -x; cd $(PLATFORM); $(MAKE) all $(pass_flags) )

common::
	@(set -x; cd common; $(MAKE) all $(pass_flags) )

$(TARGET):: common $(PLATFORM) 
	$(RM) $@ \ 
	$(AR) $@ common/*.o $(PLATFORM)/*.o
	$(RANLIB) $@
	
depend:: 
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) depend $(pass_flags)) ; \
	done)

clean:: 
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) clean $(pass_flags) ) ; \
	done)
	$(RM) $(TARGET)

distclean:: 
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) distclean $(pass_flags) ) ; \
	done)
	$(RM) $(TARGET)
	$(RM) core *.out *.log make.world *.bak *.last *.auto *.rej *.orig
	$(CP) Makefile.org Makefile

