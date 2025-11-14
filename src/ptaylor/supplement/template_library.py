#!/usr/bin/env python

# A library of functions for dealing with ***TEMPLATE
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : *** TEMPLATE ***
# ============================================================================

import sys, os, copy, glob

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au

# ============================================================================

# default values for the main obj
DOPTS = {
    'user_opts'       : [],         # command the user ran
    'verb'            : 1,
    'overwrite'       : '',
    'do_clean'        : 'Yes',
    'do_log'          : False,
    'inset'           : '',
    'prefix'          : '',
    'outdir'          : None,
    'workdir'         : '',
}

# ----------------------------------------------------------------------------


# ============================================================================

class MainObj:
    """Object for ***

Parameters
----------
inobj : InOpts object 
    object constructed from running TEMPLATE_tool.py on the command
    line. At present, the only way to really provide inputs here.

    """

    def __init__(self, user_inobj=None):

        # ----- set up attributes

        # main input variables
        self.status          = 0                       # not used
        self.user_opts       = DOPTS['user_opts']      # command the user ran
        self.user_inobj      = user_inobj

        # general variables
        self.verb            = DOPTS['verb']
        self.overwrite       = DOPTS['overwrite']
        self.do_clean        = DOPTS['do_clean']
        self.do_log          = DOPTS['do_log']

        # main data variables
        self.inset           = DOPTS['inset']
        self.prefix          = DOPTS['prefix']

        # control variables
        self.outdir          = DOPTS['outdir']         # None or str
        self.workdir         = DOPTS['workdir']

        # ----- take action(s)

        # prelim stuff
        if user_inobj :
            tmp1 = self.load_from_inopts()
            tmp2 = self.basic_setup()

    # ----- methods

    def load_from_inopts(self):
        """Populate the input values using the command line interface
        input. The user information is provided as the self.user_inobj
        object, which gets parsed and redistributed here.
        """
        
        if not(self.user_inobj) :
            ab.WP("No user_inobj? Nothing to do.")
            return 0

        # shorter name to use, and less confusing with 'self' usage
        io = self.user_inobj

        if io.user_opts is not None :
            self.user_opts = io.user_opts

        # general variables
        if io.verb is not None :
            self.verb = io.verb
        if io.overwrite is not None :
            self.overwrite = io.overwrite
        if io.do_clean is not None :
            self.do_clean = io.do_clean
        if io.do_log is not None :
            self.do_log = io.do_log

        # main data variables
        if io.inset is not None :
            self.inset = io.inset
        if io.prefix is not None :
            self.prefix = io.prefix

        # control variables
        if io.workdir is not None :
            self.workdir = io.workdir
        if io.outdir is not None :          # or this could be derived
            self.outdir = io.outdir

        return 0


    def basic_setup(self):
        """Run through basic checks of what has been input, and fill in any
        further information that is needed (like wdir, etc.)"""

        # check basic requirements

        if not(self.inset) :
            ab.EP("Need to provide an inset")
        else:
            nfail = au.check_all_dsets_exist([self.inset], label='inset', 
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load inset")

        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")

        # generate basic items

        # generate wdir with random component, if none provided
        if not(self.workdir) :
            cmd  = '3dnewid -fun11'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            rstr = com.so[0].strip()
            self.workdir = '__wdir_obliquity_' + rstr

        # convert bool-ish opts to bools
        self.do_clean       = au.convert_to_bool_yn10(self.do_clean)

        return 0


    # ----- decorators

    #@property
    def ninset(self):
        """number of insets"""
        return len(self.insets)

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

