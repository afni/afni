#!/usr/bin/env python

# A library of functions for dealing with gen_cluster_table.py
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : library for gen_cluster.py
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
    'input_clust'     : '',
    'input_atlas'     : '',
    'input_dat'       : '',
    'prefix'          : '',
    'outdir'          : None,
    'workdir'         : '',
    'min_fill_clust'  : 10.0,
    'min_fill_atlas'  : 25.0,
    'strict_fill_clust' : 'No',
    'dat_col_as_sign' : 'No',
}

# ----------------------------------------------------------------------------


# ============================================================================

class MainObj:
    """Object for gen_cluster_table.py

Parameters
----------
inobj : InOpts object 
    object constructed from running gen_cluster_table.py on the command
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
        self.input_clust     = DOPTS['input_clust']
        self.input_atlas     = DOPTS['input_atlas']
        self.input_dat       = DOPTS['input_dat']
        self.prefix          = DOPTS['prefix']

        # control variables
        self.outdir          = DOPTS['outdir']         # None or str
        self.workdir         = DOPTS['workdir']
        self.min_fill_clust  = DOPTS['min_fill_clust']
        self.min_fill_atlas  = DOPTS['min_fill_atlas']
        self.strict_fill_clust = DOPTS['strict_fill_clust']
        self.dat_col_as_sign = DOPTS['dat_col_as_sign']

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
        if io.input_clust is not None :
            self.input_clust = io.input_clust
        if io.input_atlas is not None :
            self.input_atlas = io.input_atlas
        if io.input_dat is not None :
            self.input_dat = io.input_dat
        if io.prefix is not None :
            self.prefix = io.prefix

        # control variables
        if io.workdir is not None :
            self.workdir = io.workdir
        if io.outdir is not None :          # or this could be derived
            self.outdir = io.outdir

        if io.min_fill_clust is not None :
            self.min_fill_clust = io.min_fill_clust
        if io.min_fill_atlas is not None :
            self.min_fill_atlas = io.min_fill_atlas
        if io.strict_fill_clust is not None :
            self.strict_fill_clust = io.strict_fill_clust

        if io.dat_col_as_sign is not None :
            self.dat_col_as_sign = io.dat_col_as_sign

        return 0


    def basic_setup(self):
        """Run through basic checks of what has been input, and fill in any
        further information that is needed (like wdir, etc.)"""

        # check basic requirements

        if not(self.input_clust) :
            ab.EP("Need to provide an input_clust")
        else:
            nfail = au.check_all_dsets_exist([self.input_clust], 
                                             label='input_clust', 
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load input_clust")

        if not(self.input_atlas) :
            ab.EP("Need to provide an input_atlas")
        else:
            nfail = au.check_all_dsets_exist([self.input_atlas], 
                                             label='input_atlas', 
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load input_atlas")

        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")

        # checks of params
        if self.min_fill_clust < 0.0 :
            ab.EP("The min_fill_clust must be >=0")
        if self.min_fill_atlas < 0.0 :
            ab.EP("The min_fill_atlas must be >=0")

        # generate wdir with random component, if none provided
        if not(self.workdir) :
            cmd  = '3dnewid -fun11'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            rstr = com.so[0].strip()
            self.workdir = '__wdir_obliquity_' + rstr

        # convert bool-ish opts to bools
        self.do_clean          = au.convert_to_bool_yn10(self.do_clean)
        self.do_log            = au.convert_to_bool_yn10(self.do_log)
        self.strict_fill_clust = au.convert_to_bool_yn10(self.strict_fill_clust)
        self.dat_col_as_sign   = au.convert_to_bool_yn10(self.dat_col_as_sign)

        return 0


    # ----- decorators

    #@property
    def ninput_clust(self):
        """number of input_clusts"""
        return len(self.input_clust)

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

