#!/usr/bin/env python

# A library of functions for dealing with 3dBrainTeaser
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : *** TEMPLATE ***
# ============================================================================

import sys, os, copy, glob

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au
from   afnipy import lib_vnet_defs      as DEF

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
        self.user_opts       = DEF.DOPTS['user_opts']  # command the user ran
        self.user_inobj      = user_inobj

        # general variables
        self.verb            = DEF.DOPTS['verb']
        self.overwrite       = DEF.DOPTS['overwrite']
        self.do_clean        = DEF.DOPTS['do_clean']
        self.do_log          = DEF.DOPTS['do_log']

        # main data variables
        self.inset           = DEF.DOPTS['inset']
        self.prefix          = DEF.DOPTS['prefix']
        self.mask            = DEF.DOPTS['mask']
        self.checkpoint      = DEF.DOPTS['checkpoint']

        self.device          = DEF.DOPTS['device']

        # control variables
        self.outdir          = DEF.DOPTS['outdir']         # None or str
        self.workdir         = DEF.DOPTS['workdir']

        # ----- take action(s)

        # prelim stuff
        if user_inobj :
            tmp1 = self.load_from_inopts()
            if tmp1 : return

            tmp2 = self.basic_setup()
            if tmp2 : return

            tmp3 = self.make_workdir()
            if tmp3 : return

            # ****

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
        if io.mask is not None :
            self.mask = io.mask
        if io.checkpoint is not None :
            self.checkpoint = io.checkpoint

        if io.device is not None :
            self.device = io.device

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

        # (opt) mask
        if self.mask :
            nfail = au.check_all_dsets_exist([self.mask], label='mask',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load mask")

        # (opt) checkpoint
        if self.checkpoint :
            is_ok = os.path.isfile(self.checkpoint)
            if not(is_ok) :
                ab.EP("Failed to load mask")

        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")

        if self.device not in DEFS.LIST_all_device :
            msg = "Unrecognized device '{}'. ".format(self.device)
            msg+= "Must use one from list: {}".format(DEFS.STR_all_device)
            ab.EP(msg)

        # generate basic items

        # generate wdir with random component, if none provided
        if not(self.workdir) :
            cmd  = '3dnewid -fun11'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            rstr = com.so[0].strip()
            self.workdir = '__wdir_TEMPLATE_' + rstr

        # convert bool-ish opts to bools
        self.do_clean       = au.convert_to_bool_yn10(self.do_clean)

        return 0

    def make_workdir(self):
        """Make the workdir"""

        BAD_RETURN = -10

        cmd  = '\\mkdir -p "{}" '.format(self.workdir)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Could not make workdir")
            return BAD_RETURN

        return 0

    # ----- decorators

    #@property
    def ninset(self):
        """number of insets"""
        return len(self.inset)

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

