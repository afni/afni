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
            tmp3 = self.make_workdir()
            tmp4 = self.copy_input_clust_to_wdir()
            tmp5 = self.copy_input_atlas_to_wdir()

    # ----- methods

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

    def copy_input_atlas_to_wdir(self):
        """Copy input_atlas to the wdir"""

        BAD_RETURN = -1

        # check if dset has labeltable or atlas points
        is_fail, iaol = is_atlas_or_labeltable(self.input_atlas)
        if is_fail :
            ab.EP1("Failed to check atlas/labeltable of input_atlas")
            return -1

        # copy atlas (might need to resample)
        cmd  = '3dTcat -overwrite -prefix "{}" '.format(self.wdir_atlas)
        cmd += '"{}"'.format(self.input_atlas)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Could not copy input_atlas")
            return BAD_RETURN

        # do we need to attach/reattach atlas/lt?
        if iaol :
            is_fail = propagate_copytables(self.input_atlas, self.wdir_atlas)
            if is_fail :
                ab.EP1("Failed to propagate tables of input_atlas")
                return -1

        return 0

    def copy_input_clust_to_wdir(self):
        """Copy input_clust dset to the wdir"""

        BAD_RETURN = -1

        # check if grids match, to either copy or resample
        is_fail, isg = is_same_grid(self.input_clust, self.input_atlas)
        if is_fail :
            ab.EP1("Failed to check grids of input clust and atlas")
            return -1

        # copy clust (might need to resample)
        if isg :
            # just copy
            cmd  = '3dTcat -overwrite -prefix "{}" '.format(self.wdir_clust)
            cmd += '"{}"'.format(self.input_clust)
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
        else :
            # resample
            cmd  = '3dresample -overwrite -prefix "{}" '.format(self.wdir_clust)
            cmd += '-input "{}" '.format(self.input_clust)
            cmd += '-master "{}" '.format(self.input_atlas)
            cmd += '-rmode NN'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()

        if stat :
            ab.EP1("Could not copy input_clust")
            return BAD_RETURN

        return 0

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
            self.workdir = '__wdir_cluster_table_' + rstr

        # convert bool-ish opts to bools
        self.do_clean          = au.convert_to_bool_yn10(self.do_clean)
        self.do_log            = au.convert_to_bool_yn10(self.do_log)
        self.strict_fill_clust = au.convert_to_bool_yn10(self.strict_fill_clust)
        self.dat_col_as_sign   = au.convert_to_bool_yn10(self.dat_col_as_sign)

        return 0


    # ----- decorators

    @property
    def ninput_clust(self):
        """number of input_clusts"""
        return len(self.input_clust)

    @property
    def wdir_clust(self):
        """name of clust dset in workdir"""
        return self.workdir + '/' + 'dset_clust.nii.gz'

    @property
    def wdir_atlas(self):
        """name of atlas dset in workdir"""
        return self.workdir + '/' + 'dset_atlas.nii.gz'


def is_same_grid(A, B):
    """Are the two dsets A and B on the same grid? Check with 3dinfo.

Parameters
----------
A : str
    name of a volumetric dset 
B : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
isg : int
    1 for 'yes, same grid'; 0 for not so

"""

    isg = 0
    BAD_RETURN = (-1, isg)

    cmd  = '3dinfo -same_grid {} {}'.format(A, B)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse: [0]th and [1]th values will be same now
    try:
        isg  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, isg

def is_atlas_or_labeltable(A):
    """Does dset A have an atlas or labeltable?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
iaol : int
    1 for 'yes, has atlas or labeltable'; 0 for not so

"""

    iaol = 0
    BAD_RETURN = (-1, iaol)

    cmd  = '3dinfo -is_atlas_or_labeltable {}'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse: [0]th and [1]th values will be same now
    try:
        iaol  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, iaol

def propagate_copytables(A, B):
    """Propagate copytables (atlas points, labeltable, etc.) from A to B. 

Also sent cmap to INT_CMAP.

Parameters
----------
A : str
    dset from which to propagate copytables
B : str
    dset to which to propagate copytables

Returns
-------
is_fail : int
    0 for success, nonzero for failure

"""

    BAD_RETURN = -1

    # prop tables
    cmd  = '3drefit -copytables {} {}'.format(A, B)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()

    if stat :
        ab.EPI1("Could not refit copytables: {} -> {}".format(A, B))
        return BAD_RETURN

    # int cmap
    cmd  = '3drefit -cmap INT_CMAP {}'.format(B)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()

    if stat :
        ab.EPI1("Could not refit cmap: {}".format(B))
        return BAD_RETURN

    return 0


# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

