#!/usr/bin/env python

# A library of functions for dealing with gen_cluster_table.py.
# Specifically, this file contains the main object for running the
# whole operation.
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : library for gen_cluster.py
# ============================================================================

import sys, os, copy, glob
import math

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au
from   afnipy import lib_cluster_rois   as lcr

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
    'min_perc_clust'  : 10.0,
    'min_perc_atlas'  : 25.0,
    'strict_fill_clust' : 'No',
    'olap_logic'      : 'or',
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

        # extra vars
        self.clust_min       = 0
        self.clust_max       = 0
        self.clust_list      = []
        self.clust_nzero     = 1
        self.input_dat_nv    = 0                     # num of vol in input_dat

        # control variables
        self.outdir            = DOPTS['outdir'] 
        self.workdir           = DOPTS['workdir']
        self.min_perc_clust    = DOPTS['min_perc_clust']
        self.min_perc_atlas    = DOPTS['min_perc_atlas']
        self.olap_logic        = DOPTS['olap_logic']
        self.strict_fill_clust = DOPTS['strict_fill_clust']
        self.dat_col_as_sign   = DOPTS['dat_col_as_sign']

        # ----- take action(s)

        # prelim stuff
        if user_inobj :
            tmp1 = self.load_from_inopts()
            if tmp1 : return

            tmp2 = self.basic_setup()
            if tmp2 : return

            tmp3 = self.make_workdir()
            if tmp3 : return

            tmp4 = self.copy_input_clust_to_wdir()
            if tmp4 : return

            tmp5 = self.copy_input_atlas_to_wdir()
            if tmp5 : return

            if self.input_dat :
                tmp5b = self.check_input_dat()
                if tmp5b : return

            tmp6 = self.make_clust_list()
            if tmp6 : return

            tmp7 = self.set_label_size()
            if tmp7 : return

            tmp8 = self.calc_overlap_metrics_all()
            if tmp8 : return

    # ----- methods

    def check_input_dat(self):
        """Is the input grid valid? To answer this, we first see if it is the
        same grid as input_clust.  We then also verify that there is
        only value in input_dat, so as not to get confused

        NB: this dset (if supplied) will not be resampled, and it will
        be used with the un-resampled input_clust dset.

        """

        is_fail, isg = is_same_grid(self.input_clust, self.input_dat)

        if is_fail :
            ab.EP1("Failed to check grids of input_clust and input_dat")
            return -1

        if not(isg) :
            ab.EP1("Grid mismatch for: input_clust and input_dat")
            return -2

        is_fail, self.input_dat_nv = get_nv(self.input_dat)

        if is_fail :
            ab.EP1("Failed to get nv for input_dat")
            return -1

        if self.input_dat_nv :
            ab.WP("The input_dat dset has >1 value; we only use [0]th")

        return 0

    def calc_input_dat_mean_roi(self, clust):
        """For a given clust, calculate the mean value within input_dat. 
        
        NB: this calc uses ORIGINAL dsets, not resampled ones.

        Returns
        -------
        is_fail : int
            0 for succeed, nonzero for failure
        clust_dat : float
            mean value of input_dat within clust
        """

        BAD_RETURN = (-6, 0.0)
        
        # might need a subbrick selector, if there are input_dat's nv>1 
        if self.input_dat_nv > 1 : 
            subbb = "[0]"
        else:
            subbb = ""
        
        cmd  = '''3dROIstats -quiet '''
        cmd += '''-mask "{}<{}>" '''.format(self.input_clust, clust)
        cmd += '''"{}{}" '''.format(self.input_dat, subbb)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()

        if stat :
            ab.EP1("Could not calc input_dat ave, clust: {}".format(clust))
            return BAD_RETURN
        
        try:
            clust_dat = float(com.so[0].strip())
        except:
            ab.EP1("Could not parse input_dat ave, clust: {}".format(clust))
            return BAD_RETURN

        return 0, clust_dat

    def run_tableize_roi(self, clust):
        """For a given clust, populate relevant overlap info with the
        input_atlas.
        """

        BAD_RETURN = -8

        # don't think zeropadding is so necessary? these should
        # effectively just be temporary files

        label = 'cl_{:>0{}d}'.format(clust, self.clust_nzero)

        # make a temp dset of one clust (=onecl)

        cmd  = '''3dcalc -overwrite '''
        cmd += '''-a "{}" '''.format(self.wdir_clust)
        cmd += '''-expr "equals(a,{})" '''.format(clust)
        cmd += '''-prefix "{}" '''.format(self.wdir_onecl)
        cmd += '''-datum byte -nscale'''
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not make dset of one clust: {}".format(clust))
            return BAD_RETURN

        # use onecl to mask atlas dset

        cmd  = '''3dcalc -overwrite '''
        cmd += '''-a "{}" '''.format(self.wdir_atlas)
        cmd += '''-b "{}" '''.format(self.wdir_onecl)
        cmd += '''-expr "a*step(b)" '''
        cmd += '''-prefix "{}" '''.format(self.wdir_atlas_onecl)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not mask atlas with onecl dset: {}".format(clust))
            return BAD_RETURN

        # ... and reattach labels and header stuff
        is_fail = propagate_copytables(self.wdir_atlas, self.wdir_atlas_onecl)
        if is_fail :
            ab.EP1("Failed to propagate tables to atlas_onecl")
            return -1

        # calc tableize info: relative sizes of overlaps

        otable1 = self.wdir_table_root(1) + label + '.dat'
        olog1   = self.wdir_table_root(1) + label + '_log.txt'

        cmd  = '''adjunct_aw_tableize_roi_info.py '''
        cmd += '''"{}" '''.format(otable1)
        cmd += '''"{}" '''.format(self.wdir_atlas_onecl)
        cmd += '''"{}" '''.format(self.wdir_onecl)
        cmd += '''"{}" '''.format(self.wdir_atlas)
        cmd += '''"{}" '''.format(self.wdir_atlas)
        cmd += '''0 '''
        cmd += ''' > {}'''.format(olog1)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not mask atlas w/ onecl for clust: {}".format(clust))
            return BAD_RETURN

        # do this to get line number where table of numbers and ROI
        # labels starts

        cmd  = '''grep -nE "ROI_value|Label_str" "{}" '''.format(otable1)
        cmd += '''| cut -d: -f1 | tail -n 1 '''
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not get line numbers for clust: {}".format(clust))
            return BAD_RETURN

        try:
            nnn = int(com.so[0].strip())
        except:
            msg = "Could not convert line number '{}' ".format(nnn)
            mst+= "to int, for clust: {}".format(clust)
            ab.EP1(msg)
            return BAD_RETURN

        # make minitable of only numbers
        
        otable2 = self.wdir_table_root(2) + label + '.dat'

        cmd  = '''1dcat "{}" > "{}" '''.format(otable1, otable2)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not make (mini)table2 for clust: {}".format(clust))
            return BAD_RETURN

        # figure out first and last lines of table for sed command to
        # select it; if ntable=0, then there are no olaps

        cmd  = '''cat "{}" | wc -l '''.format(otable2)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not get ntable in table2 for clust: {}".format(clust))
            return BAD_RETURN

        try:
            ntable  = int(com.so[0].strip())
        except:
            msg = "Could not convert ntable '{}' ".format(ntable)
            mst+= "to int, for clust: {}".format(clust)
            ab.EP1(msg)
            return BAD_RETURN
        
        # this should never happen, because we do no thresholding when
        # generating table2
        if not(ntable) :
            return 0

        row_top = nnn + 2
        row_bot = row_top + ntable - 1

        # get list of ROI labels (save as var): apply selector, and
        # open up last column of labels for selection

        cmd  = '''sed -n "{},{}p" '''.format(row_top, row_bot)
        cmd += '''"{}" '''.format(otable1)
        cmd += '''| awk '{{print $11}}' '''.format(otable1)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not get atlas labels for clust:", clust)
            return BAD_RETURN

        table_labels = copy.deepcopy(com.so)

        # if input_dat is used, get average value within onecl

        if self.input_dat :
            is_fail, clust_dat = self.calc_input_dat_mean_roi(clust)
            if is_fail :
                ab.EP1("Could not calc input_dat ave, clust: {}".format(clust))
                return BAD_RETURN

            print("   -> mean clust: {:.3e}".format(clust_dat))
        else:
            clust_dat = None

        # read in mini clust-table to a list of int+float

        is_fail, table = read_mini_table_info(otable2)
        if is_fail :
            ab.EP1("Could not calc input_dat ave, clust: {}".format(clust))
            return BAD_RETURN
        
        #print("HEY")
        #print(table)
        #print("HEY2")
        #print(table_labels)
        # *** need to attach info to new object, to save it all
        asdf = lcr.ClustRegionObj(clust, table, table_labels, 
                                  min_fill_clust    = self.min_fill_clust, 
                                  min_fill_atlas    = self.min_fill_atlas,
                                  olap_logic        = self.olap_logic,
                                  strict_fill_clust = self.strict_fill_clust,
                                  clust_dat         = clust_dat,
                                  dat_col_as_sign   = self.dat_col_as_sign)

        
        return 0

    def calc_overlap_metrics_all(self):
        """Loop over all clusters in clust_list and look for their overlap
        with the input atlas.
        """

        BAD_RETURN = -7

        ab.IP("Calc cluster overlap metrics")
        for clust in self.clust_list:
            if self.verb :
                print("   cluster value:", clust, flush=True)
            is_fail = self.run_tableize_roi(clust)
            if is_fail :
                ab.EP1("Could not calc olap info for clust: {}".format(clust))
                return BAD_RETURN

        return 0

    def make_clust_list(self):

        """Derive one list of all (pos and/or neg) clusters, from the
        input_clust. NB: we are using the unresampled dset for this
        list, to make sure no clusters are lost at this stage.

        While earlier versions used 3dBrickStat to get min/max ranges
        and make separate pos/neg clusters, there doesn't actually
        seem to be much reason to do that extra work. Soooo we don't now.

        We sort as follows: ascending values from 1, then all negative
        values. So, like:  1, 2, 3, 25, 87, -1, -6.

        """

        BAD_RETURN = -6

        clust_pos = []
        clust_neg = []

        # get list of all ROIs from this

        cmd  = '''3dROIstats -quiet '''
        cmd += '''-mask "3dcalc( -a {} '''.format(self.input_clust)
        cmd += '''-expr a*bool(a) )" '''
        cmd += '''"{}" '''.format(self.input_clust)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        
        if stat :
            ab.EP1("Could not get clust list with 3dROIstats")
            return BAD_RETURN

        list_roi = com.so[0].split()

        # translate str to int

        try:
            for roi in list_roi:
                val, vtype = au.try_convert_bool_float_int_str(roi,
                                                    int_val_is_int=True)
                if vtype != 'int' :
                    msg = "Non-int val for pos clust list: {}".format(val)
                    ab.EP1()
                    return BAD_RETURN
                if val > 0 :
                    clust_pos.append(val)
                else:
                    clust_neg.append(val)
        except:
            ab.EP1("Could not parse clust list")
            return BAD_RETURN

        # sort and combine

        if len(clust_pos) :
            clust_pos.sort()
            self.clust_list.extend(clust_pos)

        if len(clust_neg) :
            clust_neg.sort(reverse=True)
            self.clust_list.extend(clust_neg)

        if self.nclust :
            self.clust_min = min(self.clust_list)
            self.clust_max = max(self.clust_list)

        if self.verb:
            msg = "Found {} clusters, ".format(self.nclust)
            msg+= "in interval: [{}, {}]".format(self.clust_min, self.clust_max)
            ab.IP(msg)

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

            # check if atlas but no lt
            is_fail, ilt = is_labeltable(self.input_atlas)
            if is_fail :
                ab.EP1("Failed to check labeltable of input_atlas")
                return -1

            if not(ilt) :
                # no lt, but must have at atlas, so make adjunct lt
                cmd  = 'adjunct_atlas_points_to_labeltable '
                cmd += '-input "{}" '.format(self.wdir_atlas)
                cmd += '-add_lt_to_input'
                com  = ab.shell_com(cmd, capture=1)
                stat = com.run()

                if stat :
                    ab.EP1("Could not create lt from atlas")
                    return BAD_RETURN

                cmd  = '@MakeLabelTable '
                cmd += '-atlasize_labeled_dset "{}"'.format(self.wdir_atlas)
                com  = ab.shell_com(cmd, capture=1)
                stat = com.run()

                if stat :
                    ab.EP1("Could not atlasize labeled dset")
                    return BAD_RETURN

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

        if io.min_perc_clust is not None :
            self.min_perc_clust = io.min_perc_clust
        if io.min_perc_atlas is not None :
            self.min_perc_atlas = io.min_perc_atlas
        if io.olap_logic is not None :
            self.olap_logic = io.olap_logic
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
        if self.min_perc_clust < 0.0 :
            ab.EP("The min_perc_clust must be >=0")
        if self.min_perc_atlas < 0.0 :
            ab.EP("The min_perc_atlas must be >=0")

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

    def set_label_size(self):
        """Use the clust_min and clust_max to figure out how many zeros we
        need to pad with for cluster labels."""

        BAD_RETURN = -2

        # defaults
        powmin = 1
        powmax = 1

        # use powers of ten of clust values to get nvox
        if self.clust_min :
            # add one bc of neg sign
            powmin = int(math.ceil(math.log10(abs(self.clust_min)))) + 1
        if self.clust_max :
            powmax = int(math.ceil(math.log10(abs(self.clust_max))))

        self.clust_nzero = max(powmin, powmax)

        return 0

    def wdir_table_root(self, idx):
        """name of temporary dset in workdir---initial table"""

        return self.workdir + '/' + 'table_{:02d}_'.format(idx)

    # ----- decorators

    @property
    def ninput_clust(self):
        """number of input_clusts; should always be 1"""
        return len(self.input_clust)

    @property
    def wdir_clust(self):
        """name of clust dset in workdir"""
        return self.workdir + '/' + 'dset_clust_00_input.nii.gz'

    @property
    def wdir_atlas(self):
        """name of atlas dset in workdir"""
        return self.workdir + '/' + 'dset_atlas_00_input.nii.gz'

    @property
    def nclust(self):
        """number of clusters, based on clust_list_all"""
        return len(self.clust_list)

    @property
    def wdir_onecl(self):
        """name of temporary dset in workdir---has just one clust"""
        return self.workdir + '/' + 'dset_clust_01_onecl.nii.gz'

    @property
    def wdir_atlas_onecl(self):
        """name of temporary dset in workdir---atlas masked by onecl"""
        return self.workdir + '/' + 'dset_atlas_01_onecl.nii.gz'

    @property
    def min_fill_clust(self):
        """fractional form of min_perc_clust"""
        return self.min_perc_clust / 100.0

    @property
    def min_fill_atlas(self):
        """fractional form of min_perc_atlas"""
        return self.min_perc_atlas / 100.0


def read_mini_table_info(fname):
    """This function is for reading in a text file with name fname that
was made from extracting the non-commented, purely-numerical part of a
an overlap table created by adjunct_aw_tableize_roi_info.py.

It simply returns that information as a "2D" list of ints and floats.

Parameters
----------
fname : str
    name of text file

Returns
-------
is_fail : int
    0 for success, nonzero for failure
L : list
    contains a list of lists, each of whose elements are floats and ints

    """

    L = []
    BAD_RETURN = (-11, L)

    # verify existence
    if not(os.path.exists(fname)) :
        ab.EP1("Cannot find mini table file: {}".format(fname))
        return BAD_RETURN

    fff = open(fname, 'r')
    dat = fff.readlines()
    fff.close()

    ndat = len(dat)

    # look for rows with data, and convert all elements to int or float
    for ii in range(ndat):
        row = dat[ii].split()
        if len(row) :
            R = []
            for r in row :
                v, vtype = au.try_convert_bool_float_int_str(r)
                if not(vtype in ['int', 'float']) :
                    msg = "Table conversion failure. "
                    msg+= "Could not convert '{}' ".format(r)
                    msg+= "to float or int, was: {}".format(vtype)
                    ab.EP1(msg)
                    return BAD_RETURN
                R.append(v)
            L.append(R)

    return 0, L

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
val : int
    1 for 'yes, has atlas or labeltable'; 0 for not so

"""

    val = 0
    BAD_RETURN = (-1, val)

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
        val  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, val

def get_nv(A):
    """How many values (or volumes) in dset A?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
val : int
    number of volumes or values ('-nv', in 3dinfo parlance)

"""

    val = 0
    BAD_RETURN = (-1, val)

    cmd  = '3dinfo -nv "{}"'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse
    try:
        val  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, val

def is_labeltable(A):
    """Does dset A have a labeltable?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
val : int
    1 for 'yes, has labeltable'; 0 for not so

"""

    val = 0
    BAD_RETURN = (-1, val)

    cmd  = '3dinfo -is_labeltable "{}"'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse: single int value in a list
    try:
        val  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, val


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
    cmd  = '3drefit -copytables "{}" "{}"'.format(A, B)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()

    if stat :
        ab.EPI1("Could not refit copytables: {} -> {}".format(A, B))
        return BAD_RETURN

    # int cmap
    cmd  = '3drefit -cmap INT_CMAP "{}"'.format(B)
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

