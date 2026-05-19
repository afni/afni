#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
from afnipy import option_list   as OL
from afnipy import afni_util     as UTIL
from afnipy import afni_base     as BASE
from afnipy import lib_cluster_table as LCT

# ----------------------------------------------------------------------
# globals

g_help_string = """Overview ~1~

This program is for making a cluster overlap table. The user inputs a
map of clusters (each defined by a set of voxels with integer values)
and a reference atlas (preferably one with region labels). Then this
program writes out a multicolumn table listing relevant overlaps that
each cluster has, relative to the atlas.

auth = PA Taylor (SSCC, NIMH, NIH, USA)

------------------------------------------------------------------------

Usage ~1~

-input_clust IC :(req) input dataset: the map of clusters, of which
                 you want to list out overlaps. Should be a single 3D
                 volume.

-input_atlas IA :(req) input dataset: the reference atlas, to be used
                 to identify/list overlaps from the cluster input

-prefix PPP     :(req) output name for the table, so likely should end
                 with ".txt" or ".dat", for clarity

-min_perc_clust MFC :this parameter defines the minimum percent of a
                 cluster that must be filled by a reference atlas
                 region to be included in the table.
                 That is, if MFC% or more of the cluster overlaps with
                 a given region, then that region will be listed.
                 (def: {min_perc_clust}) 
                 **See Notes, below, for more about this**

-min_perc_atlas MFA :this parameter defines the minimum percent of a
                 reference atlas region that must be filled by a cluster 
                 to be included in the table.
                 That is, if MFA% or more of the atlas region overlaps
                 with a given cluster, then that region will be listed.
                 (def: {min_perc_atlas}) 
                 **See Notes, below, for more about this**

-olap_logic OL  :specify the logical operator that should be used to 
                 combine the cluster- and atlas-filling conditions.
                 Choices come from the following: and, or.
                 (def: {olap_logic}) 

-strict_fill_clust SFC :by default, if no atlas region overlaps with
                 the '-min_perc_clust ..' threshold value, then the
                 atlas region with maximum overlap will be displayed
                 still; use this option, however, to strictly apply
                 the threshold, so no ROI would be shown.
                 (def: {strict_fill_clust}) 

-input_dat  ID  :by default, there is no info about the mean or
                 sign of a given cluster. With this option, users
                 provide an extra input dataset for that info. The ID
                 dataset must be on the same grid as the '-input_clust
                 ..' dset.  Using this opt will add a column of mean
                 values of the ID data per cluster written using
                 scientific notation (see '-dat_col_as_sign' for
                 alternative reporting).  NB: there is no check that
                 the sign of values are constant across each cluster,
                 though that likely should be the case.  name of the
                 input dataset

-dat_col_as_sign DCAS 
                :if using '-input_dat ..', should the results be output
                 as a simple string value of 'pos', 'neg' or 'zero'?
                 If that option is not chosen, then the input_dat is 
                 reported numerically as the mean value of the data 
                 in the cluster, using scientific notation
                 (def: {dat_col_as_sign}) 

-workdir WD     :working directory name, without path; the working dir
                 will be subdirectory of the output location
                 (def: name with random chars)

-do_clean DC    :state whether to clean up any intermediate files;
                 allowed values are:  Yes, 1, No, 0
                 (def: '{do_clean}')

-do_log         :add this opt to turn on making a text log of all the
                 shell commands that are run when this program is
                 executed.  Mainly for debugging purposes.

-help, -h       :display program help file

-hist           :display program history

-ver            :display program version number

-verb  VVV      :control verbosity (def: {verb})

-show_valid_opts :show valid options for this program

------------------------------------------------------------------------

Notes ~1~

Thresholds for filling clusters and atlas regions ~2~

Probably the biggest choice that users will have to make when using
this programs is deciding what the cut-off value will be for "enough"
overlap to be counted in the cluster report table.  There are two
primary parameters for specifying this:

  '-min_perc_clust ..' : what percentage of a cluster should be filled
                         by a given atlas region to be included

  '-min_perc_atlas ..' : what percentage of an atlas region should be 
                         filled by a given cluster to be included

Having both of these parameters available is useful particularly when
clusters are much larger than atlas regions, or vice versa.  (An
earlier version of this program only had the cluster-fill condition,
and it became obvious that the atlas-based one would be necessary to
add.)  There are default values for each (min_perc_clust = {min_perc_clust}%, 
and min_perc_atlas = {min_perc_atlas}%), which seem a reasonable starting point,
but these were not chosen for deep reasons. Users should choose what 
makes most sense for their data.

There is an important secondary parameter related to the above options:

  '-olap_logic ..'     : should the cluster- and atlas-filling conditions
                         be applied with 'or' logic (so, only one condition
                         needs to be met) for an overlap to be included
                         in the table, or with 'and' logic (so both
                         conditions need to be met simultaneously).

The default logic is '{olap_logic}'.  Again, users may have their own
preferences for their study design and reporting.

------------------------------------------------------------------------

Examples ~1~

 1. Make cluster overlap table using basic/default parameters:

    gen_cluster_table.py                                \\
        -input_clust      Clust+tlrc.HEAD               \\
        -input_atlas      MNI_Glasser_HCP_v1.0.nii.gz   \\
        -prefix           Clust_01_report.dat

 2. Make cluster overlap table using different cluster and altas
    region fill thresholds:

    gen_cluster_table.py                                \\
        -input_clust      Clust+tlrc.HEAD               \\
        -input_atlas      MNI_Glasser_HCP_v1.0.nii.gz   \\
        -min_perc_clust   20.0                          \\
        -min_perc_atlas   50.0                          \\
        -prefix           Clust_02_report.dat

 3. Include another dataset, to report the sign of the cluster
    (as a string):

    gen_cluster_table.py                                \\
        -input_clust      Clust+tlrc.HEAD               \\
        -input_atlas      MNI_Glasser_HCP_v1.0.nii.gz   \\
        -input_dat        stats.beta_values.nii.gz      \\
        -dat_col_as_sign  Yes                           \\
        -prefix           Clust_03_report.dat

""".format(**LCT.DOPTS)

g_history = """
  **TEMPLATE_tool.py** history:

  0.1   Sep 25, 2025 :: started this command line interface 
"""

g_prog    = g_history.split()[0]
g_ver     = g_history.split("\n")[-2].split("::")[0].strip()
g_version = g_prog + " version " + g_ver

class InOpts:
    """Object for storing any/all command line inputs, and just checking
that any input files do, in fact, exist.  Option parsing and other
checks happen in a subsequent object.

    """

    def __init__(self):
        # main variables
        self.status          = 0                       # exit value
        self.valid_opts      = None
        self.user_opts       = None

        # general variables
        self.verb            = LCT.DOPTS['verb']
        self.do_clean        = None
        self.overwrite       = None
        self.do_log          = None

        # main data variables
        self.input_clust     = None
        self.input_atlas     = None
        self.input_dat       = None
        self.prefix          = None

        # control variables
        self.workdir         = None
        self.outdir          = None
        self.min_perc_clust  = None
        self.min_perc_atlas  = None
        self.strict_fill_clust = None
        self.olap_logic      = None
        self.dat_col_as_sign = None


        # ----- take action(s)

        # prelim stuff
        tmp1 = self.init_options()

    # ----- methods

    def init_options(self):
        """
        Prepare the set of all options, with very short help descriptions
        for each.
        """

        self.valid_opts = OL.OptionList('valid opts')

        # short, terminal arguments

        self.valid_opts.add_opt('-help', 0, [],           \
                        helpstr='display program help')
        self.valid_opts.add_opt('-hist', 0, [],           \
                        helpstr='display the modification history')
        self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                        helpstr='display all valid options')
        self.valid_opts.add_opt('-ver', 0, [],            \
                        helpstr='display the current version number')

        # required parameters

        self.valid_opts.add_opt('-input_clust', 1, [], 
                        helpstr='name of input cluster dataset')

        self.valid_opts.add_opt('-input_atlas', 1, [], 
                        helpstr='name of input reference atlas dataset')

        self.valid_opts.add_opt('-prefix', 1, [], 
                        helpstr='name of output dataset')

        # optional parameters

        self.valid_opts.add_opt('-input_dat', 1, [], 
                        helpstr='name of input dset to report sign or mean')

        self.valid_opts.add_opt('-workdir', 1, [], 
                        helpstr='name of workdir (no path)')

        self.valid_opts.add_opt('-min_perc_clust', 1, [], 
                        helpstr='minimum fraction of cluster to be filled')

        self.valid_opts.add_opt('-min_perc_atlas', 1, [], 
                        helpstr='minimum fraction of atlas to be filled')

        self.valid_opts.add_opt('-olap_logic', 1, [], 
                        helpstr='logical operator for combining conditions')

        self.valid_opts.add_opt('-strict_fill_clust', 1, [], 
                        helpstr='apply min_perc_clust strictly')

        self.valid_opts.add_opt('-dat_col_as_sign', 1, [], 
                        helpstr='should extra data be reported as just sign?')

        # may derive from -prefix by default
        #self.valid_opts.add_opt('-outdir', 1, [], 
        #                helpstr='name of output dir')

        # general options

        self.valid_opts.add_opt('-do_clean', 1, [], 
                        helpstr="turn on/off removal of intermediate files")

        self.valid_opts.add_opt('-do_log', 0, [], 
                        helpstr="turn on/off logging shell cmd execution")

        self.valid_opts.add_opt('-overwrite', 0, [], 
                        helpstr='overwrite preexisting outputs')

        self.valid_opts.add_opt('-verb', 1, [], 
                        helpstr='set the verbose level (default is 0)')

        return 0

    def process_options(self):
        """return  1 on valid and exit        (e.g. -help)
           return  0 on valid and continue    (e.g. do main processing)
           return -1 on invalid               (bad things, panic, abort)
        """

        # process any optlist_ options
        self.valid_opts.check_special_opts(sys.argv)

        # process terminal options without the option_list interface
        # (so that errors are not reported)
        # return 1 (valid, but terminal)

        # if no arguments are given, apply -help
        if len(sys.argv) <= 1 or '-help' in sys.argv:
           print(g_help_string)
           return 1

        if '-hist' in sys.argv:
           print(g_history)
           return 1

        if '-show_valid_opts' in sys.argv:
           self.valid_opts.show('', 1)
           return 1

        if '-ver' in sys.argv:
           print(g_version)
           return 1

        # ============================================================
        # read options specified by the user
        self.user_opts = OL.read_options(sys.argv, self.valid_opts)
        uopts = self.user_opts            # convenience variable
        if not uopts: return -1           # error condition

        # ------------------------------------------------------------
        # process non-chronological options, verb comes first

        val, err = uopts.get_type_opt(int, '-verb')
        if val != None and not err: self.verb = val

        # ------------------------------------------------------------
        # process options sequentially, to make them like a script

        err_base = "Problem interpreting use of opt: "

        for opt in uopts.olist:

            # main options

            if opt.name == '-input_clust':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.input_clust = val

            elif opt.name == '-input_atlas':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.input_atlas = val

            elif opt.name == '-prefix':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.prefix = val

            # general options

            elif opt.name == '-input_dat':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.input_dat = val

            elif opt.name == '-workdir':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.workdir = val

            elif opt.name == '-min_perc_clust':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.min_perc_clust = val

            elif opt.name == '-min_perc_atlas':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.min_perc_atlas = val

            elif opt.name == '-olap_logic':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.olap_logic = val

            elif opt.name == '-strict_fill_clust':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.strict_fill_clust = val

            elif opt.name == '-dat_col_as_sign':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.dat_col_as_sign = val

            # general options

            elif opt.name == '-do_clean':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.do_clean = val

            elif opt.name == '-do_log':
                self.do_log = True

            elif opt.name == '-overwrite':
                self.overwrite = '-overwrite'

            # ... verb has already been checked above

        return 0

    def check_options(self):
        """perform any final tests before execution"""

        if self.verb > 1:
            BASE.IP("Begin processing options")

        # required opt
        if self.input_clust is None :
            BASE.EP1("missing -input_clust option")
            return -1

        if self.input_atlas is None :
            BASE.EP1("missing -input_atlas option")
            return -1

        if self.prefix is None:
            BASE.EP1("missing -prefix option")
            return -1

        return 0

    def test(self, verb=3):
        """one might want to be able to run internal tests,
           alternatively, test from the shell
        """
        print('------------------------ initial tests -----------------------')
        self.verb = verb
        
        print('------------------------ reset files -----------------------')

        print('------------------------ should fail -----------------------')

        print('------------------------ more tests ------------------------')

        return None

    # ----- decorators

    @property
    def ninput_clust(self):
        """number of input_clusts"""
        return len(self.input_clust)


# ----------------------------------------------------------------------------

def main():

    # init option-reading obj
    inobj = InOpts()
    if not(inobj) :  
        return 1, None

    # process (= read) options
    rv = inobj.process_options()
    if rv > 0: 
        # exit with success (e.g. -help)
        return 0, None
    if rv < 0:
        # exit with error status
        BASE.EP1('failed to process options')
        return 1, None

    # check the options
    rv2 = inobj.check_options()
    if rv2 :
        # exit with error status
        BASE.EP1('failed whilst checking options')
        return rv2, None

    # use options to create main object
    mainobj = LCT.MainObj( user_inobj=inobj )
    if not mainobj :  
        return 1, None

    # write out log/history of what has been done (not done by default, to
    # save some time, bc this takes a mini-while)
    if inobj.do_log :
        olog = 'log_cluster_table.txt'
        BASE.IP('creating log: {}'.format(olog))
        UTIL.write_afni_com_log(olog)

    return 0, mainobj

# ============================================================================

if __name__ == '__main__':

    stat, mainobj = main()
    sys.exit(stat)


