#!/usr/bin/env python

import sys
import option_list, afni_util as UTIL, afni_base as BASE

g_help_string = """
===========================================================================
neuro_deconvolve.py:

Generate a script that would apply 3dTfitter to deconvolve an MRI signal
(BOLD response curve) into a neuro response curve.

Required parameters include an input dataset, a script name and an output
prefix.

----------------------------------------------------------------------
examples:

    1. 3d+time example

        neuro_deconvolve.py                     \\
                -input run1+orig                \\
                -script script.neuro            \\
                -mask_dset automask+orig        \\
                -prefix neuro_resp

    2. 1D example

        neuro_deconvolve.py             \\
                -input epi_data.1D      \\
                -tr 2.0                 \\
                -script script.1d       \\
                -prefix neuro.1D


----------------------------------------------------------------------
informational arguments:

    -help                       : display this help
    -hist                       : display the modification history
    -show_valid_opts            : display all valid options (short format)
    -ver                        : display the version number

----------------------------------------
required arguments:

    -input INPUT_DATASET        : set the data to deconvolve

        e.g. -input epi_data.1D

    -prefix PREFIX              : set the prefix for output filenames

        e.g. -prefix neuro_resp

                --> might create: neuro_resp+orig.HEAD/.BRIK

    -script SCRIPT              : specify the name of the output script

        e.g. -script neuro.script

----------------------------------------
optional arguments:


    -kernel KERNEL              : set the response kernel

        default: -kernel GAM

    -kernel_file FILENAME       : set the filename to store the kernel in

        default: -kernel_file resp_kernel.1D

    -mask_dset DSET             : set a mask dataset for 3dTfitter to use

        e.g. -mask_dset automask+orig

    -tr TR                      : set the scanner TR

        e.g. -tr 2.5

        The TR is needed for 1D formatted input files.  It is not needed
        for AFNI 3d+time datasets, since the TR is in the file.

    -verb LEVEL                 : set the verbose level

        e.g. -verb 2


- R Reynolds  June 12, 2008
===========================================================================
"""

g_history = """
    neuro_deconvolve.py history:

    0.1  June 12, 2008: initial version
"""

g_version = "version 0.1, June 12, 2008"

gDEF_VERB       = 1      # default verbose level

class Decon:
    def __init__(self, label):
        # general parameters
        self.label      = label
        self.verb       = gDEF_VERB
        self.valid_opts = None                  # OptionList
        self.user_opts  = None                  # OptionList
        self.aname      = None                  # afni_name from input

        # required user parameters
        self.input      = None                  # input dataset
        self.prefix     = None                  # for script output
        self.script     = None                  # script name

        # optional parameters
        self.kernel     = 'GAM'                 # response kernel
        self.kfile      = 'resp_kernel.1D'      # file for response kernel
        self.maskset    = None                  # mask dataset
        self.tr         = None                  # TR for curve and dataset

    def init_opts(self):
        global g_help_string
        self.valid_opts = option_list.OptionList('for input')

        # short, terminal arguments
        self.valid_opts.add_opt('-help', 0, [],      \
                        helpstr='display program help')
        self.valid_opts.add_opt('-hist', 0, [],      \
                        helpstr='display the modification history')
        self.valid_opts.add_opt('-show_valid_opts', 0, [], \
                        helpstr='display all valid options')
        self.valid_opts.add_opt('-ver', 0, [],       \
                        helpstr='display the current version number')

        # required arguments
        self.valid_opts.add_opt('-input', 1, [], req=1,
                        helpstr='input dataset (containing BOLD curve)')
        self.valid_opts.add_opt('-prefix', 1, [], req=1,
                        helpstr='prefix for deconvolved dataset');
        self.valid_opts.add_opt('-script', 1, [], req=1,
                        helpstr='output script filename')

        # optional arguments
        self.valid_opts.add_opt('-kernel', 1, [],
                        helpstr='response kernel [GAM])');

        self.valid_opts.add_opt('-kernel_file', 1, [],
                        helpstr='file for reponse kernel [resp_kernel.1D]');

        self.valid_opts.add_opt('-mask_dset', 1, [],
                        helpstr='computational mask dataset');

        self.valid_opts.add_opt('-tr', 1, [],
                        helpstr='specify TR for 3dDeconvolve command');

        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr='verbose level (0=quiet, 1=default, ...)')


    def read_opts(self):
        """check for terminal arguments, then read the user options"""

        # process any optlist_ options
        self.valid_opts.check_special_opts(sys.argv)

        # ------------------------------------------------------------
        # check for terminal args in argv (to ignore required args)

        # if argv has only the program name, or user requests help, show it
        if len(sys.argv) <= 1 or '-help' in sys.argv:
            print g_help_string
            return 0

        if '-hist' in sys.argv:
            print g_history
            return 0

        if '-show_valid_opts' in sys.argv:      # show all valid options
            self.valid_opts.show('', 1)
            return 0

        if '-ver' in sys.argv:
            print g_version
            return 0

        # ------------------------------------------------------------
        # now read user options

        self.user_opts = option_list.read_options(sys.argv, self.valid_opts)
        if not self.user_opts: return 1         # error condition

        return None     # normal completion

    def process_opts(self):
        """apply each option"""

        # ----------------------------------------
        # set verb first
        self.verb,err = self.user_opts.get_type_opt(int, '-verb')
        if err: return 1
        if self.verb == None: self.verb = gDEF_VERB

        # ----------------------------------------
        # required args

        self.input, err = self.user_opts.get_string_opt('-input')
        if self.input == None or err: return 1

        self.prefix, err = self.user_opts.get_string_opt('-prefix')
        if self.prefix == None or err: return 1

        self.script, err = self.user_opts.get_string_opt('-script')
        if self.script == None or err: return 1

        if self.verb > 1:
            print "-- have input '%s', script '%s', prefix '%s'" %      \
                  (self.input, self.script, self.prefix)

        # ----------------------------------------
        # optional arguments

        val, err = self.user_opts.get_string_opt('-kernel')
        if err: return 1
        if val != None: self.kernel = val

        val, err = self.user_opts.get_string_opt('-kernel_file')
        if err: return 1
        if val != None: self.kfile = val

        val, err = self.user_opts.get_string_opt('-mask_dset')
        if err: return 1
        if val != None: self.maskset = val

        val,err = self.user_opts.get_type_opt(float, '-tr')
        if err: return 1
        if val != None: self.tr = val

        # ----------------------------------------
        # check over the inputs

        # check over -input as an AFNI dataset
        self.aname = BASE.afni_name(self.input)
        if self.aname == None: return 1
        if self.verb > 2: self.aname.show()

        if self.aname.type == '1D':
            if self.tr == None:
                print '** -tr is required if the input is in 1D format'
            self.reps = UTIL.max_dim_1D(self.input)
        else:
            if self.aname.type != 'BRIK':
                print "** unknown 'type' for -input '%s'" % self.input
            err,self.reps,self.tr =     \
                UTIL.get_dset_reps_tr(self.aname.pv(),self.verb)
            if err: return 1

        if self.verb > 1:
            print '-- using kernel %s, kfile %s, tr = %s, reps = %s' %  \
                  (self.kernel, self.kfile, self.tr, self.reps)

        return None

    def make_decon_script(self):
        """create the deconvolution (3dTfitter) script"""

        cmd  = '# ------------------------------------------------------\n'  \
               '# perform neuro deconvolution via 3dTfitter\n\n'

        cmd += "# create response kernel\n"                             \
               "waver -%s -peak 1 -dt %s -inline 1@1 > %s\n\n" %        \
                (self.kernel, self.tr, self.kfile)

        if self.aname.type == '1D':
            prefix = 'detrend.1D'
        else:
            prefix = 'detrend%s' % self.aname.view

        polort = UTIL.get_default_polort(self.tr, self.reps)
        cmd += '# detrend the input\n'                          \
               '3dDetrend -polort %d -prefix %s %s\n\n' %       \
                (polort,prefix,self.input)

        if self.maskset: mask = '          -mask %s \\\n' % self.maskset
        else:            mask = ''

        if self.aname.type == '1D':
            cmd += '# transpose the detrended dataset\n'        \
                   '1dtranspose detrend.1D > detrend_tr.1D\n\n' 
            dname = 'detrend_tr.1D'
        else:
            dname = 'detrend%s' % self.aname.view

        c2  = '3dTfitter -lsqfit -RHS %s \\\n'           \
              '%s'                                       \
              '          -FALTUNG %s %s \\\n'            \
              '          01 0.0\n\n' % (dname,mask,self.kfile,self.prefix)

        cmd += UTIL.add_line_wrappers(c2)

        fp = open(self.script, "w")
        if not fp:
            print '** failed to open %s for writing decon script' % self.script
            return 1

        fp.write(cmd)
        fp.close()

        return

def process():
    decon = Decon('deconvolve response curves')
    decon.init_opts()
    rv = decon.read_opts()
    if rv != None:      # 0 is okay, else error code
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command (RO):")
        return rv

    rv = decon.process_opts()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command (PO):")
        return rv

    rv = decon.make_decon_script()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command (MS):")
        return rv

    return 0

if __name__ == "__main__":
    rv = process()
    sys.exit(rv)

