#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries (most)
from   afnipy import option_list     as OL
from   afnipy import afni_util       as UTIL
from   afnipy import afni_base       as BASE
from   afnipy import lib_simba_defs  as DEF

# ----------------------------------------------------------------------
# globals

# combine all entries for help here
g_help_dict   = {**DEF.DOPTS, 
                 'STR_all_simba_model_name': DEF.STR_all_simba_model_name, 
                 **DEF.RANGE_model_ls}

g_help_string = """Overview ~1~

This program is for running SIMBA (Scalable Image Modeling using a
Bayesian Approach), a program for both group level- and meta-analysis.
Please see here for more description:

  Zhong Y, Chen G, Taylor PA, Kang, J (2025). SIMBA: Scalable Image
  Modeling using a Bayesian Approach, A Consistent Framework for
  Including Spatial Dependencies in fMRI Studies. 
  https://arxiv.org/abs/2511.12825

This Python code implementation is by:
  Y Zhong (U. of Michigan, USA)
  Y Narayana Swamy (SSCC, NIMH, NIH, USA)
  PA Taylor (SSCC, NIMH, NIH, USA)

------------------------------------------------------------------------
Option usage ~1~

  simba.py -infiles group_dir/sub-*.nii.gz -mask group_mask.nii.gz   \\
           -prefix name_for_results [... other options]


  must use one of the following -in* opts
-infiles INSETA INSETB [INSETC ...]
               :(req) list of input dataset names (volumetric datasets)

-inset INSET   :(req) name of the input dataset (pickle format); mainly 
                for historical purposes

-prefix PREFIX :(req) name of output dset

-mask MASK     :(req) mask dataset, must match grid of all inset data

-ulay ULAY     :underlay dataset (for images within this program), must 
                match grid of all inset data

-model_name MN :name of model to run, selected from among this list: 
                 {STR_all_simba_model_name}

-model_ls  MLS :length scale parameter for the SIMBA model, which must be 
                in the range: {lsmin} < LS < {lsmax} 
                *** might make this required, estimated from 3dFWHMx ***
                (def: {model_ls})

-workdir WD    :working directory name, without path; the working dir
                will be subdirectory of the output location
                (def: name with random chars)

-do_clean DC   :state whether to clean up any intermediate files;
                allowed values are:  Yes, 1, No, 0
                (def: '{do_clean}')

-do_log        :add this opt to turn on making a text log of all the
                shell commands that are run when this program is
                executed. Mainly for debugging purposes.

-help, -h      :display program help file

-hist          :display program history

-ver           :display program version number

-verb  VVV     :control verbosity (def: {verb})

-show_valid_opts :show valid options for this program


Fine-control parameter options ~2~

These options are for controlling finer features of SIMBA models.
Some of these are implementation specific (like, for only VI or Gibbs,
etc.).  In general, these exist as options primarily for testing, and
we would not expect them to need to be used in typical applications.

General SIMBA model parameters:

-model_nu  MNU :parameter for the SIMBA model
                (def: {model_nu})

-model_L   ML  :parameter for the SIMBA model: number of basis used 
                for kernel approximation (tuning parameter)
                (def: {model_L})


SIMBA-VI model parameters:

-model_VI_ELBO_diff_tol MVI_EDT
               :a tolerance parameter for the SIMBA-VI model
                (def: {model_VI_ELBO_diff_tol})

-model_VI_para_diff_tol MVI_PDT
               :a tolerance parameter for the SIMBA-VI model
                (def: {model_VI_para_diff_tol})

-model_VI_elbo_stop MVI_ES
               :a boolean parameter for the SIMBA-VI model
                (def: {model_VI_elbo_stop})

-model_VI_max_iter MVI_MI
               :number of iterations for the SIMBA-VI model
                (def: {model_VI_max_iter})

-model_VI_verbose MVI_V
               :parameter for the SIMBA-VI model
                (def: {model_VI_verbose})


SIMBA-Gibbs model parameters:

-model_Gibbs_burnin MG_B
               :parameter for the SIMBA-Gibbs model: number of burn-in
                samples (def: {model_Gibbs_burnin})

-model_Gibbs_mcmc_sample MG_MS
               :parameter for the SIMBA-Gibbs model: number of posterior 
                samples for MCMC sampling (def: {model_Gibbs_mcmc_sample})

-model_Gibbs_thin MG_T
               :parameter for the SIMBA-Gibbs model: number of thinning 
                for MCMC sampling (def: {model_Gibbs_thin})


Posterior predictive checks (PPC) curve parameters:

-ppc_n_mcmc PPC_N 
               :number of Monte Carlo runs for PPC curve
                (def: {ppc_n_mcmc})

-ppc_alpha  PPC_A
               :alpha parameter for PPC curve
                (def: {ppc_alpha})

-ppc_color  PPC_C
               :color of simulation results in PPC curve
                (def: '{ppc_color}')

-ppc_label  PPC_L
               :label for PPC curve
                (def: '{ppc_label}')


------------------------------------------------------------------------

Notes ~1~

***

------------------------------------------------------------------------

Examples ~1~

 1) Basic SIMBA model execution, with preferred 'VI' model. Users should
    provide a lengthscale (ls) for the model, too---here, it is 8mm, based
    on smoothness estimation:

    simba.py                                                           \\
        -infiles     group_results/stats.sub-*nii.gz                   \\
        -prefix      out_simba                                         \\
        -mask        group_mask.inter.nii.gz                           \\
        -model_name  VI                                                \\
        -model_ls    8

 2) Same as Ex 1, but also providing an underlay 'ulay' dset for creating
    an image of results:

    simba.py                                                           \\
        -infiles     group_results/stats.sub-*nii.gz                   \\
        -prefix      out_simba                                         \\
        -mask        group_mask.inter.nii.gz                           \\
        -model_name  VI                                                \\
        -model_ls    8                                                 \\
        -ulay        MNI152_2009_template_SSW.nii.gz


""".format(**g_help_dict)

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
        self.argv            = None

        # general variables
        self.verb            = DEF.verb
        self.overwrite       = None
        self.do_clean        = None
        self.do_log          = None

        # main data variables
        self.inset           = None
        self.infiles         = []
        self.mask            = None
        self.ulay            = None
        self.prefix          = None
        self.workdir         = None
        self.outdir          = None

        # general model variables
        self.model_name      = None
        self.model_ls        = None
        self.model_nu        = None
        self.model_L         = None

        # model-specific variables
        self.model_VI_ELBO_diff_tol  = None
        self.model_VI_para_diff_tol  = None
        self.model_VI_elbo_stop      = None
        self.model_VI_max_iter       = None
        self.model_VI_verbose        = None
        self.model_Gibbs_burnin      = None
        self.model_Gibbs_mcmc_sample = None
        self.model_Gibbs_thin        = None

        # ppc variables
        self.ppc_n_mcmc    = None
        self.ppc_alpha     = None
        self.ppc_color     = None
        self.ppc_label     = None

        # figure variables
        self.fig_ext       = None
        self.fig_dpi       = None

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

        self.valid_opts.add_opt('-inset', 1, [], 
                        helpstr='name of input dataset (pickle)')

        self.valid_opts.add_opt('-infiles', -1, [], 
                        helpstr='name of input datasets (volumes)')

        self.valid_opts.add_opt('-prefix', 1, [], 
                        helpstr='name of output dataset')

        # optional parameters

        self.valid_opts.add_opt('-mask', 1, [], 
                        helpstr='name of mask dataset')

        self.valid_opts.add_opt('-ulay', 1, [], 
                        helpstr='name of underlay dataset (for QC images)')

        self.valid_opts.add_opt('-workdir', 1, [], 
                        helpstr='name of workdir (no path)')

        # general model parameters

        self.valid_opts.add_opt('-model_name', 1, [], 
                        helpstr='name of the SIMBA model to run')

        self.valid_opts.add_opt('-model_ls', 1, [], 
                        helpstr='SIMBA model length scale, in range [0, 1]**')

        self.valid_opts.add_opt('-model_nu', 1, [], 
                        helpstr='SIMBA model parameter nu')

        # model-specific parameters

        self.valid_opts.add_opt('-model_VI_ELBO_diff_tol', 1, [], 
                        helpstr='tolerance parameter in SIMBA-VI model')

        self.valid_opts.add_opt('-model_VI_para_diff_tol', 1, [], 
                        helpstr='tolerance parameter in SIMBA-VI model')

        self.valid_opts.add_opt('-model_VI_elbo_stop', 1, [], 
                        helpstr='true/false parameter in SIMBA-VI model')

        self.valid_opts.add_opt('-model_VI_max_iter', 1, [], 
                        helpstr='number of iterations in SIMBA-VI model')

        self.valid_opts.add_opt('-model_VI_verbose', 1, [], 
                        helpstr='parameter in SIMBA-VI model')

        self.valid_opts.add_opt('-model_Gibbs_burnin', 1, [], 
                        helpstr='parameter for the SIMBA-Gibbs model')

        self.valid_opts.add_opt('-model_Gibbs_mcmc_sample', 1, [], 
                        helpstr='parameter for the SIMBA-Gibbs model')

        self.valid_opts.add_opt('-model_Gibbs_thin', 1, [], 
                        helpstr='parameter for the SIMBA-Gibbs model')

        # ppc parameters

        self.valid_opts.add_opt('-ppc_n_mcmc', 1, [], 
                        helpstr='number of Monte Carlo runs for PPC curve')

        self.valid_opts.add_opt('-ppc_alpha', 1, [], 
                        helpstr='alpha parameter for PPC curve')

        self.valid_opts.add_opt('-ppc_color', 1, [], 
                        helpstr='color of simulation results in PPC curve')

        self.valid_opts.add_opt('-ppc_label', 1, [], 
                        helpstr='label for PPC curve')

        # figure parameters

        self.valid_opts.add_opt('-fig_ext', 1, [], 
                        helpstr='extension for figure(s)')

        self.valid_opts.add_opt('-fig_dpi', 1, [], 
                        helpstr='DPI (dots per inch( for figure(s)')

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

        self.argv = sys.argv              # record cmd, for hist

        # ------------------------------------------------------------
        # process non-chronological options, verb comes first

        val, err = uopts.get_type_opt(int, '-verb')
        if val != None and not err: self.verb = val

        # ------------------------------------------------------------
        # process options sequentially, to make them like a script

        err_base = "Problem interpreting use of opt: "

        for opt in uopts.olist:

            # main options

            if opt.name == '-inset':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.inset = val

            elif opt.name == '-infiles':
                val, err = uopts.get_string_list('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.infiles = val

            elif opt.name == '-prefix':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.prefix = val

            # general options

            elif opt.name == '-mask':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.mask = val

            elif opt.name == '-ulay':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.ulay = val

            elif opt.name == '-workdir':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.workdir = val

            # SIMBA model opts

            elif opt.name == '-model_name':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.model_name = val

            elif opt.name == '-model_ls':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_ls = val

            elif opt.name == '-model_nu':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_nu = val

            # model-specific opts

            elif opt.name == '-model_VI_ELBO_diff_tol':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_VI_ELBO_diff_tol = val

            elif opt.name == '-model_VI_para_diff_tol':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_VI_para_diff_tol = val

            elif opt.name == '-model_VI_max_iter':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_VI_max_iter = val

            elif opt.name == '-model_VI_elbo_stop':
                val, err = uopts.get_type_opt(bool, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_VI_elbo_stop = val

            elif opt.name == '-model_VI_verbose':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_VI_verbose = val

            elif opt.name == '-model_Gibbs_burnin':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_Gibbs_burnin = val

            elif opt.name == '-model_Gibbs_mcmc_sample':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_Gibbs_mcmc_sample = val

            elif opt.name == '-model_Gibbs_thin':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.model_Gibbs_thin = val

            # ppc opts

            elif opt.name == '-ppc_n_mcmc':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.ppc_n_mcmc = val

            elif opt.name == '-ppc_alpha':
                val, err = uopts.get_type_opt(float, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.ppc_alpha = val

            elif opt.name == '-ppc_color':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.ppc_color = val

            elif opt.name == '-ppc_label':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.ppc_label = val

            # figure options

            elif opt.name == '-fig_ext':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.fig_ext = val

            elif opt.name == '-fig_dpi':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: 
                    BASE.EP(err_base + opt.name)
                self.fig_dpi = val

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
        if self.inset is None and self.infiles is None :
            BASE.EP1("missing one of these input opts: -inset, -infiles")
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
    def ninset(self):
        """number of insets (return 0 if None)"""
        if self.inset is not None :
            return len(self.inset)
        else:
            return 0

    @property
    def ninfiles(self):
        """number of infiles (return 0 if None)"""
        if self.infiles is not None :
            return len(self.infiles)
        else:
            return 0


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

    # import this module here, having established processing will occur
    from   afnipy import lib_simba_run   as LSR

    # check the options
    rv2 = inobj.check_options()
    if rv2 :
        # exit with error status
        BASE.EP1('failed whilst checking options')
        return rv2, None

    # use options to create main object
    mainobj = LSR.MainObj( user_inobj=inobj )
    if not mainobj :  
        return 1

    # write out log/history of what has been done (not done by default, to
    # save some time, bc this takes a mini-while)
    if inobj.do_log :
        olog = 'log_template_tool.txt'
        UTIL.write_afni_com_log(olog)

    return 0, mainobj

# ============================================================================

if __name__ == '__main__':

    stat, mainobj = main()
    sys.exit(stat)


