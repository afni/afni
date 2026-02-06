#!/usr/bin/env python

# A library of functions for SIMBA processing
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
#        Y Narayana Swamy (SSCC, NIMH, NIH, USA)
#        Yuan Zhong (University of Michigan, USA)
# ----------------------------------------------------------------------------
# ver 1.0 : start of main interface 
# ============================================================================

import sys, os, copy, glob

import time
import torch
import numpy   as np
import nibabel as nib
from   sklearn.gaussian_process.kernels import Matern

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au
from   afnipy import lib_simba_defs     as DEF
from   afnipy import lib_simba_dobj     as DOBJ

# must have simba package in afnipy
import afnipy.simba_model.SIMBA_VI      as SIMBA_VI
import afnipy.simba_model.SIMBA_Gibbs   as SIMBA_Gibbs

# ============================================================================
# various parameters

DEF_unpack_dtype  = torch.float32

# ============================================================================

class MainObj:
    """Object for controlling and running SIMBA.

Parameters
----------
inobj : InOpts object 
    object constructed from running simba.py on the command
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

        # control behavior
        self.do_plot         = True
        self.do_save_nifti   = True
        self.do_afniize      = True
        self.do_ppc_curves   = True
        
        # main data names
        self.inset           = DEF.DOPTS['inset']
        self.infiles         = DEF.DOPTS['infiles']
        self.prefix          = DEF.DOPTS['prefix']
        self.prefix_noext    = ''
        self.prefix_outset   = ''                      # includes .nii.gz
        self.mask            = DEF.DOPTS['mask']
        self.ulay            = DEF.DOPTS['ulay']

        self.outdir          = DEF.DOPTS['outdir']     # None or str
        self.workdir         = DEF.DOPTS['workdir']

        # general model quantities
        self.model_pim       = DEF.DOPTS['model_pim']  # model post inf meth
        self.model_ls        = DEF.DOPTS['model_ls']   # model param
        self.model_nu        = DEF.DOPTS['model_nu']   # model param
        self.model_L         = DEF.DOPTS['model_L']
        self.model_L_eta     = int(0.1*self.model_L)

        self.unpack_dtype    = DEF_unpack_dtype
        self.kernel          = None                    # set by a method, below

        # model specific quantities
        self.model_VI_ELBO_diff_tol  = DEF.DOPTS['model_VI_ELBO_diff_tol']
        self.model_VI_para_diff_tol  = DEF.DOPTS['model_VI_para_diff_tol']
        self.model_VI_elbo_stop      = DEF.DOPTS['model_VI_elbo_stop']
        self.model_VI_max_iter       = DEF.DOPTS['model_VI_max_iter']
        self.model_VI_verbose        = DEF.DOPTS['model_VI_verbose']
        self.model_Gibbs_burnin      = DEF.DOPTS['model_Gibbs_burnin']
        self.model_Gibbs_mcmc_sample = DEF.DOPTS['model_Gibbs_mcmc_sample']
        self.model_Gibbs_thin        = DEF.DOPTS['model_Gibbs_thin']

        # ppc curve quantities
        self.ppc_n_mcmc      = DEF.DOPTS['ppc_n_mcmc']
        self.ppc_alpha       = DEF.DOPTS['ppc_alpha']
        self.ppc_color       = DEF.DOPTS['ppc_color']
        self.ppc_label       = DEF.DOPTS['ppc_label']
        self.ppc_xlim        = DEF.DOPTS['ppc_xlim']

        # figure quantities
        self.fig_ext         = DEF.DOPTS['fig_ext']
        self.fig_dpi         = DEF.DOPTS['fig_dpi']

        # created by model
        self.model           = None
        self.paras           = None
        self.profile         = None
        self.eff_sig         = None
        self.eff_est         = None
        self.E_s             = None

        # main data obj 
        self.simdata         = None                    # obj with data, etc.


        # ----- take action(s)

        # prelim stuff
        if user_inobj :
            tmp1 = self.load_from_inopts()
            tmp2 = self.basic_setup()

            # load in main data
            if self.inset :
                tmp3 = self.load_simdata_from_inset_pickle()
            elif self.infiles :
                tmp4 = self.load_simdata_from_infiles()

            # do the work
            if self.simdata is not None :
                tmp5 = self.specify_kernel_function()
                tmp6 = self.run_model_general()
                tmp7 = self.make_outputs()


    # ----- methods

    def run_model_general(self):
        """Use model_pim to call appropriate model method"""

        if self.verb > 1 :
            ab.IP("Start the model: " + self.model_pim)

        if self.model_pim == "VI" :
            tmp = self.run_model_VI()
        elif self.model_pim == "Gibbs" :
            tmp = self.run_model_Gibbs()
        else:
            msg = "Unrecognized model {}, ".format(self.model_pim)
            msg+= "is not in known list:\n{}".format(STR_all_simba_model_pim)
            ab.EP(msg)

        return 0

    def run_model_VI(self):
        """Run the VI model, using appropriate parameters"""
        self.model = SIMBA_VI.SIMBA_VI( Y       = self.simdata.data,
                                        X       = None,
                                        grids   = self.simdata.grid, 
                                        kernel  = self.kernel, 
                                        L       = self.model_L, 
                                        L_eta   = self.model_L_eta,
                                        verbose = self.model_VI_verbose,
                                        dtype   = self.unpack_dtype,
                                  ELBO_diff_tol = self.model_VI_ELBO_diff_tol,
                                  para_diff_tol = self.model_VI_para_diff_tol,
                                  elbo_stop     = self.model_VI_elbo_stop,
                                  max_iter      = self.model_VI_max_iter )
        
        self.paras, self.profile = self.model.run()

        if self.verb > 1 :
            ab.IP("Model done, getting effect estimates + E_s")

        # effect est (mean of MCMC) and the statistical evidence (P+ -> E_s)

        eff_est  = self.paras['E_alpha'][:,None] + \
                   self.paras['E_theta_beta'] @ self.model.basis.t()
        eff_mcmc = self.model.get_eff_mcmc()
        P_pos    = ((eff_mcmc > 0) * 1.0).mean(0)
        E_s      = (P_pos - 0.5) * 2

        select  = (E_s.abs() > 0.95) * 1
        eff_sig = eff_est * select

        # ... and reshape to 3D

        ii, jj, kk = self.simdata.ii, self.simdata.jj, self.simdata.kk

        # this might be effect estimate times sig?
        self.eff_sig = np.zeros(self.simdata.mask_img.shape)
        self.eff_sig[ii, jj, kk] = eff_sig

        self.eff_est = np.zeros(self.simdata.mask_img.shape)
        self.eff_est[ii, jj, kk] = eff_est
        self.E_s = np.zeros(self.simdata.mask_img.shape)
        self.E_s[ii, jj, kk] = E_s

        return 0

    def run_model_Gibbs(self):
        """Run the Gibbs model, using appropriate parameters"""
        self.model = SIMBA_Gibbs.SIMBA_Gibbs( Y       = self.simdata.data,
                                              grids   = self.simdata.grid, 
                                              kernel  = self.kernel, 
                                              L       = self.model_L, 
                                              L_eta   = self.model_L_eta,
                                   burnin  = self.model_Gibbs_burnin,
                               mcmc_sample = self.model_Gibbs_mcmc_sample,
                                   thin    = self.model_Gibbs_thin,
                                   dtype   = self.unpack_dtype)

        samples = self.model.run( n_chains = 3, 
                                  parallel = False )

        if self.verb > 1 :
            ab.IP("Model done, getting effect estimates + E_s")

        # effect est and the statistical evidence

        voxel_mcmc = samples['alpha'][:,:,None] + \
                     samples['theta_beta'] @ self.model.basis.t()
        eff_est = voxel_mcmc.mean(dim=(0,1))
        # E_s = ??? ***

        # ... and reshape to 3D

        ii, jj, kk = self.simdata.ii, self.simdata.jj, self.simdata.kk

        self.eff_est = np.zeros(self.simdata.mask_img.shape)
        self.eff_est[ii, jj, kk] = eff_est
        #self.E_s = np.zeros(self.simdata.mask_img.shape)
        #self.E_s[ii, jj, kk] = E_s

        return 0


    def make_outputs(self):
        """Go through possible outputs and create them, as desired"""

        if self.verb > 1 :
            ab.IP("Make outputs")

        if self.do_plot :
            self.fig = self.make_model_plot()

        if self.do_save_nifti :
            tmp1 = self.write_model_to_nifti()

        if self.do_ppc_curves :
            tmp2 = self.make_ppc_curves()

        return 0

    def make_model_plot(self):
        """use nilearn to plot map"""
        from nilearn.plotting import plot_stat_map

        if self.verb > 1 :
            ab.IP("Save plot")

        if self.simdata.ulay_data is None:
            ab.WP("No ulay dset provided --- skipping dset plot")
            return None

        # shorter names
        ii, jj, kk = self.simdata.ii, self.simdata.jj, self.simdata.kk

        post_map_img = nib.Nifti1Image(self.eff_est, 
                                       self.simdata.mask_img.affine)
        ulay_img = nib.Nifti1Image(self.simdata.ulay_data, 
                                   self.simdata.ulay_img.affine)

        fig = plot_stat_map(post_map_img,
                            bg_img=ulay_img,
                            colorbar=True,
                            threshold=0,
                            cbar_tick_format="%.3f",
                            #vmax=0.003, vmin=-0.003,
                            cmap='coolwarm',
                            #cmap = base_cmap,
                            cut_coords=[0, -22, 13])

        # save the fig as an image
        fig_name = self.prefix_noext + '.' + self.fig_ext
        fig.savefig( fig_name, dpi=self.fig_dpi )

        return fig

    def write_model_to_nifti(self):
        """*** work on this

        + have to pass along better qform_code/sform_code
        + have to make sure affine matches; right now, mask grid should match
        + ensure array is valid output type (like np.float32 for float types)
        """

        if self.verb > 1 :
            ab.IP("Write NIFTI")

        qform_code = self.simdata.nifti_qform_code
        sform_code = self.simdata.nifti_sform_code

        opre0 = self.prefix_outset
        arr0  = ensure_array_valid_type(self.eff_est)

        # text for adding subvolume labels
        opt_sublab = "-sublabel 0 eff_est "

        # concatenate the E_s volume
        if self.E_s is not None :
            arr1 = ensure_array_valid_type(self.E_s)
            arr0 = np.stack((arr0, arr1), axis=3)
            opt_sublab+= "-sublabel 1 stat_pplus "

        ovol0 = nib.Nifti1Image(arr0, affine=self.simdata.mask_img.affine)
        ovol0.header['qform_code'] = qform_code
        ovol0.header['sform_code'] = sform_code
        nib.save(ovol0, opre0)
    
        # do this to create AFNI extension, for subbrick selectors, etc.
        if self.do_afniize :
            cmd  = '3dcopy -overwrite {} {}'.format(opre0, opre0)
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()

            if stat :
                ab.EP("Cannot copy output dataset: {}".format(opre0))

            # add subbrick labels
            cmd  = '3drefit {} {}'.format(opt_sublab, opre0)
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()

            # add history
            all_opt = copy.deepcopy(self.user_inobj.argv)
            all_opt[0] = all_opt[0].split('/')[-1] # only name of prog
            hist = ' '.join(all_opt)
            cmd  = '3dNotes -h "{}" {}'.format(hist, opre0)
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()

        return 0

    def make_ppc_curves(self):
        """**** """
        from fastkde import fastKDE
        import matplotlib.pyplot as plt

        if self.verb :
            ab.IP("Start PPC curves, N = {}".format(self.ppc_n_mcmc))

        # setup PPC and flatten chains
        if self.model_pim == 'VI' :
            ppc = self.model.PPC(n_mcmc = self.ppc_n_mcmc)
        elif self.model_pim == 'Gibbs' :
            ppc = self.model.PPC(n_samples = self.ppc_n_mcmc)

        ppc = ppc.reshape(-1, self.simdata.ndset, self.simdata.nvox)
        
        # set up draw iterations and progress reporting rate
        ndraw      = ppc.shape[0]
        nprog      = ndraw // 10
        prog_old   = 0
        time_new   = time.time()
        start_time = time_new

        # start new figure
        plt.figure("PPC plot, posterior inference meth:" + self.model_pim)

        plt.axhline(0, color='0.75', lw=0.5)  

        ### Notes on fastKDE.pdf() here:
        # - unlike most online help descriptions, it returns 1 obj
        #   here, a DataArray in xarray.core.dataarray
        # - the domain of x-values to plot (or 'coords') just come
        #   from the input data, so one would get the min and max of
        #   that tensor to know; can also be extracted via: 
        #   NAME.var0.to_numpy()
        # - within the output obj, the 'data' attribute is what
        #   contains our data of interest, and one can get the np
        #   array via: NAME.data

        # takes about 3 mins for 100 draws
        for i in range(ndraw):
            # do density calcs (with flattening to vector)
            density = fastKDE.pdf(ppc[i].reshape(-1), ecf_precision=1)
            density.plot(alpha=self.ppc_alpha, color=self.ppc_color, 
                         label=self.ppc_label if i == 0 else None)
            prog_new = i // nprog
            if self.verb and i == 0 :
                time_old  = time_new
                time_new  = time.time()
                time_diff = time_new - time_old
                msg = "init kde, time = {:.2f} s".format(time_diff)
                print("   " + msg)
                prog_old = prog_new
            if self.verb and prog_new != prog_old :
                time_old  = time_new
                time_new  = time.time()
                time_diff = time_new - time_old
                msg = "{:d}0% done, time+= {:.2f} s".format(prog_new, time_diff)
                print("   " + msg)
                prog_old = prog_new
        runtime = time.time() - start_time
        ab.IP("Done making PPC curves, total time = {:.2f} s".format(runtime))

        # the full model
        data_den = fastKDE.pdf(self.simdata.data.reshape(-1), ecf_precision=1)
        data_den.plot(alpha=1.0, color="black", label='Observed data')

        # have to make this adaptable, based on calcs of the curves
        if self.ppc_xlim is None :
            self.ppc_xlim = (-0.04, 0.04)
            xvals = data_den.var0.to_numpy()
            yvals = data_den.data[:]
            _tmp, self.ppc_xlim = calc_xlim_range(xvals, yvals)

        plt.xlim(self.ppc_xlim[0], self.ppc_xlim[1])  
        plt.ylabel('')
        plt.xlabel('')
        plt.xticks(np.linspace(self.ppc_xlim[0], self.ppc_xlim[1], 5), 
                   fontsize=18)
        #plt.yticks(np.linspace(0, 70, 6), fontsize=18)
        plt.legend()

        fig_name = self.prefix_noext + '_ppc' + '.' + self.fig_ext
        plt.savefig( fig_name, dpi=self.fig_dpi, bbox_inches='tight' )
        plt.close()

        return 0

    def specify_kernel_function(self):
        """Use lengthscale and nu parameters to define kernel function"""

        if self.verb > 1 :
            ab.IP("Start kernel")

        self.kernel = Matern(length_scale=self.model_ls, nu=self.model_nu)
        return 0

    def load_simdata_from_inset_pickle(self):
        """For initial data loading, via a pickle dset"""

        if self.verb > 1 :
            ab.IP("Load data from inset")

        self.simdata = DOBJ.SimbaDataObj( fname_pickle = self.inset,
                                          fname_mask   = self.mask, 
                                          fname_ulay   = self.ulay, 
                                          verb         = self.verb)
        return 0

    def load_simdata_from_infiles(self):
        """For initial data loading, via list of infiles"""

        if self.verb > 1 :
            ab.IP("Load data from {} infiles".format(self.infiles))

        self.simdata = DOBJ.SimbaDataObj( infiles       = self.infiles,
                                          fname_mask    = self.mask, 
                                          fname_ulay    = self.ulay, 
                                          verb          = self.verb)
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
        if io.inset is not None :
            self.inset = io.inset
        if io.infiles is not None :
            self.infiles = io.infiles
        if io.prefix is not None :
            self.prefix = io.prefix
        if io.mask is not None :
            self.mask = io.mask
        if io.ulay is not None :
            self.ulay = io.ulay

        if io.workdir is not None :
            self.workdir = io.workdir
        if io.outdir is not None :          # or this could be derived
            self.outdir = io.outdir

        # general model variables
        if io.model_pim is not None :
            self.model_pim = io.model_pim
        if io.model_ls is not None :
            self.model_ls = io.model_ls
        if io.model_nu is not None :
            self.model_nu = io.model_nu
        if io.model_L is not None :
            self.model_L = io.model_L

        # model-specific parameters
        if io.model_VI_ELBO_diff_tol is not None :
            self.model_VI_ELBO_diff_tol = io.model_VI_ELBO_diff_tol
        if io.model_VI_para_diff_tol is not None :
            self.model_VI_para_diff_tol = io.model_VI_para_diff_tol
        if io.model_VI_elbo_stop is not None :
            self.model_VI_elbo_stop = io.model_VI_elbo_stop
        if io.model_VI_max_iter is not None :
            self.model_VI_max_iter = io.model_VI_max_iter
        if io.model_VI_verbose is not None :
            self.model_VI_verbose = io.model_VI_verbose
        if io.model_Gibbs_burnin is not None :
            self.model_Gibbs_burnin = io.model_Gibbs_burnin
        if io.model_Gibbs_mcmc_sample is not None :
            self.model_Gibbs_mcmc_sample = io.model_Gibbs_mcmc_sample
        if io.model_Gibbs_thin is not None :
            self.model_Gibbs_thin = io.model_Gibbs_thin

        # ppc variables
        if io.ppc_n_mcmc is not None :
            self.ppc_n_mcmc = io.ppc_n_mcmc
        if io.ppc_alpha is not None :
            self.ppc_alpha = io.ppc_alpha
        if io.ppc_color is not None :
            self.ppc_color = io.ppc_color
        if io.ppc_label is not None :
            self.ppc_label = io.ppc_label
        if io.ppc_xlim is not None :
            self.ppc_xlim = io.ppc_xlim

        # figure variables
        if io.fig_ext is not None :
            self.fig_ext = io.fig_ext
        if io.fig_dpi is not None :
            self.fig_dpi = io.fig_dpi

        return 0


    def basic_setup(self):
        """Run through basic checks of what has been input, and fill in any
        further information that is needed (like wdir, etc.)"""

        # check basic requirements

        ab.IP("Start setup for SIMBA model: {}".format(self.model_pim))

        # must have at least some input 
        if not(self.infiles) and self.inset is None :
            ab.EP("Need to provide input with either: -inset, -infiles")
        elif len(self.infiles) :
            # all infiles exist and have same grid
            nfail = au.check_all_dsets_exist(self.infiles, label='infiles',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load one or more infiles")

            ndiff = au.check_all_dsets_same_grid(self.infiles, label='infiles',
                                             verb=self.verb)
            if ndiff :
                ab.EP("Grid mismatch of one or more infiles")

            # make sure number of infiles is >1
            if not(self.ninfiles > 1) :
                ab.EP("Must have >1 infile")

        # must have output name prefix
        if not(self.prefix) : 
            ab.EP("Need to provide a prefix")
        else:
            # version of prefix with no extension info
            anobj = ab.afni_name(self.prefix)
            self.prefix_noext = anobj.noext_input()
            self.prefix_outset = self.prefix
            if not((self.prefix_outset).endswith('.nii')) or \
               not((self.prefix_outset).endswith('.nii.gz')) :
                self.prefix_outset+= '.nii.gz'

        # must have a mask
        if not(self.mask) : 
            ab.EP("Need to provide a mask")
        else:
            nfail = au.check_all_dsets_exist([self.mask], label='mask',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load mask")

        # if using infiles and mask, make sure grids match
        if len(self.infiles) and self.mask :
            ndiff = au.check_all_dsets_exist([self.infiles[0], self.mask], 
                                             label='infiles+mask',
                                             verb=self.verb)
            if ndiff :
                ab.EP("Grid mismatch between infiles and mask")


        # if entered, make sure ulay exists
        if self.ulay :
            nfail = au.check_all_dsets_exist([self.ulay], label='ulay',
                                             verb=self.verb)
            if nfail :
                ab.EP("Failed to load ulay")

        # model_pim must be an allowed one
        if self.model_pim not in DEF.LIST_all_simba_model_pim :
            msg = "Model PIM '{}' not in allowed list:".format(self.model_pim)
            msg+= "\n{}".format(all_simba_model_pim)
            ab.EP(msg)

        # allowed range of model length scales
        if self.model_ls <= DEF.RANGE_model_ls['lsmin'] or \
           self.model_ls >= DEF.RANGE_model_ls['lsmax'] :
            msg = "model_ls '{}' not in allowed range:".format(self.model_ls)
            msg+= "\n({}, {})".format(DEF.RANGE_model_ls['lsmin'], 
                                      DEF.RANGE_model_ls['lsmax'])
            ab.EP(msg)

        # generate basic items

        # generate wdir with random component, if none provided
        if not(self.workdir) :
            cmd  = '3dnewid -fun11'
            com  = ab.shell_com(cmd, capture=1)
            stat = com.run()
            rstr = com.so[0].strip()
            self.workdir = DEF.STR_workdir_base + rstr

        # convert bool-ish opts to bools
        self.do_clean           = au.convert_to_bool_yn10(self.do_clean)
        self.do_log             = au.convert_to_bool_yn10(self.do_log)
        self.model_VI_elbo_stop = au.convert_to_bool_yn10(self.model_VI_elbo_stop)
        return 0


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

def ensure_array_valid_type(A, verb=1):
    """Make sure array A is valid type for writing with nibabel.  For now,
this means that all arrays are numpy (not torch) ones, and that floats
are np.float32.

Parameters
----------
A : array
    N-dimensional array

Returns
-------
B : array
    copy of A, of type numpy array and with dtype changed to a valid one

    """

    # is_tensor: 1 for true, 0 for False, -1 for unknown
    try:
        is_tensor = int(A.type().startswith('torch'))
    except:
        is_tensor = -1

    if is_tensor == 1 :
        B = A.numpy()
    else: 
        B = copy.deepcopy(A)

    adt = B.dtype

    # all floats to float32 ( **just a start, continue!** )
    DO_FLOAT_HEADER = 0
    if np.issubdtype(adt, np.floating) :
        DO_FLOAT_HEADER = 1
        if not(isinstance(adt, np.float32)) :
            B = B.astype(np.float32)

    return B


def calc_xlim_range(xvals, yvals, tail_perc=1):
    """For a given set of xvals and yvals, estimate a reasonable pair of
xlim values (i.e., min and max values for x-axis when plotting). This
is primarily based on the size desired tail distribution percentiles,
given by the tail_perc kwarg

    """
    
    BAD_RETURN = -1, []

    # basic checks
    try:
        N = len(xvals)
        M = len(yvals)
        if N != M :
            BASE.EP1("xvals and yvals must be equal length")
            return BAD_RETURN
    except:
        BASE.EP1("xvals and yvals should each be iterable, like a list")
        return BAD_RETURN

    yabs = np.abs(yvals)
    totsum = np.sum(yabs)

    # check cumulative sums: left tail
    lsum = 0.0
    ll = 0
    while ll < N :
        lsum+= np.abs(yvabs[ll])
        if lsum/totsum >= tail_perc :
            break
        ll+=1

    # check cumulative sums: right tail
    rsum = 0.0
    rr = N-1
    while rr >= 0 :
        rsum+= np.abs(yvabs[rr])
        if rsum/totsum >= tail_perc :
            break
        rr-=1

    # **** ADJUST
    return 0, [-0.04 0.04]

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")
