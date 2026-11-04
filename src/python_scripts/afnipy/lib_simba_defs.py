#!/usr/bin/env python

# A library of default settings for SIMBA processing
# ============================================================================

import sys, os, copy, glob

# ============================================================================
# various program parameters and settings

# general program parameters
do_clean      = 'Yes'
verb          = 1
do_log        = 'No'

# general model parameters
model_ls      = 8.0             # for Matern kernel; can have phys units of mm
model_nu      = 1.5
model_pim     = 'VI'
model_L       = 1214

# model-specific parameters
model_VI_ELBO_diff_tol  = 1e-8
model_VI_para_diff_tol  = 1e-8
model_VI_elbo_stop      = 'Yes'
model_VI_max_iter       = 50000
model_VI_verbose        = 5000
model_Gibbs_burnin      = 4000
model_Gibbs_mcmc_sample = 1000
model_Gibbs_thin        = 1

# PPC parameters
ppc_n_mcmc    = 10 ###150 ****
ppc_alpha     = 0.2
ppc_color     = "#E69F00"
ppc_label     = 'Posterior predictive'
ppc_xlim      = None

# figure plotting parameters
fig_ext       = 'png'
fig_dpi       = 300

# ============================================================================

# base of default workdir name
STR_workdir_base = '__wdir_simba_'

# available model posterior inference methods (PIMs)
LIST_all_simba_model_pim = ['Gibbs', 'VI']
STR_all_simba_model_pim  = ', '.join(LIST_all_simba_model_pim)

# lenscale parameter min and max values
RANGE_model_ls = {
    'lsmin' : 0.0,
    'lsmax' : 100.0,   # just a really large upper boundary now
    }

# ============================================================================

# default values for the main obj
DOPTS = {
    'user_opts'       : [],         # command the user ran
    'verb'            : verb,
    'overwrite'       : '',
    'do_clean'        : do_clean,
    'do_log'          : do_log,
    'inset'           : '',
    'infiles'         : [],
    'prefix'          : '',
    'mask'            : '',
    'ulay'            : '',
    'outdir'          : None,
    'workdir'         : '',
    'model_pim'       : model_pim,
    'model_L'         : model_L,
    'model_ls'        : model_ls,
    'model_nu'        : model_nu,
    'model_VI_ELBO_diff_tol'  : model_VI_ELBO_diff_tol,
    'model_VI_para_diff_tol'  : model_VI_para_diff_tol,
    'model_VI_elbo_stop'      : model_VI_elbo_stop,
    'model_VI_max_iter'       : model_VI_max_iter,
    'model_VI_verbose'        : model_VI_verbose,
    'model_Gibbs_burnin'      : model_Gibbs_burnin,
    'model_Gibbs_mcmc_sample' : model_Gibbs_mcmc_sample,
    'model_Gibbs_thin'        : model_Gibbs_thin,
    'ppc_n_mcmc'      : ppc_n_mcmc,
    'ppc_alpha'       : ppc_alpha,
    'ppc_color'       : ppc_color,
    'ppc_label'       : ppc_label,
    'ppc_xlim'        : ppc_xlim,
    'fig_ext'         : fig_ext,
    'fig_dpi'         : fig_dpi,
}


# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")
