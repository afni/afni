#!/usr/bin/env python

# A library of default settings for SIMBA processing
# ============================================================================

import sys, os, copy, glob

# ============================================================================
# various program parameters and settings

# general program parameters
do_clean      = 'Yes'
verb          = 1
do_log        = False

# general model parameters
model_ls      = 5.0             # for Matern kernel; can have phys units of mm
model_nu      = 1.5
model_name    = 'VI'
model_L       = 1214

# model-specific parameters
model_VI_ELBO_diff_tol  = 1e-8
model_VI_para_diff_tol  = 1e-8
model_VI_elbo_stop      = True
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

# figure plotting parameters
fig_ext       = 'png'
fig_dpi       = 300

# ============================================================================

# available model names
LIST_all_simba_model_name = ['Gibbs', 'VI']
STR_all_simba_model_name  = ', '.join(LIST_all_simba_model_name)

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
    'model_name'      : model_name,
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
    'fig_ext'         : fig_ext,
    'fig_dpi'         : fig_dpi,
}


# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")
