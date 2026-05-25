#!/usr/bin/env python

# A library of default settings for SIMBA processing
# ============================================================================

import sys, os, copy, glob

# ============================================================================

# base of default workdir name
STR_workdir_base = '__wdir_brainteaser_'

# available model posterior inference methods (PIMs)
LIST_all_device = ['cpu', 'cuda']
STR_all_device  = ', '.join(LIST_all_device)

# model parameters from training, and 
model_orient = 'RSP'
model_matrix = 16

# ============================================================================
# default values, parameters and settings for the main obj

DOPTS = {
    'user_opts'       : [],         # command the user ran
    'verb'            : 1,
    'overwrite'       : '',
    'do_clean'        : 'Yes',
    'do_log'          : 'No',
    'inset'           : '',
    'comp_mask'       : '',
    'checkpoint'      : '',
    'prefix'          : '',
    'device'          : 'cpu',
    'outdir'          : None,
    'workdir'         : '',
}

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")
