#!/usr/bin/env python

# A library of option parsing for generating AP derivatives
# 
# Primarily used with ***
#
# written by PA Taylor and RC Reynolds (NIMH, NIH, USA) 
# 
# ==========================================================================

import sys, os
import glob
import copy

from afnipy import afni_base as ab

# ==========================================================================

# default input option/parameter settings
DEF = {
    'subj_dir'  : '',             # str, AP results directory for 1 subj
    'deriv_dir' : '',             # str, deriv dir to map to
    'ow_mode_top'  : 'simple_ok', # str, control of overwriting topdir
    'ow_mode_subj' : 'backup',    # str, control of overwriting subjdir
    'verb'      : 0,              # int, verbosity level
}

DEF_deriv_dir_base = 'bids_deriv'

# ==========================================================================

def is_valid_ap_res_dir(ap_res_dir):
    """Return 1 is all of the following are True (else, 0):
    Does input ap_res_dir exist, and is it a directory? 
    Does ap_res_dir contain an out.ss_review_uvar.json file?

Parameters
----------
ap_res_dir : str
    name of subject directory (which should be an afni_proc.py results dir)

Returns
-------
is_valid : int
    0 means it is valid AP results dir, nonzero means it ain't.

    """

    # simplify dir name
    ap_res_dir = ap_res_dir.rstrip('/')

    # does the directory exist?
    is_dir = os.path.isdir(ap_res_dir)
    if not(is_dir) :
        EP("Input 'ap_res_dir' does not exist: {}".format(ap_res_dir), 
           end_exit=False)
        return 1

    # is there a uvar file (which each AP results dir should have)?
    uvar_file = ap_res_dir + '/' + 'out.ss_review_uvars.json'
    is_uvar   = os.path.isfile( uvar_file )
    if not(is_uvar) :
        EP("Input 'ap_res_dir' does not have uvar: {}".format(uvar_file), 
           end_exit=False)
        return 1

    # if here, must be OK
    return 0

def determine_deriv_dir_name(deriv_dir, ap_res_dir, subj=''):
    """Figure out what the derivative directory name should be (with path
info), in a kind of triage, based on the inputs.  If deriv_dir is not
an empty string, then that will simply be the returned
name. Otherwise, the name will be a sub-directory off of ap_res_dir. A
default base name is known, and if the subj kwarg is non-null, that
will be appended to it with an underscore

Parameters
----------
deriv_dir : str
    possible name of output deriv_dir (that is, it *will* be, as long as
    it is not null str)
ap_res_dir : str
    if input deriv_dir is '', the we will make an output dir as a sub-
    directory of this dir (which should be the AP results dir)
subj : str
    subject ID, which can be appended at end of derived derivative dir
    name (*if* input deriv_dir is not '')

Returns
-------
deriv_dir : str
    the name we have determined for output deriv dir (could just be
    copy of input deriv_dir, if that is not empty str)

    """

    if deriv_dir :
        return deriv_dir

    # ... but if no deriv_dir was given, make a name (with path) for it
    if not(ap_res_dir) :
        ab.EP("Need a non-empty ap_res_dir entered here.")

    deriv_dir = ap_res_dir + '/' + DEF_deriv_dir_base
    
    if subj :
        deriv_dir+= '_' + subj

    return deriv_dir

# ================================ main =====================================

if __name__ == "__main__":

    sys.exit(0)
