#!/usr/bin/env python

import os, sys, copy
import numpy   as np
import nibabel as nib

from afnipy import afni_util         as au
from afnipy import afni_base         as ab
from afnipy import lib_nifti         as NIF
from afnipy import lib_nibabel_utils as lnu

# ============================================================================
# 
# A library file for testing the BRIK/HEAD -> NIFTI conversion goes.
#
# ============================================================================

DEF_ssep = ':::'

ALL_nifti1_keys = NIF.dict_nifti1.keys()

# ============================================================================

def read_brick_attributes(fname, ssep=DEF_ssep, verb=1):
    """For a given AFNI-formatted BRIK/HEAD dset, called fname, read in
all attributes to a dictionary. This uses a shell command call to
3dAttribute.

Parameters
----------
fname : str
    BRIK/HEAD-format dset filename
ssep : str
    string used as separator within attribute string lists shouldn't really
    ever need to change
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
Adict : dict
    dictionary of AFNI header attributes; each value is a list

    """

    BAD_RETURN = (-1, 0)

    # initialize default
    Adict = {}

    # read in attributes as a sorted list and convert to dict; make
    # separator something unique and nonexistent within HEAD file
    cmd = '''3dAttribute        \
    -all                        \
    -ssep "{ssep}"              \
    "{fname}"                   \
    | sort
    '''.format( fname = fname, 
                ssep = ssep )
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    L = copy.copy(com.so)

    # convert list to dict
    is_fail, Adict = parse_attribute_list(L, ssep=ssep, verb=verb)
    if is_fail :
        return BAD_RETURN

    return 0, Adict

def parse_attribute_list(L, ssep=DEF_ssep, verb=1):
    """Parse the list of terminal text lines dumped by 3dAttribute, which
have been stored in a list L.

*** To decide at some point: do we care that BRICK_KEYWORDS can be
    '(null)', and should we treat that as more than a literal str?

Parameters
----------
L : list
    list of header attributes from running 3dAttribute on cmd line
ssep : str
    string used as separator within attribute string lists shouldn't really
    ever need to change
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
Adict : dict
    dictionary of AFNI header attributes; each value is a list

    """

    BAD_RETURN = (-1, 0)

    # initialize default
    Adict = {}     # AFNI attribute dict

    for line in L :
        row = line.split(" = ")

        if len(row) < 2 :
            msg = "Line in attribute list contained no ' = ', only:\n"
            msg+= line
            ab.EP1(msg)
            return BAD_RETURN

        key = row[0].strip()
        val = "".join(row[1:])

        # sign of val being a string; check for this specially, so we
        # preserve white space if a string
        if val.endswith(ssep) :
            # remove ssep at end and listify
            vvv = val[:-len(ssep)]
            www = vvv.split(ssep)

            # special case, of wanting to split at ';', too
            if key == 'BRICK_STATSYM' :
                # add the '[0]' to not get a list of lists
                www = [w.split(';') for w in www][0]

            Adict[key] = www

        else:
            # split at whitespace and convert each element
            vvv = val.split()
            lll = []
            for v in vvv:
                y, ytype = au.try_convert_bool_float_int_str(v)
                lll.append(y)
            Adict[key] = copy.deepcopy(lll)

    return 0, Adict

# ------------------------------------------------------------------------

def read_nifti_fields(fname, verb=1):
    """For a given NIFTI-formatted dset, called fname, read in all header
fields (i.e., attributes) to a dictionary. This uses nibabel.

Parameters
----------
fname : str
    NIFTI dset filename
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
Ndict : dict
    dictionary of NIFTI header fields; each value is a list

    """

    BAD_RETURN = (-1, 0)

    # initialize default
    Ndict = {}

    nibobj = nib.load(os.path.expanduser(fname))
    nibhdr = nibobj.header.copy()

    for key in ALL_nifti1_keys :
        val = nibhdr.get(key)
        # this next step occurs bc some values are ndim=0 arrays
        val_list, val_type = lnu.try_convert_list_float_int_str_arr(val)
        Ndict[key] = val #*** val_list

        if verb > 1 :
            print("   {:<20s}  : {}".format(key, val_list))

    return 0, Ndict

# --------------------------------------------------------------------------

def make_nifti_from_brick(fname, name_nifti=None, clean_nifti=True, verb=1):
    """For a given BRIK/HEAD dset, called fname, first use the command
line to copy it to a temporary NIFTI dset. Then read in that new
file's NIFTI header and return it as a dictionary.

Users can optionally give a name to the temporary dset, as well clean
(=purge) it.

Parameters
----------
fname : str
    AFNI BRIK/HEAD dset filename
name_nifti : str
    name of temporary NIFTI (def: make name with random chars)
clean_nifti : bool
    decide whether to remove the NIFTI (True -> remove)
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
name_nifti : str
    name of temporary (or not) NIFTI file, in case it is useful
Ndict : dict
    dictionary of NIFTI header fields; each value is a list

    """

    BAD_RETURN = (-1, '', 0)

    # initialize default
    Ndict = {}

    if name_nifti is None :
        cmd  = '''3dnewid -fun11'''
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        sss  = com.so[0]
        name_nifti = "_tmp_" + sss + ".nii.gz"

        if verb > 1 :
            ab.IP("Making temporary NIFTI file: " + name_nifti)

    elif not isinstance(name_nifti, str) :
        ab.EP("kwarg name_nifti must have type str, not {}"
              "".format(ab.simple_type(name_nifti)))

    # make tmp nifti
    cmd  = '''3dcopy -overwrite {} {}'''.format(fname, name_nifti)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()

    # make the dictionary of tmp nifti dset
    is_fail, Ndict = read_nifti_fields(name_nifti, verb=verb)
    if is_fail :
        ab.EP("Could not make dictionary from NIFTI file:", name_nifti)

    if clean_nifti :
        cmd  = '''\\rm {}'''.format(name_nifti)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if verb > 1 :
            ab.IP("Removed temp NIFTI file: " + name_nifti)

    return 0, name_nifti, Ndict

def compare_nifti_headers(all_Ndict, verb=1):
    """For a given collection of NIFTI-header-dictionaries all_Ndict, go
through and see where there are any differences.

Parameters
----------
fname : str
    AFNI BRIK/HEAD dset filename
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
report : dict
    dictionary of NIFTI header fields; each value is a list

    """






# ==========================================================================

if __name__ == "__main__" :

    # Ex. 1: stats file
    fname1A = '~/AFNI_data6/FT_analysis/FT.results/stats.FT+tlrc.'
    is_fail1A, Adict1 = read_brick_attributes(fname1A)

    fname1N = '~/AFNI_data6/FT_analysis/FT.results/stats.FT.nii.gz'
    is_fail1N, tmp_nameN, Ndict1 = make_nifti_from_brick(fname1A, verb=2)
