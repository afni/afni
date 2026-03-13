#!/usr/bin/env python

import sys, copy
import numpy as np

from afnipy import afni_util as au
from afnipy import afni_base as ab
from afnipy import lib_nifti as NIF

# ============================================================================
# 
# A library file for testing the BRIK/HEAD -> NIFTI conversion goes.
#
# ============================================================================

DEF_ssep = ':::'

# ============================================================================

def read_brick_attributes(fname, ssep=DEF_ssep, verb=1):
    """For a given AFNI-formatted BRIK/HEAD dset, called fname, read in
all attributes to a dictionary. This uses a shell command call to
3dAttribute.

Parameters
----------
fname : str
    dset filename
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
    dictionary of AFNI header attributes

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
    dictionary of AFNI header attributes

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

if __name__ == "__main__" :

    # Ex. 1: stats file
    fname1 = '~/AFNI_data6/FT_analysis/FT.results/stats.FT+tlrc.'
    is_fail, Adict1 = read_brick_attributes(fname1)
