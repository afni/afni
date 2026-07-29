#!/usr/bin/env python

# A set of functions for getting useful items (individual or subsets)
# from 3dinfo.  This library complements the lib_info_dict.py library,
# which is focused on getting many/all single-valued items from 3dinfo.
# 
# Functions here may have one or more datasets as inputs. This mostly
# wraps around convenient checks/calcs we would get from 3dinfo on the
# command line.
#
# In theory, many functions here could be compressed into fewer ones
# and have the option name included as an arg, say.  However, there
# are so many different parsings for int/float/str, and different
# numbers of outputs, we have just gone with the longer list of
# functions for convenience.
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
#
# ============================================================================

from   afnipy import afni_base          as ab
from   afnipy import afni_util          as au

# ============================================================================

def is_atlas_or_labeltable(A):
    """Does dset A have an atlas or labeltable?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
val : int
    1 for 'yes, has atlas or labeltable'; 0 for not so

"""

    val = 0
    BAD_RETURN = (-1, val)

    cmd  = '3dinfo -is_atlas_or_labeltable {}'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse: [0]th and [1]th values will be same now
    try:
        val  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, val


def is_labeltable(A):
    """Does dset A have a labeltable?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
val : int
    1 for 'yes, has labeltable'; 0 for not so

"""

    val = 0
    BAD_RETURN = (-1, val)

    cmd  = '3dinfo -is_labeltable "{}"'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse: single int value in a list
    try:
        val  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, val


def get_nv(A):
    """How many values (or volumes) in dset A?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
val : int
    number of volumes or values ('-nv', in 3dinfo parlance)

"""

    val = 0
    BAD_RETURN = (-1, val)

    cmd  = '3dinfo -nv "{}"'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return BAD_RETURN

    # now parse
    try:
        val  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, val

def get_n3(A):
    """What is the matrix dims of dset A?

Parameters
----------
A : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
n3 : list
    list of 3 int values, the matrix dims of A ('-n3', in 3dinfo parlance)

"""

    val = [0, 0, 0]
    BAD_RETURN = (-1, val)

    cmd  = '3dinfo -n3 "{}"'.format(A)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            ab.EP1("Had 'NO-DSET' in row")
            return BAD_RETURN

    # now parse
    try:
        mmm = lll[0].split()
        is_fail, val, valtype = au.try_convert_bool_float_int_str_LIST(mmm)
        if is_fail or len(valtype) > 1 or valtype[0] != 'int' :
            ab.EP1("Failed parsing 3dinfo output for n3: {}".format(lll))
            return BAD_RETURN
    except:
        ab.EP1("Failed getting values for n3: {}".format(lll))
        return BAD_RETURN

    return 0, val

# --------------------------------------------------------------------------------

def is_same_grid(A, B):
    """Are the two dsets A and B on the same grid? Check with 3dinfo.

Parameters
----------
A : str
    name of a volumetric dset 
B : str
    name of a volumetric dset 

Returns
-------
is_fail : int
    0 for success, nonzero for failure
isg : int
    1 for 'yes, same grid'; 0 for not so

"""

    isg = 0
    BAD_RETURN = (-1, isg)

    cmd  = '3dinfo -same_grid {} {}'.format(A, B)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    lll  = com.so

    # verify dsets can be read by AFNI
    for row in lll:
        if row == 'NO-DSET' :
            return (-2, 0)

    # now parse: [0]th and [1]th values will be same now
    try:
        isg  = int(lll[0].strip())
    except:
        return BAD_RETURN

    return 0, isg

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

