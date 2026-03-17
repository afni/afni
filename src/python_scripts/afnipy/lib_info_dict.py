#!/usr/bin/env python

# Make a dictionary of the 3dinfo props of a dset

#ver='1.0' ; date='Sept 16, 2019'
# + [PT] start
#
ver='1.1' ; date='Dec 10, 2025'
# + [PT] update some of the neater formatting parts
#
###############################################################################

import sys, os
import json
from afnipy import afni_base as BASE
from afnipy import afni_util as UTIL

# list of 3dinfo opts not to use (reasons shown)
list_3dinfo_ignore = [ \
    '-HELP',              # show help
    '-NA_flag',           # modifier
    '-VERB',              # modifier
    '-all_opts',          # show opts
    '-atr_delim',         # modifier
    '-echo_edu',          # show cmd (shouldn't happen)
    '-h',                 # show help
    '-h_aspx',            # show help
    '-h_find',            # show help
    '-h_raw',             # show help
    '-h_spx',             # show help
    '-h_view',            # show help
    '-h_web',             # show help
    '-hdr',               # modifier
    '-header_line',       # modifier
    '-help',              # show help
    '-hview',             # show help
    '-hweb',              # show help
    '-monog_pairs',       # req 2+ dsets
    '-n[i|j|k',           # existed at some point
    '-no_hist',           # modifier
    '-same_all_grid',     # req 2+ dsets
    '-same_center',       # req 2+ dsets
    '-same_delta',        # req 2+ dsets
    '-same_dim',          # req 2+ dsets
    '-same_grid',         # req 2+ dsets
    '-same_obl',          # req 2+ dsets
    '-same_orient',       # req 2+ dsets
    '-sb_delim',          # modifier opt
    '-short',             # modifier
    '-sval_diff',         # req 2+ dsets
    '-val_diff',          # req 2+ dsets
    '-verb',              # modifier
]

# ============================================================================

#fname = 'MNI152_2009_template_SSW.nii.gz'
def get_all_3dinfo_dset_basic(fname) :
    '''Input: a dset name 'pon which to run 3dinfo. This function makes a
simple dictionary of all of single-dset attributes. 

The '-' part of opt name is part of each key here.

Helpy opts or comparison ones like -same* are _not_ included (see
list_3dinfo_ignore for reference of excluded opts)

There is no filtering of empty fields here-- the bog standard outputs
are all returned; each value in the dictionary is a list of string(s),
which may be null.  

See the possibly more helpful "get_all_3dinfo_dset_neatly()" function
for similar functionality but with additional filtering/proc'ing of
keys and values.

Parameters
----------
fname : str
    name of a dataset

Returns
-------
dict_info : dict
    dictionary whose keys are 3dinfo option flags (with leading '-') and
    values are associated dset values for each

    '''

    # ------- Quick check on dset being valid
    cmd = '''3dinfo -prefix {fff}'''.format(fff=fname)
    com = BASE.shell_com(cmd, capture=1, save_hist=0)
    com.run()

    if com.so[0] == 'NO-DSET' :
        print('+* WARNING: No valid dset of that name: {fff}\n.'
              '   Bye.'.format(fff=fname))
        return {}

    # ------- Get list of all opts in 3dinfo
    cmd = '''apsearch -list_popts 3dinfo'''
    com = BASE.shell_com(cmd, capture=1, save_hist=0)
    com.run()

    # filter out some ones we don't want
    list_3dinfo_opts = []
    for x in com.so:
        y = x.strip()
        if not(list_3dinfo_ignore.__contains__(y)) :
            list_3dinfo_opts.append(y)

    dict_info = {}

    for opt in list_3dinfo_opts:
        cmd = '''3dinfo {ooo} {fff}'''.format( ooo=opt, fff=fname )
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        dict_info[opt] = com.so

    return dict_info

# -------------------------------------

def make_neater_3dinfo_dict(dd) :
    '''Make a "neater" version of a simple dictionary of 3dinfo
attributes, such as was created by get_all_3dinfo_dset_basic().

The "neater" version of the values in dd specifically means that
single line strings are processed as follows:
+ try splitting at whitespace; 
+ if nothing split, try splitting at |
+ and attach either a list of str (if something split) or else just
  the str itself

Parameters
----------
dd : dict
    dictionary whose keys are 3dinfo option flags (with leading '-')
    and simple values are associated dset values for each

Returns
-------
dd_new : dict
    dictionary whose keys are 3dinfo option flags (with leading '-')
    and a "neatened" form of values, as described above

    '''

    dd_new = {}

    for key in dd.keys():

        # no items: leave as null string
        if not(dd[key]) :
            dd_new[key] = ''
            
        # single item: see if it should be split
        elif len(dd[key]) == 1: 
            sss = dd[key][0].strip()
            if sss :
                # first try splitting this at whitespaces
                ttt = sss.split()
                # if nothing split, then try to split the original
                # with the pipe char
                if len(ttt) == 1 :
                    ttt = sss.split('|')
                
                # if neither splitter had any effect, just attach the
                # string; otherwise, attach the list
                if len(ttt) == 1 :
                    dd_new[key] = sss
                else:
                    dd_new[key] = ttt

        # multiple items:  just keep as is
        else:
            dd_new[key] = dd[key]

    return dd_new

# -------------------------------------

def get_all_3dinfo_dset_neatly(fname, numberize_values=False, 
                               remove_dash_in_keys=True) :
    '''For a dset fname, this function makes a dictionary of all
single-dset attributes; it also neatens the values, using
make_neater_3dinfo_dict().

When numberize_values is True, values that are just a single number
will be converted to a numerical type (rather than a string), and a
list of numbers will have each element converted.

When remove_dash_in_keys is True, each key will have the leading '-'
removed from it (which exist by default because each is just an opt of
3dinfo).

Parameters
----------
fname : str
    name of a dataset
numberize_values : bool
    for values that are a single number or just a set of numbers, 
    convert the value item(s) to an number type (rather than str)
remove_dash_in_keys: bool
    remove the leading '-' from each key's name (which is there by 
    default)

Returns
-------
DD_new : dict
    dictionary whose keys are 3dinfo option flags and values are
    associated dset values for each; both keys and values can get
    neatened in various ways here (see kwargs)

    '''

    DD     = get_all_3dinfo_dset_basic(fname)
    DD_new = make_neater_3dinfo_dict(DD)

    if numberize_values :
        DD_new = make_number_values(DD_new)

    if remove_dash_in_keys :
        DD_new = make_keys_dashless(DD_new)

    return DD_new


def make_keys_dashless(DD):
    """Go through all keys in the dictionary DD remove any leading dash in
the [0]th element of a key.  This is useful since many keys here are
option names in 3dinfo.

Parameters
----------
DD : dict
    dictionary to have each value or element of values numerified

Returns
-------
EE : dict
    processed version of DD; should be same number of keys and values

    """

    EE = {}

    # go through all keys, and remove any '-' at start of str
    for key in DD.keys():
        val = DD[key]

        if isinstance(key, str) :
            if len(key) and key[0] == '-' :
                EE[key[1:]] = val
            else:
                EE[key] = val
        else:
            EE[key] = val

    return EE


def make_number_values(DD):
    """Go through all values in the dictionary DD and convert any that can
to simple numerical types (bool for True and False; float; int);
otherwise, leave as a string.  If the value is a list, go through each
element.

Parameters
----------
DD : dict
    dictionary to have each value or element of values numerified

Returns
-------
EE : dict
    processed version of DD; should be same number of keys and values

    """

    EE = {}

    # go through all values, and see what values (or their elements,
    # if a list) can be converted
    for key in DD.keys():
        val = DD[key]

        # when the values are a list, check each one to convert
        if isinstance(val, list) :
            L = []
            for vv in val:
                # get each element as either a numerical version or
                # just the str itself...
                ww, wtype = UTIL.try_convert_bool_float_int_str(vv)
                if ww is not None :
                    L.append(ww)
                else:
                    # ... or whatever it was originally
                    L.append(vv)
            EE[key] = L

        # the only other type of values should be simply be str; see
        # if any of those convert gracefully to numerical types,
        # otherwise, keep the string as the value
        else:
            ww, wtype = UTIL.try_convert_bool_float_int_str(val)
            if ww is not None :
                EE[key] = ww
            else:
                EE[key] = val

    return EE
            

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")



