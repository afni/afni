#!/usr/bin/env python

# ============================================================================
# 
# A library file for testing the BRIK/HEAD -> NIFTI conversion goes.
#
# auth: PA Taylor (SSCC, NIMH, NIH, USA)
# 
# ============================================================================

import os, sys, copy
import numpy   as np
import nibabel as nib

from afnipy import afni_util         as au
from afnipy import afni_base         as ab
from afnipy import lib_nibabel_utils as lnu
from afnipy import lib_nifti         as NIF

# ============================================================================

DEF_ssep = ':::'            # default string separator in 3dAttribute output
DEF_fl_tol = 0.001          # default float tolerance in fl point comparisons

ALL_nifti1_keys = NIF.dict_nifti1.keys()

# ============================================================================

def read_brick_attributes_3dA(fname, ssep=DEF_ssep, verb=1):
    """For a given AFNI-formatted BRIK/HEAD dset, called fname, read in
all attributes to a dictionary. 

This uses a shell command call to 3dAttribute.

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
    # separator something unique and nonexistent within HEAD file; NB:
    # 3dAttribute output cannot be sorted, because annoyingly some
    # attributes are actually multi-line, not just explicitly
    # including a '\n' char
    cmd = '''3dAttribute        \
    -all                        \
    -ssep "{ssep}"              \
    "{fname}"
    '''.format( fname = fname, 
                ssep = ssep )
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()
    L = copy.copy(com.so)

    if stat :
        ab.EP1("3dAttribute failed for dset: {}".format(fname))
        return BAD_RETURN

    # convert list to dict
    is_fail, Adict = parse_attribute_list(L, ssep=ssep, verb=verb)
    if is_fail :
        return BAD_RETURN

    return 0, Adict

def parse_attribute_list(L, ssep=DEF_ssep, verb=1):
    """Parse the list of terminal text lines dumped by 3dAttribute, which
have been stored in a list L.

Note: most attributes are a single line and contain explicit '\n'
characters, but some like MARKS_XYZ and MARKS_HELP actually take up
multiple lines. This complicates our life, by having to parse a bit
more. Sigh.

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

    # go through list L first, because some attributes actually run
    # over into multiple lines, rather than just containing '\n' in
    # them; here, we concatenate those multilines with explicit '\n'
    # values
    N = len(L)
    M = []
    prev = L[0] # this one must have a '='
    if not('=' in prev):
        msg = "First Line in attribute in list must contain '=':\n" + prev
        ab.EP1(msg)
        return BAD_RETURN
    for ii in range(1, N):
        line = L[ii]
        if '=' in line :
            M.append(prev)
            prev = line
        else:
            prev+= '\n' + line
    M.append(prev)

    # now, each line should have (at least) one '='
    for line in M :
        row = line.split(" = ")

        # should never happen, due to above creation of M
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
            # remove ssep at end (there can be more than one) and listify
            vvv = val[:-len(ssep)]
            while vvv.endswith(ssep) :
                vvv = vvv[:-len(ssep)]
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

    fname_full = os.path.expanduser(fname)

    if not(os.path.exists(fname_full)) :
        ab.EP1("Cannot find dset: {}".format(fname_full))
        return BAD_RETURN

    # initialize default
    Ndict = {}

    nibobj = nib.load(fname_full)
    nibhdr = nibobj.header.copy()

    for key in ALL_nifti1_keys :
        val = nibhdr.get(key)
        # this next step occurs bc some values are ndim=0 arrays
        val_list, val_type = lnu.try_convert_list_float_int_str_arr(val)
        Ndict[key] = val_list

        if verb > 1 :
            print("   {:<20s}  : {}".format(key, val_list))

    return 0, Ndict

# --------------------------------------------------------------------------

def compare_nifti_from_brick_with_self_copy(fname, fl_tol=DEF_fl_tol, 
                                            name_nifti=None, clean_nifti=True, 
                                            verb=1):
    """Start with a BRIK/HEAD file called fname, first do a direct mapping
to a set of NIFTI header fields, and then compare that with the set of
NIFTI fields arising from a real NIFTI dset created by 3dcopy'ing the
original.

The output is the same set of comparison items from
compare_and_disp_two_nifti_headers().

Parameters
----------
fname : str
    AFNI BRIK/HEAD dset filename
fl_tol : float
    tolerance for differences of floating point values
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
Cdict_diffs : dict
    dictionary of key-value pairs where values differed between the two inputs

    """

    BAD_RETURN = (-1, {})

    # get NIFTI header from AFNI brik/head info
    is_fail2, NdictA = NIF.make_nifti_header_from_brik( fname, verb=verb )
    if is_fail2 :    return BAD_RETURN

    # make the NIFTI dset and get its header for comparison
    is_fail3, name_nifti, NdictB = \
        make_nifti_dset_from_brik(fname, name_nifti=name_nifti, 
                                  clean_nifti=clean_nifti, verb=verb)
    if is_fail3 :    return BAD_RETURN

    # do the comparison of headers
    all_Ndict = [ NdictA, NdictB ]

    if verb > 2 :
        ab.IP("About to compare headers:")
        print("   Display NdictA:")
        is_failA = display_simple_dict(NdictA)
        print("   Display NdictB:")
        is_failB = display_simple_dict(NdictB)


    is_fail4, Cdict_diffs = \
        compare_and_disp_two_nifti_headers(all_Ndict, fl_tol=fl_tol, 
                                           do_disp_diffs=True, 
                                           titleA='BRIK/HEAD',
                                           titleB='NIFTI',
                                           verb=verb)
    if is_fail4 :    return BAD_RETURN


    return is_fail4, Cdict_diffs
    

# --------------------------------------------------------------------------

def make_nifti_dset_from_brik(fname, name_nifti=None, clean_nifti=True, 
                              verb=1):
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

        if stat :
            ab.EP1("3dnewid failed")
            return BAD_RETURN

        if verb > 1 :
            ab.IP("Making temporary NIFTI file: " + name_nifti)

    elif not isinstance(name_nifti, str) :
        ab.EP("kwarg name_nifti must have type str, not {}"
              "".format(ab.simple_type(name_nifti)))

    # make tmp nifti
    cmd  = '''3dcopy -overwrite {} {}'''.format(fname, name_nifti)
    com  = ab.shell_com(cmd, capture=1)
    stat = com.run()

    if stat :
        ab.EP1("3dcopy failed for dset: {}".format(fname))
        return BAD_RETURN

    # make the dictionary of tmp nifti dset
    is_fail, Ndict = read_nifti_fields(name_nifti, verb=verb)
    if is_fail :
        ab.EP("Could not make dictionary from NIFTI file:", name_nifti)

    if clean_nifti :
        cmd  = '''\\rm {}'''.format(name_nifti)
        com  = ab.shell_com(cmd, capture=1)
        stat = com.run()
        if stat :
            ab.EP1("'rm' failed for dset: {}".format(name_nifti))
            return BAD_RETURN

        if verb > 1 :
            ab.IP("Removed temp NIFTI file: " + name_nifti)

    return 0, name_nifti, Ndict

def compare_nifti_headers(all_Ndict, fl_tol=DEF_fl_tol, verb=1):
    """For a given collection of NIFTI-header-dictionaries all_Ndict, go
through and see where there are any differences.

Note a special case: if any value is None, even if all of them are
None for a given key, we will consider this a mismatch, because None
is not a valid NIFTI field value (just a Pythonic placeholder).

This function can also display the diffs, if do_disp_diffs is true.
This is probably most useful only for the case len(all_Ndict) is
small, like 2 or so, because the columns can get wide.

Parameters
----------
fname : str
    AFNI BRIK/HEAD dset filename
fl_tol : float
    tolerance for differences of floating point values
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
Cdict_base : dict
    dictionary of how "base" values (i.e., the values from the first
    list in the input all_Ndict, against which all others were compared)
    for each key 
Cdict_count : dict
    dictionary of how many differences there are across NIFTI headers,
    per key
Cdict : dict of lists
    comparison results: dictionary of NIFTI header fields; each key is
    a NIFTI field, and each value is a list; within each comparison list, 
    a None in the i-th list entry means that the i-th dset's value matched
    that of the 0-th, and a non-None is a list of the differing value(s).

    """

    BAD_RETURN = (-1, {}, {}, {})

    N = len(all_Ndict)

    if not(N) :
        ab.EP1("No dictionaries input for comparison")
        return BAD_RETURN
    elif N == 1 :
        ab.EP1("Only 1 dictionary input for comparison---not enough")
        return BAD_RETURN

    # give a simpler name to the [0]th dict, against which others are comp'ed
    base_dict = all_Ndict[0]

    # initialize defaults
    Cdict = {}                # store diffs for output report
    Cdict_base  = {}          # store base vals
    Cdict_count = {}          # store count of diffs

    for key in ALL_nifti1_keys :
        # value and comparison result for base_dict
        val_b = base_dict[key]
        Cdict_base[key] = val_b
        Cdict_count[key] = 0
        L = [None]

        for ii in range(1, N):
            comp_dict = all_Ndict[ii]
            val_c     = comp_dict[key]

            is_fail, is_same = is_same_nifti_field_values(val_b, val_c, 
                                                          fl_tol=fl_tol)
            if is_fail :
                ab.EP1("Failed to compare values for key: " + key)
                return BAD_RETURN

            if is_same == True :
                L.append(None)
            else: 
                L.append(val_c)
                Cdict_count[key]+= 1

            # ... and retain the list of possible diffs
            Cdict[key] = copy.deepcopy(L)

    return 0, Cdict_base, Cdict_count, Cdict

def compare_and_disp_two_nifti_headers(all_Ndict, fl_tol=DEF_fl_tol, 
                                       do_disp_diffs=True, 
                                       titleA='', titleB='', verb=1):
    """A special case of compare_nifti_headers(...), with
len(all_Ndict)=2. This also adds the (optional, but likely useful)
feature of displaying the diffs.

Besides a fail/succeed int, this function returns a list with two
items: copies of the input dictionaries, but only with the key-value
pairs where there were value differences.

Parameters
----------
fname : str
    AFNI BRIK/HEAD dset filename
fl_tol : float
    tolerance for differences of floating point values
do_disp_diffs : bool
    should the differences be displayed?
titleA : str
    provide a string to put a label over the first col of values
titleB : str
    provide a string to put a label over the second col of values
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
all_diff_Ndict : dict
    dictionary of 2

    """

    BAD_RETURN = (-1, []) 

    num_dict = len(all_Ndict)
    if num_dict != 2 :
        ab.EP1("This function processes exactly 2 dicts, not {}".format(num_dict))
        return BAD_RETURN

    # do the comparison
    is_fail, Cdict_base, Cdict_count, Cdict = \
        compare_nifti_headers(all_Ndict, fl_tol=fl_tol, verb=verb)

    # two dictionaries with same keys
    DA = {}
    DB = {}
    count = 0

    for key in Cdict_base.keys():
        ndiff = Cdict_count[key]
        if ndiff :
            DA[key] = all_Ndict[0][key]
            DB[key] = all_Ndict[1][key]
            count+=1

    all_diff_Ndict = [DA, DB]

    if do_disp_diffs :
        if count :
            is_fail = display_simple_dict_pair(all_diff_Ndict,
                                               titleA=titleA,
                                               titleB=titleB, 
                                               verb=verb)
        else:
            msg = "++ No differences to display"
            print("-"*len(msg))
            print(msg)
            print("-"*len(msg))

    return 0, all_diff_Ndict

def is_same_nifti_field_values(a, b, fl_tol=DEF_fl_tol):
    """Compare two fields a and b from the NIFTI header dictionaries
created here. Each of a and b will be lists, by definition, and so we
compare element by element.

There is a default tolerance for floating point diffs, given by the
kwarg fl_tol.

Parameters
----------
a: list
    one of the NIFTI header field items to compare, necessarily a list
b: list
    one of the NIFTI header field items to compare, necessarily a list
fl_tol : float
    tolerance for differences of floating point values

Returns
-------
is_fail : int
    0 on success, nonzero on failure
is_same : bool
    True for same, False for different

    """

    BAD_RETURN = (-1, False)

    Na = len(a)
    Nb = len(b)

    # compare len of lists first
    if Na != Nb :
        return 0, False
    
    # go through all comparisons
    is_same = True

    for ii in range(Na):
        if a[ii] is None or b[ii] is None :
            # even if BOTH are false, we will consider this a
            # mismatch, because None is not a valid NIFTI field value
            # (just a Pythonic placeholder).
            is_same = False
        elif isinstance(a[ii], bytes) or isinstance(b[ii], bytes) or \
           isinstance(a[ii], int) or isinstance(b[ii], int) :
            if a[ii] != b[ii] :
                is_same = False
        elif isinstance(a[ii], float) or isinstance(b[ii], float) :
            if np.abs(a[ii] - b[ii]) > fl_tol :
                is_same = False
        else:
            msg = "Should not reach here. Comparison vals are: "
            msg+= "{} and {}".format(a, b)
            ab.WP(msg)
            return BAD_RETURN

    return 0, is_same


def display_simple_dict(D, top_bot_lines=True, verb=1):
    """For a dictionary D, display its contents in a reasonably nice way.

We assume that each key is a string, and each value is a list.

This works best for displaying NIFTI header dictionaries.  It will
work for AFNI BRIK/HEAD attribute dictionaries, but some of the values
in them can be very long (like FDRCURVE* lists), so the formatting
might be a bit odd.

Parameters
----------
D : dict
    dictionary
top_bot_lines : bool
    add lines at top and bot of list
verb : int
    verbosity

Returns
-------
is_fail : int
    0 on success, nonzero on failure

    """
    
    BAD_RETURN = -1

    # verify simplicity of input
    if not(is_simple_dict(D)):
        return BAD_RETURN

    # make stringified version of D to display -> SD
    is_fail, SD, Lkey, Lval = convert_simple_dict_to_str_dict(D)

    # total length (= sum of pieces plus ' : ')
    ssep = ' : '
    Ltot = Lkey + Lval + len(ssep)

    # now, ready to display stringified dict
    if top_bot_lines :
        print('-' * Ltot)
    for key in SD.keys():
        print("{:<{}s}{}{:<{}s}".format(key, Lkey, ssep, SD[key], Lval))
    if top_bot_lines :
        print('-' * Ltot)

    return 0

def display_simple_dict_pair(all_Ndict, top_bot_lines=True, 
                             titleA='', titleB='', verb=1):
    """For a list of 2 dictionaries, which we assume have the same keys,
the contents in a reasonably nice way.

For the dictionaries, we assume that each key is a string, and each
value is a list.

See display_simple_dict_pair() for why this likely is best for NIFTI
headers, but not AFNI BRIK/HEAD attributes.

Parameters
----------
DA : dict
    dictionary
DB : dict
    dictionary
top_bot_lines : bool
    add lines at top and bot of list
titleA : str
    provide a string to put a label over the first col of values
titleB : str
    provide a string to put a label over the second col of values
verb : int
    verbosity

Returns
-------
is_fail : int
    0 on success, nonzero on failure

    """
    
    BAD_RETURN = -1

    ssep = ' : '
    sdiv = '  '

    num_dict = len(all_Ndict)
    if num_dict != 2 :
        ab.EP1("This function processes exactly 2 dicts, not {}".format(num_dict))
        return BAD_RETURN

    # just rename elements of input list, for simplicity
    DA = all_Ndict[0]
    DB = all_Ndict[1]

    # verify simplicity of input
    if not(is_simple_dict(DA)) or not(is_simple_dict(DB)):
        return BAD_RETURN

    # make stringified version of D to display -> SD
    is_failA, SDA, LkeyA, LvalA = convert_simple_dict_to_str_dict(DA)
    if is_failA :
        return BAD_RETURN

    is_failB, SDB, LkeyB, LvalB = convert_simple_dict_to_str_dict(DB)
    if is_failB :
        return BAD_RETURN

    # ----- deal with titles, perhaps even extending the col width for printing

    if len(titleA) > LvalA :
        LvalA = len(titleA)
    if len(titleB) > LvalB :
        LvalB = len(titleB)

    if titleA or titleB :
        line = "{:<{}s}{}".format("key", LkeyA, ' '*len(ssep))
        line+= "{:<{}s}{}".format(titleA, LvalA, sdiv)
        line+= "{:<{}s}".format(titleB, LvalB)
        print(line)

    # total length (= sum of pieces plus ' : ' and '  ')
    Ltot = LkeyA + LvalA + LvalB + len(ssep)+ len(sdiv)

    # now, ready to display stringified dict
    if top_bot_lines :
        print('-' * Ltot)
    for key in SDA.keys():
        line = "{:<{}s}{}{:<{}s}".format(key, LkeyA, ssep, SDA[key], LvalA)
        line+= "{}{:<{}s}".format(sdiv, SDB[key], LvalB)
        print(line)
    if top_bot_lines :
        print('-' * Ltot)

    return 0


def convert_simple_dict_to_str_dict(D):
    """For a dictionary D to be a "simple dictionary" here, we mean that:
+ it is a dictionary
+ each key is a str
+ each value is a list.

This function converts the simple dict to a dict where the values are
str forms of themselves (for display). Also return the max length of
the keys (Lkey) and of the values (Lval).

Parameters
----------
D : dict
    dictionary, a contender for being simple

Returns
-------
is_fail : int
    0 on success, nonzero on failure
SD : dict
    dictionary where keys and values are str
Lkey : int
    length of max key string
Lval : int
    length of max value string

    """

    BAD_RETURN = (-1, {}, 0, 0)

    # verify simplicity of input
    if not(is_simple_dict(D)):
        return BAD_RETURN

    # prepare to make stringified version of D to display
    SD = {}
    all_len_key = []
    all_len_val = []
    for key in D.keys():
        all_len_key.append( len(key) )
        # prep values, including putting [] around actual lists
        val  = D[key]
        sval = ', '.join([str(x) for x in val])
        if len(val)>1 :
            sval = '[' + sval + ']'
        all_len_val.append( len(sval) )
        SD[key] = sval

    Lkey = max(all_len_key)
    Lval = max(all_len_val)

    return 0, SD, Lkey, Lval

def is_simple_dict(D):
    """For a dictionary D to be a "simple dictionary" here, we mean that:
+ it is a dictionary
+ each key is a str
+ each value is a list.

This function verifies that. It returns 1 if the dictionary is simple,
and 0 if it ain't.

Parameters
----------
D : dict
    dictionary, a contender for being simple

Returns
-------
is_simple : int
    1 for being simple dict, 0 for not being so

    """

    BAD_RETURN = 0

    if not(isinstance(D, dict)):
        st = au.simple_type(D)
        ab.EP1("Input to display_dict() must be dict, not: {}".format(st))
        return BAD_RETURN

    # verify assumed key and value types
    for key in D.keys():
        if not(isinstance(key, str)):
            st = au.simple_type(key)
            msg = "Each key here must be str, but '{}' is a {}".format(key, st)
            ab.EP1(msg)
            return BAD_RETURN
        val = D[key]
        if not(isinstance(val, list)):
            st = au.simple_type(val)
            msg = "Each value here must be list, "
            msg+= "but '{}' is a {}".format(val, st)
            ab.EP1(msg)
            return BAD_RETURN

    return 1

# ==========================================================================

if __name__ == "__main__" :


    print("\n----- Ex. 1: stats file -----\n")

    fname1 = '~/AFNI_data6/FT_analysis/FT.results/stats.FT+tlrc.'

    is_fail1, Cdict_diffs1 = \
        compare_nifti_from_brick_with_self_copy( fname1, clean_nifti=False )

    print("\n----- Ex. 2: anat -----\n")

    fname2 = '~/AFNI_data6/FT_analysis/FT.results/FT_anat+orig.HEAD'
    is_fail2, Adict2 = read_brick_attributes_3dA(fname2)

    is_fail2, Cdict_diffs2 = \
        compare_nifti_from_brick_with_self_copy( fname2, clean_nifti=False )

    print("\n----- Ex. 3: time series -----\n")

    fname3 = '~/AFNI_data6/FT_analysis/FT.results/pb00.FT.r01.tcat+orig'

    is_fail3, Cdict_diffs3 = \
        compare_nifti_from_brick_with_self_copy( fname3, clean_nifti=False )

    sys.exit(0)
