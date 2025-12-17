#!/usr/bin/env python

import sys
import numpy as np

# ============================================================================
# 
# A library of functions and data objects for mapping a dictionary of
# AFNI header file attributes to NIFTI-1 standard (Cox et al., 2004)
# attributes. These functions do _not_make use of AFNI programs but
# instead perform all mapping from just the HEAD file contents and
# file name itself.  While it would be simpler and more convenient to
# use AFNI programs (e.g., via shell commands), these functions could
# be used on systems without AFNI installed.
#
# For reference, the NIFTI header fields are listed, defined and
# described here:
# https://github.com/NIFTI-Imaging/nifti_clib/blob/master/nifti2/nifti1.h
# 
# Additionally, attributes of the AFNI header are listed, defined and
# described here:
# https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.attributes.html
#
# This webpage also includes useful information about some NIFTI and
# AFNI header features:
# https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/nifti/main_toc.html
#
# ============================================================================
# nifti1 header: dictionaries

# this section includes subsets of values with relevant properties to
# be aware of (officially unused keys, ones not mapped in practice
# from the AFNI header, etc.)

# dict of NIFTI-1 header fields
dict_nifti1 = {
    'sizeof_hdr'      : 348,      ## int
    'data_type'       : None,     ## char [10]
    'db_name'         : None,     ## char [18]
    'extents'         : None,     ## int
    'session_error'   : None,     ## short
    'regular'         : None,     ## char
    'dim_info'        : None,     # char 
    'dim'             : None,     ## short [8]
    'intent_p1'       : None,     # float
    'intent_p2'       : None,     # float
    'intent_p3'       : None,     # float
    'intent_code'     : None,     # short
    'datatype'        : None,     # short
    'bitpix'          : None,     # short
    'slice_start'     : None,     # short
    'pixdim'          : None,     # float [8]
    'vox_offset'      : None,     # float
    'scl_slope'       : None,     # float
    'scl_inter'       : None,     # float
    'slice_end'       : None,     # short
    'slice_code'      : None,     # char
    'xyzt_units'      : None,     ## char
    'cal_max'         : None,     ## float
    'cal_min'         : None,     ## float
    'slice_duration'  : None,     # float
    'toffset'         : None,     ## float
    'glmax'           : None,     ## int
    'glmin'           : None,     ## int
    'descrip'         : None,     ## char [80]
    'aux_file'        : None,     ## char [24]
    'qform_code'      : None,     ## short
    'sform_code'      : None,     ## short
    'quatern_b'       : None,     ## float
    'quatern_c'       : None,     ## float
    'quatern_d'       : None,     ## float
    'qoffset_x'       : None,     ## float
    'qoffset_y'       : None,     ## float
    'qoffset_z'       : None,     ## float
    'srow_x'          : None,     ## float [4]
    'srow_y'          : None,     ## float [4]
    'srow_z'          : None,     ## float [4]
    'intent_name'     : None,     # char [16]
    'magic'           : None,     # char [4]
}

# which keys in the nifti1 header dict are unused?
dict_nifti1_unused = {
    'data_type'       : '',       ## char [10]
    'db_name'         : '',       ## char [18]
    'extents'         : 0,        ## int
    'session_error'   : 0,        ## short
    'regular'         : 'r',      ## char
    'glmax'           : 0,        ## int
    'glmin'           : 0,        ## int
}

# which keys in the nifti1 header dict are unmapped from AFNI header?
# *** candidates (may change...)
dict_nifti1_unmapped = {
    'cal_max'         : 0.0,      ## float
    'cal_min'         : 0.0,      ## float
    'descrip'         : '',       ## char [80]
    'aux_file'        : '',       ## char [24]
}

# which keys in the nifti1 header dict should come from the data
# array, when applying/copying the header to a new set?
list_nifti1_recalc_from_data = [
    'datatype',                   # short
    'bitpix',                     # short
]

# ============================================================================
# AFNI header: useful dictionaries, lists and strings

# list of known av_space values (though only +orig and +tlrc are common)
LIST_allowed_av_space = [
    '+orig',
    '+tlrc',
    '+acpc',                      # mostly unused nowadays
    '+mni',                       # mostly unused nowadays
]

# string version of the av_space list
STR_allowed_av_space = ', '.join(LIST_allowed_av_space)

# ============================================================================
# calculate nifti fields: 
# + toffset : float

def calc_nifti_toffset( Adict, verb=1 ):
    """Given the dictionary of AFNI header attributes Adict calculate what
the corresponding time offset would be, that is, a nonzero start point
for the time axis.

For datasets without a time axis, it will be 0.0.  For datasets
created with to3d, it will likely also be 0.0.

This checks for these AFNI header attributes:
+ TAXIS_FLOATS (might not exist, if dset does not have time, like if 3D
  or just a 'bucket')
  [0] Time origin (in units given by TAXIS_NUMS[2]).

Parameters
----------
Adict : dict
    dictionary of AFNI header attributes
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
toffset : float
    value of temporal offset

    """

    BAD_RETURN = (-1, 0.0)

    # initialize default
    toffset = 0.0

    # check for time floats, and parse if it exists
    key = 'TAXIS_FLOATS'
    if key in Adict.keys() :
        tfloats = Adict[key]
        is_fail, arr_tfloats = extract_first_n_int(tfloats, wall_value=-999999,
                                                   min_len=5, max_len=5,
                                                   verb=verb)
        if is_fail :
            print("** Error: failed to extract array for key " + key)
            sys.exit(-1)

        # simply get TAXIS_FLOATS[0] value,
        toffset = arr_tfloats[0]
    # else: there isn't a time axis, which is OK.

    return 0, toffset

# ============================================================================
# calculate nifti fields: 
# + xyzt_units : char

def calc_nifti_xyzt_units( Adict, verb=1 ):
    """Given the dictionary of AFNI header attributes Adict calculate what
the corresponding physical units would be, as recorded with a
numerical code in xyzt_units. It should basically always be: mm (for
length); and s (for time), if the time axis is used.  For AFNI
'bucket' datasets, time might not technically be used.

Specifically, xyzt_units encodes the units of pixdim[1..4]. So, if the
'time' axis isn't used (which might be the case on the AFNI side for
'bucket' datasets), then only the spatial units are stored.

The 'char' xyzt_units has the ability to store distance and/or
time units using the magic of binary numbers. From the nifti1.h file:
   Bits 0..2 of xyzt_units specify the units of pixdim[1..3]
    (e.g., spatial units are values 1..7).
   Bits 3..5 of xyzt_units specify the units of pixdim[4]
    (e.g., temporal units are multiples of 8).

This checks for these AFNI header attributes:
+ TAXIS_NUMS (might not exist, if dset does not have time, like if 3D
  or just a 'bucket')
  [2] code for units of time (nearly always seconds in AFNI)

At present, we assume that the following is implicit when translating
the AFNI header:
+ spatial units always exist, and that they are always 'mm'.

Parameters
----------
Adict : dict
    dictionary of AFNI header attributes
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
xyzt_units : int
    code for spatial (and maybe temporal) units

    """

    BAD_RETURN = (-1, 0)

    # initialize default; 0="Unknown"
    xyzt_units = 0

    # we just *assume* this is always the case with a BRIK/HEAD dset
    unit_space = 2

    # initialize time unit value; BRIK/HEAD dset might not have a time axis
    unit_time = 0 

    # check for time axis, and parse if it exists
    key = 'TAXIS_NUMS'
    if key in Adict.keys() :
        tnums = Adict[key]
        is_fail, arr_tnums = extract_first_n_int(tnums, wall_value=-999,
                                                 min_len=3, max_len=3,
                                                 verb=verb)
        if is_fail :
            print("** Error: failed to extract array for key " + key)
            sys.exit(-1)

        # decode the short list of possible TAXIS_NUMS[2] values;
        # (doesn't seem worth making a separate function, but we could)
        if arr_tnums[2] == 77001 :  # units: msec
            unit_time = 16
        elif arr_tnums[2] == 77002 :  # units: sec
            unit_time = 8
        elif arr_tnums[2] == 77003 :  # units: Hz
            unit_time = 32
        else:
            print("** ERROR: unknown time code in TAXIS_NUMS[2]:", arr_tnums)
            return BAD_RETURN
    # else: there isn't a time axis, which is OK.

    # combine space and time units codes
    xyzt_units = unit_space + unit_time

    return 0, xyzt_units


# ============================================================================
# calculate nifti fields: 
# + dim - short [8]

def calc_nifti_dim( Adict, verb=1 ):
    """Given the dictionary of AFNI header attributes Adict, calculate
what the corresponding array of dim values would be.

This checks for several AFNI header attributes:
+ DATASET_RANK : describes dimensionality of the data; may be up to 8
  values, but probably fewer nonzero in practice 
    [0] number of spatial dimensions (must be 3)
    [1] number of volumes (time points, nvals or nv)
+ DATASET_DIMENSIONS : 3 values of the spatial axes
    [0] = number of voxels along the x-axis (nx)
    [1] = number of voxels along the y-axis (ny)
    [2] = number of voxels along the z-axis (nz)
    The voxel with 3-index (i,j,k) in a sub-brick
    is located at position (i+j*nx+k*nx*ny), for
    i=0..nx-1, j=0..ny-1, k=0..nz-1.  Each axis must
    have at least 2 points!
+ TAXIS_NUMS : (which might not exist, like for bucket dsets) contains
               info about whether a time axis is present

For the present calc, it appears we could get the info of 'is there a
time axis?' from either TAXIS_NUMS (whether it exists or not) or
SCENE_DATA (is the dset a bucket or not).  We will start by using
TAXIS_NUMS.

On the NIFTI side, 
+ dim[0] : states how many dimensions are present; if dim[0] is not in [1,7],
  the byte swapping is needed 
  [Q] ***not sure what to do in case of values outside this range***
+ dim[1], dim[2], dim[3] : spatial dimensions
+ dim[4] : time dimension
+ dim[5], dim[6], dim[7] : other, as needed
... and it appears that empty/unused dim values are set to 1.
+ NB: a 4D time series might have 4 dims, while a bucket dataset (like of stats)
  might have 5

Parameters
----------
Adict : dict
    dictionary of AFNI header attributes
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
dim : array of 8 int
    dimensionality info

    """

    BAD_RETURN = (-1, np.zeros(8, dtype=int))

    # def array; empty values get set to 1, so initialize like this
    dim = np.ones(8, dtype=int)

    # NB: 'translating' many of these AFNI header attributes means
    # taking an array that has excess values on the end, and removing
    # those excess values; typically those values are 0 or -999.

    # get number of spatial and temporal dims
    key = 'DATASET_RANK'
    if key in Adict.keys() :
        drank = Adict[key]
        is_fail, arr_rank = extract_first_n_int(drank, wall_value=0,
                                                min_len=2, max_len=2,
                                                verb=verb)
        if is_fail :
            print("** Error: failed to extract array for key " + key)
            sys.exit(-1)

        # extra check for this array: zeroth element must have value 3
        if arr_rank[0] != 3 :
            print("** Error: [0]th element of dataset_rank != 3:", drank)
            return BAD_RETURN
    else:
        print("** ERROR: need to parse " + key)
        return BAD_RETURN

    # get spatial matrix sizes
    key = 'DATASET_DIMENSIONS'
    if key in Adict.keys() :
        ddims = Adict[key]
        is_fail, arr_dims = extract_first_n_int(ddims, wall_value=0,
                                                min_len=3, max_len=3,
                                                verb=verb)
        if is_fail :
            print("** Error: failed to extract array for key " + key)
            sys.exit(-1)

    else:
        print("** ERROR: need to parse " + key)
        return BAD_RETURN

    # check about whether TAXIS_NUMS exists in AFNI header. This
    # determines whether the nifti is 4D (3 space, 1 time) or 5D (3
    # space, 1 placeholder, 1 "number of volumes").
    key = 'TAXIS_NUMS'
    if key in Adict.keys() :
        has_time_axis = True
    else:
        has_time_axis = False

    # merge mini-dim arrays here
    is_fail, dim = translate_afni_arrays_to_dim(arr_rank, arr_dims, 
                                                has_time_axis, verb=1)
    if is_fail :
        print("** Error: failed to convert mini-dim arrays to dim")
        sys.exit(-1)

    return 0, dim


def translate_afni_arrays_to_dim(arr_rank, arr_dims, has_time_axis, verb=1):
    """For a given set of mini-arrays, originating with the DATASET_RANK,
DATASET_DIMENSIONS and TAXIS_NUMS arrays in the AFNI header, figure
out the NIFTI header's dim array.  Each attribute array here has been
"precleaned" of excess/unused values it might have had in the AFNI header.

**Order very much matters for inputting the correct arrays here.**

Parameters
----------
arr_rank : array of int
    array of int values taken from the AFNI header's DATASET_RANK attribute
arr_dims : array of int
    array of int values taken from the AFNI header's DATASET_DIMENSIONS 
    attribute
has_time_axis : bool
    is the dset 4D (has_time_axis=True) or 5D (has_time_axis=False)
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
dim : 
    the NIFTI header dim array itself

    """

    BAD_RETURN = (-1, 0)

    # initialize dim arr, which will be our main output: we know it
    # has 8 ints, and the unused elements have value = 1
    dim = np.ones(8, dtype=int)

    # 3 spatial matrix dim values
    dim[1] = arr_dims[0]
    dim[2] = arr_dims[1]
    dim[3] = arr_dims[2]

    # number of volumes 
    nvals = arr_rank[1]

    # whether nvals goes into dim[4] or dim[5] is determined by the
    # following considerations (might grow over time); additionally,
    # dim[0] stores the total number of dimensions in the data, which
    # is determined by this choice, too, so we set it at the same time.

    if has_time_axis :
        # dim[4] remains 1, as initialized above
        dim[5] = nvals
        dim[0] = 5
    else:
        dim[4] = nvals
        dim[0] = 4

    # we assume everything else is an unknown template, hence this code
    return 0, dim


# ============================================================================
# calculate nifti fields: 
# + qform_code - short
# + sform_code - short

# throughout the code, these are collectively referred to as: qsform_code

def calc_nifti_qsform_code( Adict, fname=None, verb=1 ):
    """Given the dictionary of AFNI header attributes Adict (and/or
provided file name fname), calculate what the corresponding qform_code
or sform_code value would be.

This function first checks for an AFNI header attribute
TEMPLATE_SPACE, which might not be present in all dsets, particularly
older ones, but it would be the most informative attribute to map.
Then, it checks for SCENE_DATA, which should be present in all
dsets. As a last gasp, it can optionally fall back on the file name if
necessary. Note that fname is no longer required, because the
SCENE_DATA attribute should be a reliable second pass, even for much
older dsets.

See here for more info
https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/nifti/qsform_code.html

Note: in code and functions, we abbreviate the phrase 'qform_code
and/or sform_code' as 'qsform_code', for brevity.  When only one or
the other applies, we would refer specifically to each separately.

Parameters
----------
Adict : dict
    dictionary of AFNI header attributes
fname : str
    name of AFNI dset (can include path), optional as a fallback
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
qsform_code : int
    an integer value for allowed

    """

    BAD_RETURN = (-1, 0)

    # check for this attribute first; it might not be present in all
    # datasets, esp. older ones, but it would be the most informative
    if 'TEMPLATE_SPACE' in Adict.keys() :
        key = 'TEMPLATE_SPACE'
        tspace = Adict[key]
        is_fail, qsform_code = translate_template_space_to_qform_code(tspace, 
                                                                      verb=verb)
        if is_fail :
            print("** Error: failed to extract data for key " + key)
            sys.exit(-1)

    # this attribute should be present in all dsets, has a short list
    # of mappings to qsform_code values
    elif 'SCENE_DATA' in Adict.keys() :
        key = 'SCENE_DATA'
        sdata = Adict[key]
        is_fail, arr_sdata = extract_first_n_int(sdata, wall_value=-999,
                                                 min_len=3, max_len=3,
                                                 verb=verb)
        if is_fail :
            print("** Error: failed to extract data for key " + key)
            sys.exit(-1)

        is_fail, qsform_code = translate_scene_data_to_qform_code(arr_sdata, 
                                                                  verb=verb)
        if is_fail :
            print("** Error: failed to convert data for key " + key)
            sys.exit(-1)

    # as a last resort, can try to parse the filename itself, for the
    # '+orig', '+tlrc', etc. part of prefix
    elif fname is not None :
        is_fail1, av_space = get_dset_av_space(fname, verb=verb)
        is_fail2, qsform_code = translate_av_space_to_qform_code(av_space, 
                                                                 verb=verb)
        if is_fail1 or is_fail2 :
            print("** Error: failed to information for fname " + fname)
            sys.exit(-2)

    else:
        # no information to judge this attribute, which is bad
        return BAD_RETURN

    return 0, qsform_code


def translate_template_space_to_qform_code(tspace, verb=1):
    """For a given tspace ('TEMPLATE_SPACE' value from AFNI header), state
what best guess of qsform_code value is.

Most AFNI BRIK/HEAD dsets should have this attribute nowadays. But
older dsets might not.  Additionally, there are many variations of
TEMPLATE_SPACE names for a given specific space (like MNI, MNI_anat,
MNI2009c, etc.), and so we do our best to guess what is an appropriate
qsform_code value, but this function might need updates over time.

Parameters
----------
tspace : str
    value of AFNI header's TEMPLATE_SPACE attribute, like TLRC, ORIG, 
    IBT_C1, etc.
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
qsform_code : 
    the nifti code itself

    """

    BAD_RETURN = (-1, 0)

    if not(isinstance(tspace, str)) :
        print("** Error: must provide a tspace of type str")
        return BAD_RETURN

    if tspace == 'ORIG' :
        return 0, 1

    if tspace == 'TLRC' or tspace.startswith('TLRC') or \
       tspace.startswith('TT') :
        return 0, 3

    if tspace == 'MNI' or tspace.startswith('MNI') :
        return 0, 4

    # we don't need to put these here per se, but if ever change what
    # the default output (for unrecognized values) is, this might be
    # useful to have
    if tspace == 'IBT' or tspace.startswith('HaskinsPeds') or \
       tspace.startswith('NMT') or tspace.startswith('MBM') :
        return 0, 5

    # we assume everything else is an unknown template, hence this code
    return 0, 5

def translate_scene_data_to_qform_code(arr_sdata, verb=1):
    """For a given array arr_sdata (from SCENE_DATA attribute), state what
best guess of qsform_code value is.

The arr_sdata[0] = SCENE_DATA[0] code is used here. This integer
basically encodes view type (=av_space) as follows: 
    0=+orig
    1=+acpc
    2=+tlrc

Note that nowadays, essentially all non-original space dsets have
av_space=='+tlrc', so from just the name alone, we can't know whether
the dset is really TLRC specifically, or MNI or HaskinsPeds or
something else. So, there is inherent degeneracy of this term; for
more detailed information, one would want to be using the AFNI
header's TEMPLATE_SPACE attribute, which should be present in modern
dsets (though older ones might not have it).  

So, this function returns a best guess, given limited info.

Parameters
----------
arr_sdata : array of int values
    ints, whose [0]th element maps onto the AFNI view space string, 
    as noted above
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
qsform_code : 
    the nifti code itself

    """

    BAD_RETURN = (-1, 0)

    # make input is an array/list/tuple
    try:
        N = len(arr_sdata)
    except:
        print("** Error: must provide a arr_sdata of type array")
        return BAD_RETURN

    int_view = arr_sdata[0]
    if int(int_view) != int_view :
        print("** Error: must provide an array arr_sdata of int values")
        return BAD_RETURN

    # go through list of recognized codes

    if int_view == 0 or int_view == 1 : 
        # +orig or +acpc
        return 0, 1

    elif int_view == 2 : 
        # +tlrc
        return 0, 3

    # case of unencoded int_view, should never happen
    print("** Error: unencoded int at start of array:", int_view)
    return BAD_RETURN


def translate_av_space_to_qform_code(av_space, verb=1):
    """For a given av_space (from dset name), state what best guess of
qsform_code value is.

Note that nowadays, essentially all non-original space dsets have
av_space=='+tlrc', so from just the name alone, we can't know whether
the dset is really TLRC specifically, or MNI or HaskinsPeds or
something else. So, there is inherent degeneracy of this term; for
more detailed information, one would want to be using the AFNI
header's TEMPLATE_SPACE attribute, which should be present in modern
dsets (though older ones might not have it).  

So, this function returns a best guess, given limited info.

Parameters
----------
av_space : str
    AFNI view space string, like +orig, +tlrc, etc.
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
qsform_code : 
    the nifti code itself

    """

    BAD_RETURN = (-1, 0)

    if not(isinstance(av_space, str)) :
        print("** Error: must provide an av_space of type str")
        return BAD_RETURN

    # go through list of recognized strings

    if av_space == '+orig' or av_space == '+acpc' :
        return 0, 1

    elif av_space == '+tlrc' :
        return 0, 3

    elif av_space == '+mni' :
        return 0, 4

    # case of unknown av_space string, should never happen
    print("** Error: unknown av_space string:", av_space)
    return -1, 0

# -----------------------------------------------------------------------------

def get_dset_av_space(fname, verb=1):
    """For a BRIK/HEAD dset with filename fname (which can include path),
get the AFNI view space name, like +orig or +tlrc.

The fname string can be full name of *.BRIK* or *.HEAD file, or one of the
shortened versions that AFNI recognizes, as long as the av_space is there.

Parameters
----------
fname : str
    name of AFNI dset (can include path)
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
av_space : 
    the av_space itself

    """

    BAD_RETURN = (-1, '')

    if not(isinstance(fname, str)) :
        print("** Error: fname must be of type str")
        return BAD_RETURN

    N  = len(fname)

    # search backwards for first '+'
    ii = 1
    while ii <= N :
        if fname[-ii] == '+' :
            break
        ii+=1

    if ii > N :
        print("** Error: Could not fine '+' for av_space.")
        return BAD_RETURN
    
    # search forwards for end of av_space: first '.', or end of fname
    jj = N-ii
    av_space = ''
    while jj < N :
        if fname[jj] == '.' :
            break
        jj+=1
        av_space = fname[N-ii:jj]

    # check av_space for allowed value
    if av_space not in LIST_allowed_av_space :
        msg = "** Error: av_space '{}' is not in ".format(av_space)
        msg+= "list of known values:\n   {}".format(STR_allowed_av_space)
        print(msg)
        return BAD_RETURN

    return 0, av_space

# ============================================================================
# calculate nifti fields: 
# + srow_x - float [4]
# + srow_y - float [4]
# + srow_z - float [4]

def calc_nifti_srow_xyz( Adict, verb=1 ):
    """Given the dictionary of AFNI header attributes Adict, calculate
what the corresponding srow_{x,y,z} values.

This first checks for an AFNI header attribute IJK_TO_DICOM_REAL, but
then will fall back on IJK_TO_DICOM if necessary.

Parameters
----------
Adict : dict
    dictionary of AFNI header attributes
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
srow_x : 
    the 1x4 array floating point srow_x values
srow_y : 
    the 1x4 array floating point srow_y values
srow_z : 
    the 1x4 array floating point srow_z values
    """

    zarr = np.zeros(4, dtype=float)
    BAD_RETURN = (-1, zarr, zarr, zarr)

    # check for this attribute first
    if 'IJK_TO_DICOM_REAL' in Adict.keys() :
        aff12 = Adict['IJK_TO_DICOM_REAL']
    elif 'IJK_TO_DICOM' in Adict.keys() :
        aff12 = Adict['IJK_TO_DICOM']
    else:
        # no information to judge this attribute, which is bad
        return BAD_RETURN

    is_fail, srow_x, srow_y, srow_z = \
        translate_aff12_to_srow_xyz(aff12, verb=verb)

    if is_fail :
        sys.exit(-1)

    return 0, srow_x, srow_y, srow_z 


def translate_aff12_to_srow_xyz(aff12, verb=1):
    """For a given aff12 ('IJK_TO_DICOM_REAL' or 'IJK_TO_DICOM' value from
AFNI header), state what the srow_{x,y,z} values should be. 

As a first step, these 12 float values in aff12 are unpacked into a
'mat44' form, which, despite its name, is a 3x4 affine matrix: a 3x3
scale+rotation matrix, plus a 3x1 shift or offset value.

This function mainly just converts the sign conventions for
orientation systems.  In AFNI, the orientation string specifies which
axis sides have *negative* signs.  So, this function converts from
AFNI's default RAI-DICOM (i.e., right/anterior/inferior all have
negative signs) to nibabel's LPI system (i.e., left/posterior/inferior
all have negative signs.  Note that nibabel refers to the latter
convention as RAS+, that is the right/anterior/superior all have
positive signs.

Most AFNI BRIK/HEAD dsets should have both IJK_TO_DICOM and
IJK_TO_DICOM_REAL nowadays. But older dsets might only have the
former.

Parameters
----------
aff12: array of 12 floats
    value of one of AFNI header's IJK_TO_DICOM* attribute
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
srow_x : 
    the 1x4 array floating point srow_x values
srow_y : 
    the 1x4 array floating point srow_y values
srow_z : 
    the 1x4 array floating point srow_z values

    """

    zarr = np.zeros(4, dtype=float)
    BAD_RETURN = (-1, zarr, zarr, zarr)

    if len(aff12) != 12 :
        print("** Error: aff12 must have 12 values, not:", len(aff12))
        return BAD_RETURN

    # convert DICOM-RAI to LPS (and nibabel calls RAS+)
    srow_x = -1.0 * np.array(aff12[0:4], dtype=float)
    srow_y = -1.0 * np.array(aff12[4:8], dtype=float)
    srow_z =  1.0 * np.array(aff12[8:12], dtype=float)

    return 0, srow_x, srow_y, srow_z

# ============================================================================
# calculate nifti fields: 
# + quatern_b - float
# + quatern_c - float
# + quatern_d - float
# + qoffset_x - float
# + qoffset_y - float
# + qoffset_z - float

# here, just translate the srow_x, srow_y and srow_z info directly;
# basically do what the nifti_dmat44_to_quatern(...) function in in
# nifti/nifti2/nifti2_io.c does

def calc_nifti_quatern_and_qoffset( srow_x, srow_y, srow_z, verb=1 ):
    """Given the NIFTI header srow_{x,y,z} arrays that have already been
calculated from AFNI header, calculate the corresponding quaternion
values (quatern_b, quatern_c, and quatern_d) and offsets (qoffset_x,
qoffset_y and qoffset_z).

The calculations here follow NIFTI library C code for this procedure,
just mapped over to Python.

*** NB: at present we assume the input matrix from which srow_* were
    generated, and therefore the srow_* values themselves, was/were
    properly orthogonal.  We can add in other functionality to
    internally orthogonalize, like the AFNI function does, at some
    point.  That has *not* been done at present.

Parameters
----------
srow_x : 
    the 1x4 array floating point srow_x values of the NIFTI header
srow_y : 
    the 1x4 array floating point srow_y values of the NIFTI header
srow_z : 
    the 1x4 array floating point srow_z values of the NIFTI header
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
quatern_b : 
    quaternion element
quatern_c : 
    quaternion element
quatern_d : 
    quaternion element
qoffset_x : 
    quaternion offset
qoffset_y : 
    quaternion offset
qoffset_z : 
    quaternion offset

    """

    BAD_RETURN = (-1, 0.0, 0.0, 0.0)

    # verify lengths
    srow_all = [srow_x, srow_y, srow_z]
    name_all = ['srow_x', 'srow_y', 'srow_z']
    for ii in range(len(srow_all)) :
        nrow = len(srow_all[ii])
        if nrow != 4 :
            msg = "** Error: srow lengths must be 4, but "
            msg+= " {} has len = {}".format(name_all[ii], nrow)
            print(msg)

    # ----- calc quaternion offsets

    # these are straightforward to read directly from matrix
    qoffset_x = srow_x[3]
    qoffset_y = srow_y[3]
    qoffset_z = srow_z[3]

    # ----- do background calcs for quaternions

    # define local variables
    r11 = srow_x[0] ; r12 = srow_x[1] ; r13 = srow_x[2] 
    r21 = srow_y[0] ; r22 = srow_y[1] ; r23 = srow_y[2] 
    r31 = srow_z[0] ; r32 = srow_z[1] ; r33 = srow_z[2] 

    # compute lengths of each column; these determine grid spacings
    xd = np.sqrt( r11*r11 + r21*r21 + r31*r31 ) 
    yd = np.sqrt( r12*r12 + r22*r22 + r32*r32 ) 
    zd = np.sqrt( r13*r13 + r23*r23 + r33*r33 ) 

    # if a column length is zero, patch the trouble
    if xd == 0.0 :
        r11 = 1.0 ; r21 = 0.0; r31 = 0.0 ; xd = 1.0 
    if yd == 0.0 :
        r22 = 1.0 ; r12 = 0.0; r32 = 0.0 ; yd = 1.0
    if zd == 0.0 :
        r33 = 1.0 ; r13 = 0.0; r23 = 0.0 ; zd = 1.0

    # assign the output lengths
    dx = xd ; dy = yd ; dz = zd
    
    # normalize the columns
    r11 /= xd ; r21 /= xd ; r31 /= xd ;
    r12 /= yd ; r22 /= yd ; r32 /= yd ;
    r13 /= zd ; r23 /= zd ; r33 /= zd ;

    # *** for now, skip the orthogonalization step, assume original
    # *** input matrix was properly affine

    # compute the determinant to determine if it is proper:
    # zd should be -1 or 1
    # [Q]: should this be 'zd' again, which was defined above?
    zd = r11*r22*r33 - r11*r32*r23 - r21*r12*r33 + \
         r21*r32*r13 + r31*r12*r23 - r31*r22*r13 
    if zd > 0 :                          # proper
        qfac = 1.0
    else :                               # improper ==> flip 3rd column
        qfac = -1.0
        r13  = -r13 ; r23 = -r23 ; r33 = -r33

    # ----- now, compute quaternion parameters

    a = r11 + r22 + r33 + 1.0

    if a > 0.5 :                         # simplest case
        a = 0.50 * np.sqrt(a) 
        b = 0.25 * (r32-r23) / a 
        c = 0.25 * (r13-r31) / a 
        d = 0.25 * (r21-r12) / a 
    else:                                # trickier case 
        xd = 1.0 + r11 - (r22+r33)       #  4*b*b 
        yd = 1.0 + r22 - (r11+r33)       #  4*c*c
        zd = 1.0 + r33 - (r11+r22)       #  4*d*d 
        if xd > 1.0 :
            b = 0.50 * np.sqrt(xd) 
            c = 0.25 * (r12+r21) / b 
            d = 0.25 * (r13+r31) / b 
            a = 0.25 * (r32-r23) / b 
        elif yd > 1.0 :
            c = 0.50 * np.sqrt(yd)
            b = 0.25 * (r12+r21) / c
            d = 0.25 * (r23+r32) / c
            a = 0.25 * (r13-r31) / c
        else:
            d = 0.50 * np.sqrt(zd)
            b = 0.25 * (r13+r31) / d
            c = 0.25 * (r23+r32) / d
            a = 0.25 * (r21-r12) / d
        # to be mathematically consistent, this would include a = -a
        if a < 0.0 :
            b = -b ; c = -c ; d = -d

    # finalize
    quatern_b = b
    quatern_c = c
    quatern_d = d

    return 0, quatern_b, quatern_c, quatern_d, \
        qoffset_x, qoffset_y, qoffset_z


# ============================================================================
# generic helper functions

def extract_first_n_int(A, wall_value=0, min_len=0, max_len=None, verb=1):
    """Helper function. For an ordered collection (array, list or tuple)
of ints A, get the first N values prior to hitting the wall_value, and
return that object as an array.  That is, go through A until the first
wall_value (or end) is reached, and return those values in an array.

If the user has an expected minimum length of output array (which is a
common expectation here), that can be specified with
min_len. Similarly, a max_len can be specified. Used together these
can be used to determine an exact value (i.e., if min_len=max_len).

Parameters
----------
A : array, list or tuple of int values
    some ordered collection of ints
min_len : int
    the minimum number of elements the output arr B can have, below which
    it is considered a bad result
max_len : None or int
    the maximum number of elements the output arr B can have, above which
    it is considered a bad result
verb : int
    verbosity level for messages whilst working

Returns
-------
is_fail : int
    0 on success, nonzero on failure
B : array of ints
    array of int values extracted from A, as described above

    """

    BAD_RETURN = (-1, np.zeros(1, dtype=int))

    try:
        N = len(A)
    except:
        print("** Error: must provide an array, list or tuple")
        return BAD_RETURN

    # make a list of all values until hitting the wall_value
    L  = []
    ii = 0
    while ii < N :
        if A[ii] != wall_value:  L.append(A[ii])
        else:                    break
        ii+= 1

    M = len(L)

    # check about length restrictions/expectations
    if M < min_len :
        print("** Error: too few array values: "
              "{} < {}".format(M, min_len))
        return BAD_RETURN
    if max_len is not None and M > max_len :
        print("** Error: too many array values: "
              "{} > {}".format(M, max_len))
        return BAD_RETURN

    return 0, np.array(L)


# ============================================================================

if __name__ == "__main__" :

    # example use cases
    print("++ None yet")
