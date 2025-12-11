#!/usr/bin/env python

import sys
import numpy as np

# ====================================================================================

# AFNI header: OBJ.header.info

# ------------------------------------- NIFTI ----------------------------------------

# dict of NIFTI-1 header fields
dict_nifti1 = {
    'sizeof_hdr'      : 348,      # int
    'data_type'       : '',       # char [10]
    'db_name'         : '',       # char [18]
    'extents'         : 0,        # int
    'session_error'   : 0,        # short
    'regular'         : 'r',      # char
    'dim_info'        : None,     # char 
    'dim'             : None,     # short [8]
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
    'xyzt_units'      : None,     # char
    'cal_max'         : None,     # float
    'cal_min'         : None,     # float
    'slice_duration'  : None,     # float
    'toffset'         : None,     # float
    'glmax'           : 0,        # int
    'glmin'           : 0,        # int
    'descrip'         : None,     # char [80]
    'aux_file'        : None,     # char [24]
    'qform_code'      : None,     # short
    'sform_code'      : None,     # short
    'quatern_b'       : None,     # float
    'quatern_c'       : None,     # float
    'quatern_d'       : None,     # float
    'qoffset_x'       : None,     # float
    'qoffset_y'       : None,     # float
    'qoffset_z'       : None,     # float
    'srow_x'          : None,     # float [4]
    'srow_y'          : None,     # float [4]
    'srow_z'          : None,     # float [4]
    'intent_name'     : None,     # char [16]
    'magic'           : None,     # char [4]
}

# which keys in the nifti1 header dict are unused?
list_nifti1_unused = [
    'data_type',
    'db_name',
    'extents',
    'session_error',
    'regular',
    'glmax',
    'glmin', 
]


def calc_nifti_qsform_code( Adict, fname=None, verb=1 ):
    """Given the dictionary of AFNI header attributes Adict (and/or
provided file name fname), calculate what the corresponding qform_code
or sform_code value would be.

This first checks for an AFNI header attribute TEMPLATE_SPACE, but
then will fall back on the file name if necessary. At present, the
fname should always be given, for compatability with older dsets.

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
    name of AFNI dset (can include path)
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

    if not(isinstance(fname, str)) :
        print("** Error: must provide a fname of type str")
        return BAD_RETURN

    # check for this attribute first
    if 'TEMPLATE_SPACE' in Adict.keys() :
        tspace = Adict['TEMPLATE_SPACE']
        is_fail, qsform_code = translate_template_space_to_qform_code(tspace, 
                                                                      verb=verb)
        if is_fail :
            sys.exit(-1)

    elif fname is not None :
        is_fail1, av_space = get_dset_av_space(fname, verb=verb)
        is_fail2, qsform_code = translate_av_space_to_qform_code(av_space, 
                                                                 verb=verb)
        if is_fail1 or is_fail2 :
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

    if av_space == '+orig' or av_space == '+acpc' :
        return 0, 1

    if av_space == '+tlrc' :
        return 0, 3

    if av_space == '+mni' :
        return 0, 4

    # case of unknown av_space string, should never happen
    return -1, 0

# -----------------------------------------------------------------------------

# list of known av_space values (though only +orig and +tlrc are common)
LIST_allowed_av_space = [
    '+orig',
    '+tlrc',
    '+acpc',
    '+mni',
]

STR_allowed_av_space = ', '.join(LIST_allowed_av_space)

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

if __name__ == "__main__" :

    # example use cases
    print("++ None yet")
