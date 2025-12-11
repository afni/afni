#!/usr/bin/env python


# AFNI header: OBJ.header.info

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
