"""Minimal NIfTI-1 writer (no nibabel dependency).

Writes an uncompressed .nii with a simple diagonal affine.  Data are stored
as float32 (or int16 for integer masks) with NIfTI's Fortran (x fastest)
ordering: the array passed in must be indexed [x, y, z] or [x, y, z, t].
"""
import struct
import numpy as np


def write_nifti(fname, arr, voxsize=(2.0, 2.0, 2.0), tr=1.0):
    arr = np.asarray(arr)
    if arr.ndim == 3:
        arr = arr[..., None]
    nx, ny, nz, nt = arr.shape
    if np.issubdtype(arr.dtype, np.integer):
        datatype, bitpix, dt = 4, 16, np.int16        # NIFTI_TYPE_INT16
    else:
        datatype, bitpix, dt = 16, 32, np.float32     # NIFTI_TYPE_FLOAT32
    ndim = 4 if nt > 1 else 3
    dim = [ndim, nx, ny, nz, nt, 1, 1, 1]
    pixdim = [1.0, voxsize[0], voxsize[1], voxsize[2], tr, 0, 0, 0]
    hdr = bytearray(348)
    struct.pack_into('<i', hdr, 0, 348)
    struct.pack_into('<8h', hdr, 40, *dim)
    struct.pack_into('<h', hdr, 70, datatype)
    struct.pack_into('<h', hdr, 72, bitpix)
    struct.pack_into('<8f', hdr, 76, *pixdim)
    struct.pack_into('<f', hdr, 108, 352.0)        # vox_offset
    struct.pack_into('<f', hdr, 112, 1.0)          # scl_slope
    struct.pack_into('<f', hdr, 116, 0.0)          # scl_inter
    struct.pack_into('<h', hdr, 252, 1)            # qform_code = SCANNER_ANAT
    struct.pack_into('<h', hdr, 254, 1)            # sform_code
    struct.pack_into('<6f', hdr, 256, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    struct.pack_into('<4f', hdr, 280, voxsize[0], 0, 0, 0)
    struct.pack_into('<4f', hdr, 296, 0, voxsize[1], 0, 0)
    struct.pack_into('<4f', hdr, 312, 0, 0, voxsize[2], 0)
    hdr[344:348] = b'n+1\0'
    data = np.ascontiguousarray(arr.transpose(3, 2, 1, 0)).astype(dt)  # on disk x is fastest
    with open(fname, 'wb') as f:
        f.write(bytes(hdr))
        f.write(b'\0\0\0\0')
        f.write(data.tobytes())
    return fname
