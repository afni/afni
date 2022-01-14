from cffi import FFI

ffi = FFI()
tdef = r"""
typedef struct {
int * counts;       /* total number of differing voxels per volume */
int nt;             /* total number of volumes compared */
int nv;             /* total number of voxels per volume */
int msize;          /* how many voxels were in the mask */
int error;          /* whether an error occurred */
} AfniDiffResult;
"""
ffi.cdef(tdef)
fdef = r"""
AfniDiffResult afni_diff(
char * a,     /* dset a filename */
char * b,     /* dset b filename */
char * m,     /* mask dset filename */
float tol           /* max absolute difference to be "equal" */
);
"""
ffi.cdef(fdef)
aflib = ffi.dlopen("libafni_diff.so")

def run_afni_diff(a, b, mask=None, tol=1e-8):
    a = bytes(a, "ascii")
    b = bytes(b, "ascii")
    if mask:
        m = bytes(mask, "ascii")
    else:
        m = ffi.NULL
    result = aflib.afni_diff(a, b, m, tol)

    if result.error:
        raise RuntimeError("Diff comparison failed!")

    else:
        print(f"Compared {result.nv} volumes over {result.nt} tpoints")
