from cffi import FFI
import argparse
import numpy as np

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

class AfniDiffResult:
    def __init__(self, result):
        self._result = result
        if result.error != 0:
            raise RuntimeError(f"Error for AfniDiff: {hex(result.error)}")


    @property
    def counts(self):
        return [self._result.counts[i] for i in range(self._result.nt)]

    @property
    def ndiffs(self):
        return np.sum(self.counts)

    @property
    def is_different(self):
        return self.ndiffs != 0

    @property
    def is_error(self):
        return self._result.error != 0


def run_afni_diff(a, b, mask=None, tol=1e-8):
    a = bytes(a, "ascii")
    b = bytes(b, "ascii")
    if mask:
        m = bytes(mask, "ascii")
    else:
        m = ffi.NULL
    result = aflib.afni_diff(a, b, m, tol)

    return AfniDiffResult(result)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("A", help="The left image to compare")
    ap.add_argument("B", help="The right image to compare")
    ap.add_argument("--mask", help="Mask image")
    ap.add_argument("--tol", type=float, default=1e-8)
    args = ap.parse_args()
    diff = run_afni_diff(args.A, args.B, mask=args.mask, tol=args.tol)

    if diff.is_error:
        print("Error!")
    else:
        print(f"Element diff count: {diff.ndiffs}")


if __name__ == '__main__':
    main()
