/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   

Usage: cat_matvec matvec_spec matvec_spec ...

Catenates 3D rotation+shift matrix+vector transformations.
Each matvec_spec is of the form

  mfile [-opkey]

Where 'mfile indicates from where to get the matrix+vector:

  * The name of an ASCII file with 12 numbers:
       u11 u12 u13 v1
       u21 u22 u23 v2
       u31 u32 u33 u3
    where each 'uij' and 'vi' is a number.  The 3x3
    matrix [uij] is the matrix of the transform, and
    the 3-vector [vi] is the shift.  The transform
    is [xnew] = [uij]*[xold] + [vi].

  * The name of an AFNI dataset that is a linear warp
    (+acpc) of another dataset (+orig).  The warp
    transformation is the transformation from +orig
    to +acpc coordinates.

The optional 'opkey' (operation key) following each mfile
starts with a '-', and then is a set of letters telling
how to treat the input:

  * I or i = invert the transformation
  * T or t = if mfile is a dataset, get the matrix+vector
             from the TAGALIGN_MATVEC attribute (computed
             by 3dTagalign) instead of the WARP_DATA
             attribute (created by AFNI markers alignment).

The transformation resulting by catenating the transformations
is written to stdout in the ASCII file format.  This can be
used as input to 3drotate -matvec_dicom (provided [uij] is a
proper orthogonal matrix).

EXAMPLE: Suppose you have two anatomical datasets on the same
subject (taken on different days) and wish to align their
associated functional datasets.  You transform each anat
dataset to +acpc coordinates.  You then mark each +acpc
dataset with a set of tags, and then use 3dTagalign to
compute the transformation that bests aligns the tagsets.
The transformational picture is

          T1
  a1+orig --> a1+acpc
    ^           ^
    |T4         |T3
    |           |
  a2+orig --> a2+acpc
          T2

Transformation T1 is stored as the WARP_DATA in a1+acpc.HEAD;
T2 is stored as the WARP_DATA in a2+acpc.HEAD.  T3 is from

  3dTagalign -master a1+acpc -prefix a2tagged a2+acpc

and is now stored in TAGALIGN_MATVEC in a2tagged+acpc.HEAD.
To compute the transformation that takes a2+orig to a1+orig
(labeled T4 in the diagram above), the command to use is

  cat_matvec a2+acpc a2tagged+acpc -T a1+acpc -I > T4.mfile

You can use T4.mfile as input to 3drotate to transform
functional data from the a2+orig session to be aligned with
data from the a1+orig session by doing

  3drotate -Fourier -clipit -matvec_dicom T4.mfile
           -prefix f2rot f1+orig

Please note that when marking and +acpc transforming
datasets with AFNI, they MUST be in separate directories.
