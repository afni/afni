.. contents:: 
    :depth: 4 

**********
cat_matvec
**********

.. code-block:: none

    Usage: cat_matvec [-MATRIX | -ONELINE] matvec_spec matvec_spec ...
    
    Catenates 3D rotation+shift matrix+vector transformations.
    Each matvec_spec is of the form
    
      mfile [-opkey]
    
    'mfile' specifies the matrix, and can take 4(ish) forms:
    
    === FORM 1 ===
    mfile is the name of an ASCII file with 12 numbers arranged
    in 3 lines:
          u11 u12 u13 v1
          u21 u22 u23 v2
          u31 u32 u33 v3
    where each 'uij' and 'vi' is a number.  The 3x3 matrix [uij]
    is the matrix of the transform, and the 3-vector [vi] is the
    shift.  The transform is [xnew] = [uij]*[xold] + [vi].
    
    === FORM 1a === [added 24 Jul 2007]
    mfile is the name of an ASCII file with multiple rows, each
    containing 12 numbers in the order
      u11 u12 u13 v1 u21 u22 u23 v2 u31 u32 u33 v3
    The filename must end in the characters '.aff12.1D', as output
    by the '-1Dmatrix_save' option in 3dAllineate and 3dvolreg.
    Each row of this file is treated as a separate matrix, and
    multiple matrices will be computed.
    ** N.B.: At most ONE input matrix can be in this format! **
    
    === FORM 2 ===
    mfile is of the form 'dataset::attribute', where 'dataset'
    is the name of an AFNI dataset, and 'attribute' is the name
    of an attribute in the dataset's header that contains a
    matrix+vector.  Examples:
     'fred+orig::VOLREG_MATVEC_000000'        = fred+orig from 3dvolreg
     'fred+acpc::WARP_DATA'                   = fred+acpc warped in AFNI
     'fred+orig::WARPDRIVE_MATVEC_FOR_000000' = fred+orig from 3dWarpDrive
     'fred+orig::ROTATE_MATVEC_000000'        = fred+orig from 3drotate
     For matrices to turn voxel coordinates to dicom:
     'fred+orig::IJK_TO_CARD_DICOM'   
     'fred+orig::IJK_TO_DICOM_REAL'        
    
    Note that both of VOLREG_MATVEC_ and ROTATE_MATVEC_ are usually
    accompanied with VOLREG_CENTER_OLD and VOLREG_CENTER_BASE or
    ROTATE_CENTER_OLD and ROTATE_CENTER_BASE attributes.
    These center attributes are automatically taken into account in
    cat_matvec's output.
    
    === FORM 3 ===
    mfile is of the form
     'MATRIX(u11,u12,u13,v1,u21,u22,u23,v2,u31,u32,u33,v3)'
    directly giving all 12 numbers on the command line.  You will
    need the 'forward single quotes' around this argument.
    
    === FORM 4 ===
    mfile is of the form
     '-rotate xI yR zA'
    where 'x', 'y', and 'z' are angles in degrees, specifying rotations
    about the I, R, and A axes respectively.  The letters 'I', 'R', 'A'
    specify the axes, and can be altered as in program 3drotate.
    (The 'quotes' are mandatory here because the argument contains spaces.)
    
    
    === COMPUTATIONS ===
    If [U] [v] are the matrix/vector for the first mfile, and
       [A] [b] are the matrix/vector for the second mfile, then
    the catenated transformation is
      matrix = [A][U]   vector = [A][v] + [b]
    That is, the second mfile transformation follows the first.
    ** Thus, the order of matrix multiplication is exactly the  **
    ** opposite of the order of the inputs on the command line! **
    
    The optional 'opkey' (operation key) following each mfile
    starts with a '-', and then is a set of letters telling how
    to treat the input.  The opkeys currently defined are:
    
      -I = invert the transformation:
                         -1              -1
           [xold] = [uij]  [xnew] - [uij]  [vi]
    
      -P = Do a polar decomposition on the 3x3 matrix part 
           of the mfile. This would result in an orthogonal
           matrix (rotation only, no scaling) Q that is closest,
           in the Frobenius distance sense, to the input matrix A.
        Note: if A = R * S * E, where R, S and E are the Rotation,
           Scale, and shEar matrices, respctively, Q does not 
           necessarily equal R because of interaction; Each of R,
           S and E affects most of the columns in matrix A.
    
      -IP = -I followed by -P
    
      -S = square root of the matrix
        Note: Not all matrices have square roots!
           The square root of a matrix will do 'half' the transformation.
           One application: 3dLRflip + 3dAllineate to register a volume
           to its mirror image, then apply half the transformation to
           bring it into vertical alignment.
    
    The transformation resulting by catenating the transformations
    is written to stdout in the same 3x4 ASCII file format.  This can
    be used as input to '3drotate -matvec_dicom' (provided [uij] is a
    proper orthogonal matrix), or to '3dWarp -matvec_xxx'.
    
      -MATRIX: indicates that the resulting matrix will
          be written to stdout in the 'MATRIX(...)' format (FORM 3).
          This feature could be used, with clever scripting, to input
          a matrix directly on the command line to program 3dWarp.
      -ONELINE: option indicates that the resulting matrix
          will simply be written as 12 numbers on one line.
      -4x4: Output matrix in augmented form (last row is 0 0 0 1)
            This option does not work with -MATRIX or -ONELINE
    N.B.: If only 9 numbers can be read from an mfile, then those
          values form the [uij] matrix, and the vector is set to zero.
    N.B.: If form 1a (.aff12.1D) is used to compute multiple matrices,
          then the output matrices are written to stdout, one matrix
