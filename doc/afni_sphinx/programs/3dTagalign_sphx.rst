.. contents:: 
    :depth: 4 

**********
3dTagalign
**********

.. code-block:: none

    Usage: 3dTagalign [options] dset
    Rotates/translates dataset 'dset' to be aligned with the master,
    using the tagsets embedded in their .HEAD files.
    
    Options:
     -master mset  = Use dataset 'mset' as the master dataset
                       [this is a nonoptional option]
    
     -nokeeptags   = Don't put transformed locations of dset's tags
                       into the output dataset [default = keep tags]
    
     -matvec mfile = Write the matrix+vector of the transformation to
                       file 'mfile'.  This can be used as input to the
                       '-matvec_in2out' option of 3dWarp, if you want
                       to align other datasets in the same way (e.g.,
                       functional datasets).
    
     -rotate       = Compute the best transformation as a rotation + shift.
                       This is the default.
    
     -affine       = Compute the best transformation as a general affine
                       map rather than just a rotation + shift.  In all
                       cases, the transformation from input to output
                       coordinates is of the form
                          [out] = [R] [in] + [V]
                       where [R] is a 3x3 matrix and [V] is a 3-vector.
                       By default, [R] is computed as a proper (det=1)
                       rotation matrix (3 parameters).  The '-affine'
                       option says to fit [R] as a general matrix
                       (9 parameters).
               N.B.: An affine transformation can rotate, rescale, and
                       shear the volume.  Be sure to look at the dataset
                       before and after to make sure things are OK.
    
     -rotscl       = Compute transformation as a rotation times an isotropic
                       scaling; that is, [R] is an orthogonal matrix times
                       a scalar.
               N.B.: '-affine' and '-rotscl' do unweighted least squares.
    
     -prefix pp    = Use 'pp' as the prefix for the output dataset.
                       [default = 'tagalign']
     -verb         = Print progress reports
     -dummy        = Don't actually rotate the dataset, just compute
                       the transformation matrix and vector.  If
                       '-matvec' is used, the mfile will be written.
      -linear     }
      -cubic      } = Chooses spatial interpolation method.
      -NN         } =   [default = cubic]
      -quintic    }
    
    Nota Bene:
    * The transformation is carried out
      using the same methods as program 3dWarp.
    
    Author: RWCox - 16 Jul 2000, etc.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
