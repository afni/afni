***********
3dWarpDrive
***********

.. _3dWarpDrive:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dWarpDrive [options] dataset
    Warp a dataset to match another one (the base).
    
    This program is a generalization of 3dvolreg.  It tries to find
    a spatial transformation that warps a given dataset to match an
    input dataset (given by the -base option).  It will be slow.
    
     *** Also see the script align_epi_anat.py for a more general ***
     **  alignment procedure, which does not require that the two  **
     **  datasets be defined on the same 3D grid.                  **
     **  align_epi_anat.py uses program 3dAllineate, which can     **
     *** also do nonlinear (polynomial) warping for registration. ***
    
    --------------------------
    Transform Defining Options: [exactly one of these must be used]
    --------------------------
      -shift_only         =  3 parameters (shifts)
      -shift_rotate       =  6 parameters (shifts + angles)
      -shift_rotate_scale =  9 parameters (shifts + angles + scale factors)
      -affine_general     = 12 parameters (3 shifts + 3x3 matrix)
      -bilinear_general   = 39 parameters (3 + 3x3 + 3x3x3)
    
      N.B.: At this time, the image intensity is NOT 
             adjusted for the Jacobian of the transformation.
      N.B.: -bilinear_general is not yet implemented.
    
    -------------
    Other Options:
    -------------
      -linear   }
      -cubic    } = Chooses spatial interpolation method.
      -NN       } =   [default = linear; inaccurate but fast]
      -quintic  }     [for accuracy, try '-cubic -final quintic']
    
      -base bbb   = Load dataset 'bbb' as the base to which the
                      input dataset will be matched.
                      [This is a mandatory option]
    
      -verb       = Print out lots of information along the way.
      -prefix ppp = Sets the prefix of the output dataset.
                    If 'ppp' is 'NULL', no output dataset is written.
      -input ddd  = You can put the input dataset anywhere in the
                      command line option list by using the '-input'
                      option, instead of always putting it last.
      -summ sss   = Save summary of calculations into text file 'sss'.
                      (N.B.: If 'sss' is '-', summary goes to stdout.)
    
    -----------------
    Technical Options:
    -----------------
      -maxite    m  = Allow up to 'm' iterations for convergence.
      -delta     d  = Distance, in voxel size, used to compute
                       image derivatives using finite differences.
                       [Default=1.0]
      -weight  wset = Set the weighting applied to each voxel
                       proportional to the brick specified here.
                       [Default=computed by program from base]
      -thresh    t  = Set the convergence parameter to be RMS 't' voxels
                       movement between iterations.  [Default=0.03]
      -twopass      = Do the parameter estimation in two passes,
                       coarse-but-fast first, then fine-but-slow second
                       (much like the same option in program 3dvolreg).
                       This is useful if large-ish warping is needed to
                       align the volumes.
      -final 'mode' = Set the final warp to be interpolated using 'mode'
                       instead of the spatial interpolation method used
                       to find the warp parameters.
      -parfix n v   = Fix the n'th parameter of the warp model to
                       the value 'v'.  More than one -parfix option
                       can be used, to fix multiple parameters.
      -1Dfile ename = Write out the warping parameters to the file
                       named 'ename'.  Each sub-brick of the input
                       dataset gets one line in this file.  Each
                       parameter in the model gets one column.
      -float        = Write output dataset in float format, even if
                       input dataset is short or byte.
      -coarserot    = Initialize shift+rotation parameters by a
                       brute force coarse search, as in the similar
                       3dvolreg option.
    
      -1Dmatrix_save ff = Save base-to-input transformation matrices
                          in file 'ff' (1 row per sub-brick in the input
                          dataset).  If 'ff' does NOT end in '.1D', then
                          the program will append '.aff12.1D' to 'ff' to
                          make the output filename.
              *N.B.: This matrix is the coordinate transformation from base
                     to input DICOM coordinates.  To get the inverse matrix
                     (input-to-base), use the cat_matvec program, as in
                       cat_matvec fred.aff12.1D -I
    
    ----------------------
    AFFINE TRANSFORMATIONS:
    ----------------------
    The options below control how the affine tranformations
    (-shift_rotate, -shift_rotate_scale, -affine_general)
    are structured in terms of 3x3 matrices:
    
      -SDU or -SUD }= Set the order of the matrix multiplication
      -DSU or -DUS }= for the affine transformations:
      -USD or -UDS }=   S = triangular shear (params #10-12)
                        D = diagonal scaling matrix (params #7-9)
                        U = rotation matrix (params #4-6)
                      Default order is '-SDU', which means that
                      the U matrix is applied first, then the
                      D matrix, then the S matrix.
    
      -Supper      }= Set the S matrix to be upper or lower
      -Slower      }= triangular [Default=lower triangular]
    
      -ashift OR   }= Apply the shift parameters (#1-3) after OR
      -bshift      }= before the matrix transformation. [Default=after]
    
    The matrices are specified in DICOM-ordered (x=-R+L,y=-A+P,z=-I+S)
    coordinates as:
    
      [U] = [Rotate_y(param#6)] [Rotate_x(param#5)] [Rotate_z(param #4)]
            (angles are in degrees)
    
      [D] = diag( param#7 , param#8 , param#9 )
    
            [    1        0     0 ]        [ 1 param#10 param#11 ]
      [S] = [ param#10    1     0 ]   OR   [ 0    1     param#12 ]
            [ param#11 param#12 1 ]        [ 0    0        1     ]
    
     For example, the default (-SDU/-ashift/-Slower) has the warp
     specified as [x]_warped = [S] [D] [U] [x]_in + [shift].
     The shift vector comprises parameters #1, #2, and #3.
    
     The goal of the program is to find the warp parameters such that
       I([x]_warped) = s * J([x]_in)
     as closely as possible in a weighted least squares sense, where
     's' is a scaling factor (an extra, invisible, parameter), J(x)
     is the base image, I(x) is the input image, and the weight image
     is a blurred copy of J(x).
    
     Using '-parfix', you can specify that some of these parameters
     are fixed.  For example, '-shift_rotate_scale' is equivalent
     '-affine_general -parfix 10 0 -parfix 11 0 -parfix 12 0'.
     Don't attempt to use the '-parfix' option unless you understand
     this example!
    
    -------------------------
      RWCox - November 2004
    -------------------------
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
