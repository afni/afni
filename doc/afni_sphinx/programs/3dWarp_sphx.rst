.. _ahelp_3dWarp:

******
3dWarp
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dWarp [options] dataset
    Warp (spatially transform) a 3D dataset.
    --------------------------
    Transform Defining Options: [exactly one of these must be used]
    --------------------------
      -matvec_in2out mmm = Read a 3x4 affine transform matrix+vector
                            from file 'mmm':
                             x_out = Matrix x_in + Vector
    
      -matvec_out2in mmm = Read a 3x4 affine transform matrix+vector
                             from file 'mmm':
                             x_in = Matrix x_out + Vector
    
         ** N.B.: The coordinate vectors described above are
                   defined in DICOM ('RAI') coordinate order.
                   (Also see the '-fsl_matvec option, below.)
         ** N.B.: Using the special name 'IDENTITY' for 'mmm'
                   means to use the identity matrix.
         ** N.B.: You can put the matrix on the command line
                   directly by using an argument of the form
           'MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)'
                   in place of 'mmm', where the aij values are the
                   matrix entries (aij = i-th row, j-th column),
                   separated by commas.
                 * You will need the 'forward single quotes' around
                   the argument.
    
      -tta2mni = Transform a dataset in Talairach-Tournoux Atlas
                  coordinates to MNI-152 coordinates.
      -mni2tta = Transform a dataset in MNI-152 coordinates to
                  Talairach-Tournoux Atlas coordinates.
    
      -matparent mset = Read in the matrix from WARPDRIVE_MATVEC_*
                         attributes in the header of dataset 'mset',
                         which must have been created by program
                         3dWarpDrive.  In this way, you can apply
                         a transformation matrix computed from
                         in 3dWarpDrive to another dataset.
    
         ** N.B.: The above option is analogous to the -rotparent
                    option in program 3drotate.  Use of -matparent
                    should be limited to datasets whose spatial
                    coordinate system corresponds to that which
                    was used for input to 3dWarpDrive (i.e., the
                    input to 3dWarp should overlay properly with
                    the input to 3dWarpDrive that generated the
                    -matparent dataset).
    
      -card2oblique obl_dset or 
      -oblique_parent obl_dset = Read in the oblique transformation matrix
         from an oblique dataset and make cardinal dataset oblique to match.
      -deoblique or
      -oblique2card = Transform an oblique dataset to a cardinal dataset
         Both these oblique transformation options require a new grid for the
         output as specified with the -newgrid or -gridset options
         or a new grid will be assigned based on the minimum voxel spacing
        ** N.B.: EPI time series data should be time shifted with 3dTshift before                rotating the volumes to a cardinal direction
    
    Sample usages:
     3dWarpDrive -affine_general -base d1+orig -prefix d2WW -twopass -input d2+orig
     3dWarp -matparent d2WW+orig -prefix epi2WW epi2+orig
    
     3dWarp -card2oblique oblique_epi+orig -prefix oblique_anat card_anat+orig
     3dWarp -oblique2card -prefix card_epi_tshift -newgrid 3.5 epi_tshift+orig
    
    Example of warping +tlrc results back to +orig space of some subject
    (get xform matrix, apply it, tell dataset it is not in orig space):
    
        cat_matvec subj1_anat+tlrc::WARP_DATA > tlrc_xform.1D
        3dWarp -matvec_out2in tlrc_xform.1D -prefix group_warped+tlrc \
               -gridset subj1_epi+orig -cubic group_data+tlrc
        3drefit -view orig group_warped+tlrc
    
    -----------------------
    Other Transform Options:
    -----------------------
      -linear     }
      -cubic      } = Chooses spatial interpolation method.
      -NN         } =   [default = linear]
      -quintic    }
    
      -fsl_matvec   = Indicates that the matrix file 'mmm' uses FSL
                        ordered coordinates ('LPI').  For use with
                        matrix files from FSL and SPM.
    
      -newgrid ddd  = Tells program to compute new dataset on a
                        new 3D grid, with spacing of 'ddd' mmm.
                      * If this option is given, then the new
                        3D region of space covered by the grid
                        is computed by warping the 8 corners of
                        the input dataset, then laying down a
                        regular grid with spacing 'ddd'.
                      * If this option is NOT given, then the
                        new dataset is computed on the old
                        dataset's grid.
    
      -gridset ggg  = Tells program to compute new dataset on the
                        same grid as dataset 'ggg'.
    
      -zpad N       = Tells program to pad input dataset with 'N'
                        planes of zeros on all sides before doing
                        transformation.
    ---------------------
    Miscellaneous Options:
    ---------------------
      -verb         = Print out some information along the way.
      -prefix ppp   = Sets the prefix of the output dataset.
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
