.. contents:: 
    :depth: 4 

*********
3dmaskave
*********

.. code-block:: none

    Usage: 3dmaskave [options] inputdataset
    
    Computes average of all voxels in the input dataset
    which satisfy the criterion in the options list.
    If no options are given, then all voxels are included.
    
    ------------------------------------------------------------
    Examples:
    
    1. compute the average timeseries in epi_r1+orig, over voxels
       that are set (any non-zero value) in the dataset, ROI+orig:
    
        3dmaskave -mask ROI+orig epi_r1+orig
    
    2. restrict the ROI to values of 3 or 4, and save (redirect)
       the output to the text file run1_roi_34.txt:
    
        3dmaskave -mask ROI+orig -quiet -mrange 3 4   \
                  epi_r1+orig > run1_roi_34.txt
    ------------------------------------------------------------
    
    Options:
      -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be averaged from 'dataset'.  Note
                     that the mask dataset and the input dataset
                     must have the same number of voxels.
                   SPECIAL CASE: If 'mset' is the string 'SELF',
                                 then the input dataset will be
                                 used to mask itself.  That is,
                                 only nonzero voxels from the
                                 #miv sub-brick will be used.
    
      -mindex miv  Means to use sub-brick #'miv' from the mask
                     dataset.  If not given, miv=0.
      -mrange a b  Means to further restrict the voxels from
                     'mset' so that only those mask values
                     between 'a' and 'b' (inclusive) will
                     be used.  If this option is not given,
                     all nonzero values from 'mset' are used.
                     Note that if a voxel is zero in 'mset', then
                     it won't be included, even if a < 0 < b.
                       [-mindex and -mrange are old options that predate]
                       [the introduction of the sub-brick selector '[]' ]
                       [and the sub-range value selector '<>' to AFNI.  ]
    
      -xbox x y z     } These options are the same as in
      -dbox x y z     } program 3dmaskdump:
      -nbox x y z     } They create a mask by putting down boxes
      -ibox x y z     } or balls (filled spheres) at the specified
      -xball x y z r  } locations.  See the output of
      -dball x y z r  }   3dmaskdump -help
      -nball x y z r  } for the gruesome and tedious details.
       https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dmaskdump.html
    
      -dindex div  Means to use sub-brick #'div' from the inputdataset.
                     If not given, all sub-bricks will be processed.
      -drange a b  Means to only include voxels from the inputdataset whose
                     values fall in the range 'a' to 'b' (inclusive).
                     Otherwise, all voxel values are included.
                       [-dindex and -drange are old options that predate]
                       [the introduction of the sub-brick selector '[]' ]
                       [and the sub-range value selector '<>' to AFNI.  ]
    
      -slices p q  Means to only included voxels from the inputdataset
                     whose slice numbers are in the range 'p' to 'q'
                     (inclusive).  Slice numbers range from 0 to
                     NZ-1, where NZ can be determined from the output
                     of program 3dinfo.  The default is to include
                     data from all slices.
                       [There is no provision for geometrical voxel]
                       [selection except in the slice (z) direction]
    
      -sigma       Means to compute the standard deviation in addition
                     to the mean.
      -sum         Means to compute the sum instead of the mean.
      -sumsq       Means to compute the sum of squares instead of the mean.
      -enorm       Means to compute the Euclidean norm instead of the mean.
                   This is the sqrt() of the sumsq result.
      -median      Means to compute the median instead of the mean.
      -max         Means to compute the max instead of the mean.
      -min         Means to compute the min instead of the mean.
                     [-sigma is ignored with -sum, -median, -max, or -min.]
                     [the last given of -sum, -median, -max, or -min wins.]
      -perc XX     Means to compute the XX-th percentile value (min=0 max=100).
                   XX should be an integer from 0 to 100.
      -dump        Means to print out all the voxel values that
                     go into the result.
      -udump       Means to print out all the voxel values that
                     go into the average, UNSCALED by any internal
                     factors.
                     N.B.: the scale factors for a sub-brick
                           can be found using program 3dinfo.
      -indump      Means to print out the voxel indexes (i,j,k) for
                     each dumped voxel.  Has no effect if -dump
                     or -udump is not also used.
                     N.B.: if nx,ny,nz are the number of voxels in
                           each direction, then the array offset
                           in the brick corresponding to (i,j,k)
                           is i+j*nx+k*nx*ny.
     -q     or
     -quiet        Means to print only the minimal numerical result(s).
                   This is useful if you want to create a *.1D file,
                   without any extra text; for example:
                     533.814 [18908 voxels]   ==   'normal' output
                     533.814                  ==   'quiet' output
    
    The output is printed to stdout (the terminal), and can be
    saved to a file using the usual redirection operation '>'.
    
    Or you can do fun stuff like
      3dmaskave -q -mask Mfile+orig timefile+orig | 1dplot -stdin -nopush
    to pipe the output of 3dmaskave into 1dplot for graphing.
    
    -- Author: RWCox
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
