***********
3dBrickStat
***********

.. _ahelp_3dBrickStat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dBrickStat [options] dataset
    Compute maximum and/or minimum voxel values of an input dataset
    
    The output is a number to the console.  The input dataset
    may use a sub-brick selection list, as in program 3dcalc.
    
    Note that this program computes ONE number as the output; e.g.,
    the mean over all voxels and time points.  If you want (say) the
    mean over all voxels but for each time point individually, see
    program 3dmaskave.
    
    Note: If you don't specify one sub-brick, the parameter you get
    ----- back is computed from all the sub-bricks in dataset.
    Options :
      -quick = get the information from the header only (default)
      -slow = read the whole dataset to find the min and max values
             all other options except min and max imply slow
      -min = print the minimum value in dataset
      -max = print the maximum value in dataset (default)
      -mean = print the mean value in dataset 
      -sum = print the sum of values in the dataset
      -var = print the variance in the dataset 
      -stdev = print the standard deviation in the dataset 
               -stdev and -var are mutually exclusive
      -count = print the number of voxels included
      -volume = print the volume of voxels included in microliters
      -positive = include only positive voxel values 
      -negative = include only negative voxel values 
      -zero = include only zero voxel values 
      -non-positive = include only voxel values 0 or negative 
      -non-negative = include only voxel values 0 or greater 
      -non-zero = include only voxel values not equal to 0 
      -absolute = use absolute value of voxel values for all calculations
                  can be combined with restrictive non-positive, non-negative,
                  etc. even if not practical. Ignored for percentile and
                  median computations.
      -nan = include only voxel values that are finite numbers, 
             not NaN, or inf. -nan forces -slow mode.
      -nonan =exclude voxel values that are not numbers
      -mask dset = use dset as mask to include/exclude voxels
      -mrange MIN MAX = Only accept values between MIN and MAX (inclusive)
                        from the mask. Default it to accept all non-zero
                        voxels.
      -mvalue VAL = Only accept values equal to VAL from the mask.
      -automask = automatically compute mask for dataset
        Can not be combined with -mask
      -percentile p0 ps p1 write the percentile values starting
                  at p0% and ending at p1% at a step of ps%
                  Output is of the form p% value   p% value ...
                  Percentile values are output first. 
                  Only one sub-brick is accepted as input with this option.
                  Write the author if you REALLY need this option
                  to work with multiple sub-bricks.
      -median a shortcut for -percentile 50 1 50
      -ver = print author and version info
      -help = print this help screen
    
    INPUT DATASET NAMES
    -------------------
    This program accepts datasets that are modified on input according to the
    following schemes:
      'r1+orig[3..5]'                                    {sub-brick selector}
      'r1+orig<100..200>'                                {sub-range selector}
      'r1+orig[3..5]<100..200>'                          {both selectors}
      '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}
    For the gruesome details, see the output of 'afni -help'.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
