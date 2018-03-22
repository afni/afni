******
3dMean
******

.. _ahelp_3dMean:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dMean [options] dset dset ...
    Takes the voxel-by-voxel mean of all input datasets;
    the main reason is to be faster than 3dcalc.
    
    Options [see 3dcalc -help for more details on these]:
      -verbose    = Print out some information along the way.
      -prefix ppp = Sets the prefix of the output dataset.
      -datum ddd  = Sets the datum of the output dataset.
      -fscale     = Force scaling of the output to the maximum integer range.
      -gscale     = Same as '-fscale', but also forces each output sub-brick to
                      to get the same scaling factor.
      -nscale     = Don't do any scaling on output to byte or short datasets.
                      ** Only use this option if you are sure you
                         want the output dataset to be integer-valued!
      -non_zero   = Use only non-zero values for calculation of mean or squares
    
      -sd *OR*    = Calculate the standard deviation, sqrt(variance), instead
      -stdev         of the mean (cannot be used with -sqr, -sum or -non_zero).
    
      -sqr        = Average the squares, instead of the values.
      -sum        = Just take the sum (don't divide by number of datasets).
      -count      = compute only the count of non-zero voxels.
    
      -mask_inter = Create a simple intersection mask.
      -mask_union = Create a simple union mask.
    
                    The masks will be set by any non-zero voxels in
                    the input datasets.
    
      -weightset WSET = Sum of N dsets will be weighted by N volume WSET.
                        e.g. -weightset opt_comb_weights+tlrc
                    This weight dataset must be of type float.
    
    N.B.: All input datasets must have the same number of voxels along
           each axis (x,y,z,t).
        * At least 2 input datasets are required.
        * Dataset sub-brick selectors [] are allowed.
        * The output dataset origin, time steps, etc., are taken from the
           first input dataset.
    *** If you are trying to compute the mean (or some other statistic)
        across time for a 3D+time dataset (not across datasets), use
        3dTstat instead.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
