.. _ahelp_@compute_gcor:

*************
@compute_gcor
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    -----------------------------------------------------------------
    @compute_gcor       - compute GCOR, the global correlation
    
        usage : @compute_gcor [options] -input dataset
    
    This program computes the average correlation between every voxel
    and ever other voxel, over any given mask.  This output GCOR value
    is a single number.
    
    -----------------------------------------------------------------
    
    Common examples:
    
      0. This program can be used for 1D files:
    
            @compute_gcor -input data.1D
    
         HOWEVER, if column selection is desired, please use 1d_tool.py, directly.
    
            1d_tool.py -infile data.1D'[2..17]' -show_gcor
    
      1. Simple usage, akin to the afni_proc.py processing script.
    
            @compute_gcor -input errts.FT+orig -mask full_mask.FT+orig
    
         OR, for +tlrc:
    
            @compute_gcor -input errts.FT+tlrc -mask full_mask.FT+tlrc
    
      2. Speed things up slightly, an errts dataset does not need to be demeaned.
    
            @compute_gcor -no_demean -input errts.FT+tlrc -mask full_mask.FT+tlrc
    
      3. Be vewy, veeewy, qwiet...
    
            @compute_gcor -verb 0 -input errts.FT+tlrc -mask full_mask.FT+tlrc
    
         OR, save the result:
    
            set gcor = `@compute_gcor -verb 0 -input errts.FT+tlrc -mask full_mask.FT+tlrc`
    
      4. Output correlation volume: for each voxel, the average correlation
         with all voxels in mask.
    
         Specify correlation volume prefix, FT_corr.
    
           @compute_gcor -input errts.FT+tlrc -mask full_mask.FT+tlrc -corr_vol FT_corr
    
    -----------------------------------------------------------------
    
    Overview of processing steps: 
    
    0.  If the input is a 1D file, cheat and run "1d_tool.py -show_gcor", instead.
    
    otherwise...
    
    1.  Scale the input to a unit time series, so that each voxel voxel has a
        length of 1.
    
    2.  Compute the average of these unit time series.
    
    3a. If requested, compute the correlation volume, the dot product of the
        unit and average time series.
    
    3b.  Return GCOR = the length of the resulting average, squared.
    
    ---------------------------------------------
    
    terminal options:
    
       -help             : show this help
       -hist             : show modification history
       -ver              : show version number
    
    important processing options:
    
       -input DSET       : specify input dataset to compute the GCOR over
       -mask DSET        : specify mask dataset, for restricting the computation
    
    other processing options:
    
       -corr_vol PREFIX  : specify input dataset to compute the GCOR over
       -nfirst NFIRST    : specify number of initial TRs to ignore
       -no_demean        : do not (need to) demean as first step
       -savetmp          : save temporary files (do not remove at end)
       -verb VERB        : set verbose level (0=quiet, 3=max)
    
    ---------------------------------------------
    
    R Reynolds, Jan, 2013
