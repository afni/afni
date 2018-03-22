*******
3dTto1D
*******

.. _3dTto1D:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    -------------------------------------------------------------------------
    3dTto1D             - collapse a 4D time series to a 1D time series
    
    The program takes as input a 4D times series (a list of 1D time series)
    and optionally a mask, and computes from in a 1D time series using some
    method applied to the first (backward) differences.  Methods include:
    
        enorm           : the Euclidean norm
        rms/dvars       : root mean square (DVARS)
        srms            : rms scaled down by global mean 
        shift_srms      : srms shifted by the global mean
        mdiff           : mean abs(diff)
        smdiff          : mdiff scaled down by global mean
        4095_count      : count voxels with max of exactly 4095
        4095_frac       : fraction of masked voxels with max of exactly 4095
        4095_warn       : warn if short datum and max of exactly 4095
    
    More details are provided after the examples.
    
    --------------------------------------------------
    examples:
    
    E1. compute SRMS of EPI time series within a brain mask
        (might be good for censoring, and is comparable across subjects)
    
           3dTto1D -input epi_r1+orig -mask mask.auto.nii.gz -method srms \
                   -prefix epi_srms.1D
    
    E2. compute DVARS of EPI time series within a brain mask
        (similarly good for censoring, but not comparable across subjects)
    
           3dTto1D -input epi_r1+orig -mask mask.auto.nii.gz -method dvars \
                   -prefix epi_dvars.1D
    
    E3. compute ENORM of motion parameters
        (as is done by afni_proc.py via 1d_tool.py)
    
        Note that 1D inputs will generally need the transpose operator,
        applied by appending an escaped ' to the -input dataset.
    
           3dTto1D -input dfile.r01.1D\' -method enorm -prefix enorm.r01.1D
    
    E4. warn if short data and max is 4095
    
           3dTto1D -input epi+orig -method 4095_warn
    
    --------------------------------------------------
    methods:
    
       Since the initial step is generally to compute the first (backwards)
       difference, call that dataset TDIFF.  The value at any voxel of TDIFF
       is the same as the input, minus the value at the prior time point.
       TDIFF is defined as 0 at time point 0.
    
       method enorm
    
          This is the Euclidean norm.
    
          Starting with the TDIFF dataset, the value at each time point is
          the Euclidean norm for that volume (or list of values).  This is
          the same as the L2-norm, and which is often applied to the motion
          parameters for censoring.
    
             enorm = sqrt(sum squares)
    
    
       method rms (or dvars)
    
          RMS = DVARS = enorm/sqrt(nvox).
    
          The RMS (root mean square) is the same as the enorm divided by
          sqrt(nvox).  It is like a standard deviation, but without removal
          of the mean (per time point).
    
             rms = dvars = enorm/sqrt(nvox) = sqrt(sum squares/nvox)
    
          This is the RMS of backward differences first described by Smyser
          et. al., 2010, for motion detection, and later renamed to DVARS by
          Power et. al., 2012.
    
        * DVARS survives a resampling, where it would be unchanged if every
          voxel were listed multiple times, for example.
    
        * DVARS does not survive a scaling, it scales with the data.
          This is why the SRMS method was introduced.
    
    
       method srms (or cvar) (= scaled rms = dvars/mean)
    
          This result is basically the coefficient of variation, but without
          removal of each volume mean.
    
          This is the same as dvars divided by the global mean, gmean.
    
             srms = dvars/gmean = enorm/sqrt(nvox)/gmean
    
        * SRMS survives both a resampling and scaling of the data.  Since it
          is unchanged with any data scaling (unlike DVARS), values are
          comparable across subjects and studies.
    
      *** The above 3 curves will look identical, subject to scaling.
    
    
       method shift_srms  (= srms - meandiff)
    
          This is simply the SRMS curve shifted down by the global mean of
          (the absolute values of) the first differences.  This is probably
          useless.
    
    
       method mdiff (mean diff = mean abs(first diff))
    
          Again, starting with the first backward difference, TDIFF, this
          is just the mean absolute value, per time point.
    
    
       method smdiff (scaled mean diff = mdiff/mean)
    
          This is the mean diff scaled by the global mean.
    
       method 4095_count
    
          At each time point, output the number of (masked) voxels that are
          exactly 4095.
    
       method 4095_frac
    
          At each time point, output the fraction of (masked) voxels that
          are exactly 4095.
    
       method 4095_warn
    
          Simply warn whether the maximum is exactly 4095 (so no -prefix).
    
    --------------------------------------------------
    informational command arguments:
    
       -help                    : show this help
       -hist                    : show program history
       -ver                     : show program version
    
    --------------------------------------------------
    required command arguments:
    
       -input DSET              : specify input dataset
    
             e.g. -input epi_r1+orig
             e.g. -input dfile.r01.1D\'
    
          Specify the input dataset to be processed.  This should be a set
          of 3D time series.  If the input is in 1D format, a transpose
          operator will typically be required.
    
       -method METHOD           : specify 4D to 1D conversion method
    
             e.g. -method srms
             e.g. -method DVARS
             e.g. -method dvars
             e.g. -method enorm
    
          Details of the computational methods are at the top of the help.
          The methods (which are case insensitive) include:
    
             enorm      : Euclidean norm of first differences
                          = sqrt(sum squares(first diffs))
    
             rms        : RMS (root mean square) of first differences
                          = DVARS = enorm/sqrt(nvox)
    
             srms       : scaled (by grand mean) RMS of first differences
                          = DVARS/mean
    
                      * seems like the most useful method for censoring
    
             s_srms     : SRMS shifted by grand mean abs of first diffs
                          = SRMS - mean(abs(first diffs))
    
             mdiff      : mean absolute first differences
                          = mean(abs(first diff))
    
             smdiff     : mdiff scaled by grand mean
                          = mdiff/mean
    
             4095_count : count of voxels that are exactly 4095
    
             4095_frac  : fraction of voxels that are exactly 4095
                          = 4095_count/(mask size)
    
             4095_warn  : state whether global max is exactly 4095
                          (no 1D output)
    
    --------------------------------------------------
    optional command arguments:
    
       -automask        : restrict computation to automask
       -mask MSET       : restrict computation to given mask
       -prefix PREFIX   : specify output file
             e.g.     -prefix SVAR_run1.1D
             default: -prefix stdout
       -verb LEVEL      : specify verbose level
             e.g.     -verb 2
             default: -verb 1
    
    --------------------------------------------------
    R Reynolds  July, 2017
    -------------------------------------------------------------------------
    3dTto1D version 1.2, 1 February 2018
    compiled: Mar  7 2018
