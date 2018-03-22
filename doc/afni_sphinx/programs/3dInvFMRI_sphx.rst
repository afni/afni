*********
3dInvFMRI
*********

.. _3dInvFMRI:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dInvFMRI [options]
    Program to compute stimulus time series, given a 3D+time dataset
    and an activation map (the inverse of the usual FMRI analysis problem).
    -------------------------------------------------------------------
    OPTIONS:
    
     -data yyy  =
       *OR*     = Defines input 3D+time dataset [a non-optional option].
     -input yyy =
    
     -map  aaa  = Defines activation map; 'aaa' should be a bucket dataset,
                    each sub-brick of which defines the beta weight map for
                    an unknown stimulus time series [also non-optional].
    
     -mapwt www = Defines a weighting factor to use for each element of
                    the map.  The dataset 'www' can have either 1 sub-brick,
                    or the same number as in the -map dataset.  In the
                    first case, in each voxel, each sub-brick of the map
                    gets the same weight in the least squares equations.
                      [default: all weights are 1]
    
     -mask mmm  = Defines a mask dataset, to restrict input voxels from
                    -data and -map.  [default: all voxels are used]
    
     -base fff  = Each column of the 1D file 'fff' defines a baseline time
                    series; these columns should be the same length as
                    number of time points in 'yyy'.  Multiple -base options
                    can be given.
     -polort pp = Adds polynomials of order 'pp' to the baseline collection.
                    The default baseline model is '-polort 0' (constant).
                    To specify no baseline model at all, use '-polort -1'.
    
     -out vvv   = Name of 1D output file will be 'vvv'.
                    [default = '-', which is stdout; probably not good]
    
     -method M  = Determines the method to use.  'M' is a single letter:
                   -method C = least squares fit to data matrix Y [default]
                   -method K = least squares fit to activation matrix A
    
     -alpha aa  = Set the 'alpha' factor to 'aa'; alpha is used to penalize
                    large values of the output vectors.  Default is 0.
                    A large-ish value for alpha would be 0.1.
    
     -fir5     = Smooth the results with a 5 point lowpass FIR filter.
     -median5  = Smooth the results with a 5 point median filter.
                   [default: no smoothing; only 1 of these can be used]
    -------------------------------------------------------------------
    METHODS:
     Formulate the problem as
        Y = V A' + F C' + errors
     where Y = data matrix      (N x M) [from -data]
           V = stimulus         (N x p) [to -out]
           A = map matrix       (M x p) [from -map]
           F = baseline matrix  (N x q) [from -base and -polort]
           C = baseline weights (M x q) [not computed]
           N = time series length = length of -data file
           M = number of voxels in mask
           p = number of stimulus time series to estimate
             = number of parameters in -map file
           q = number of baseline parameters
       and ' = matrix transpose operator
     Next, define matrix Z (Y detrended relative to columns of F) by
                           -1
       Z = [I - F(F'F)  F']  Y
    -------------------------------------------------------------------
     The method C solution is given by
                     -1
       V0 = Z A [A'A]
    
     This solution minimizes the sum of squares over the N*M elements
     of the matrix   Y - V A' + F C'   (N.B.: A' means A-transpose).
    -------------------------------------------------------------------
     The method K solution is given by
                 -1                            -1
       W = [Z Z']  Z A   and then   V = W [W'W]
    
     This solution minimizes the sum of squares of the difference between
     the A(V) predicted from V and the input A, where A(V) is given by
                        -1
       A(V) = Z' V [V'V]   = Z'W
    -------------------------------------------------------------------
     Technically, the solution is unidentfiable up to an arbitrary
     multiple of the columns of F (i.e., V = V0 + F G, where G is
     an arbitrary q x p matrix); the solution above is the solution
     that is orthogonal to the columns of F.
    
    -- RWCox - March 2006 - purely for experimental purposes!
    
    ===================== EXAMPLE USAGE =====================================
    ** Step 1: From a training dataset, generate activation map.
      The input dataset has 4 runs, each 108 time points long.  3dDeconvolve
      is used on the first 3 runs (time points 0..323) to generate the
      activation map.  There are two visual stimuli (Complex and Simple).
    
      3dDeconvolve -x1D xout_short_two.1D -input rall_vr+orig'[0..323]'   \
          -num_stimts 2                                                   \
          -stim_file 1 hrf_complex.1D               -stim_label 1 Complex \
          -stim_file 2 hrf_simple.1D                -stim_label 2 Simple  \
          -concat '1D:0,108,216'                                          \
          -full_first -fout -tout                                         \
          -bucket func_ht2_short_two -cbucket cbuc_ht2_short_two
    
      N.B.: You may want to de-spike, smooth, and register the 3D+time
            dataset prior to the analysis (as usual).  These steps are not
            shown here -- I'm presuming you know how to use AFNI already.
    
    ** Step 2: Create a mask of highly activated voxels.
      The F statistic threshold is set to 30, corresponding to a voxel-wise
      p = 1e-12 = very significant.  The mask is also lightly clustered, and
      restricted to brain voxels.
    
      3dAutomask -prefix Amask rall_vr+orig
      3dcalc -a 'func_ht2_short+orig[0]' -b Amask+orig -datum byte \
             -nscale -expr 'step(a-30)*b' -prefix STmask300
      3dmerge -dxyz=1 -1clust 1.1 5 -prefix STmask300c STmask300+orig
    
    ** Step 3: Run 3dInvFMRI to estimate the stimulus functions in run #4.
      Run #4 is time points 324..431 of the 3D+time dataset (the -data
      input below).  The -map input is the beta weights extracted from
      the -cbucket output of 3dDeconvolve.
    
      3dInvFMRI -mask STmask300c+orig                       \
                -data rall_vr+orig'[324..431]'              \
                -map cbuc_ht2_short_two+orig'[6..7]'        \
                -polort 1 -alpha 0.01 -median5 -method K    \
                -out ii300K_short_two.1D
    
      3dInvFMRI -mask STmask300c+orig                       \
                -data rall_vr+orig'[324..431]'              \
                -map cbuc_ht2_short_two+orig'[6..7]'        \
                -polort 1 -alpha 0.01 -median5 -method C    \
                -out ii300C_short_two.1D
    
    ** Step 4: Plot the results, and get confused.
    
      1dplot -ynames VV KK CC -xlabel Run#4 -ylabel ComplexStim \
             hrf_complex.1D'{324..432}'                         \
             ii300K_short_two.1D'[0]'                           \
             ii300C_short_two.1D'[0]'
    
      1dplot -ynames VV KK CC -xlabel Run#4 -ylabel SimpleStim \
             hrf_simple.1D'{324..432}'                         \
             ii300K_short_two.1D'[1]'                          \
             ii300C_short_two.1D'[1]'
    
      N.B.: I've found that method K works better if MORE voxels are
            included in the mask (lower threshold) and method C if
            FEWER voxels are included.  The above threshold gave 945
            voxels being used to determine the 2 output time series.
    =========================================================================
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
