.. _ahelp_3dDespike:

*********
3dDespike
*********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dDespike [options] dataset
    
    Removes 'spikes' from the 3D+time input dataset and writes
    a new dataset with the spike values replaced by something
    more pleasing to the eye.
    
    ------------------
    Outline of Method:
    ------------------
     * L1 fit a smooth-ish curve to each voxel time series
        [see -corder option for description of the curve]
        [see -NEW option for a different & faster fitting method]
     * Compute the MAD of the difference between the curve and
        the data time series (the residuals).
     * Estimate the standard deviation 'sigma' of the residuals
        from the MAD.
     * For each voxel value, define s = (value-curve)/sigma.
     * Values with s > c1 are replaced with a value that yields
        a modified s' = c1+(c2-c1)*tanh((s-c1)/(c2-c1)).
     * c1 is the threshold value of s for a 'spike' [default c1=2.5].
     * c2 is the upper range of the allowed deviation from the curve:
        s=[c1..infinity) is mapped to s'=[c1..c2)   [default c2=4].
    
    An alternative method for replacing the spike value is provided
    by the '-localedit' option, and that method is preferred by
    many users.
    
    The input dataset can be stored in short or float formats.
    The output dataset will always be stored in floats. [Feb 2017]
    
    --------
    Options:
    --------
     -ignore I  = Ignore the first I points in the time series:
                   these values will just be copied to the
                   output dataset [default I=0].
     -corder L  = Set the curve fit order to L:
                   the curve that is fit to voxel data v(t) is
    
                           k=L [        (2*PI*k*t)          (2*PI*k*t) ]
     f(t) = a+b*t+c*t*t + SUM  [ d * sin(--------) + e * cos(--------) ]
                           k=1 [  k     (    T   )    k     (    T   ) ]
    
                   where T = duration of time series;
                   the a,b,c,d,e parameters are chosen to minimize
                   the sum over t of |v(t)-f(t)| (L1 regression);
                   this type of fitting is is insensitive to large
                   spikes in the data.  The default value of L is
                   NT/30, where NT = number of time points.
    
     -cut c1 c2 = Alter default values for the spike cut values
                   [default c1=2.5, c2=4.0].
    
     -prefix pp = Save de-spiked dataset with prefix 'pp'
                   [default pp='despike']
    
     -ssave ttt = Save 'spikiness' measure s for each voxel into a
                   3D+time dataset with prefix 'ttt' [default=no save]
    
     -nomask    = Process all voxels
                   [default=use a mask of high-intensity voxels, ]
                   [as created via '3dAutomask -dilate 4 dataset'].
    
     -dilate nd = Dilate 'nd' times (as in 3dAutomask).  The default
                   value of 'nd' is 4.
    
     -q[uiet]   = Don't print '++' informational messages.
    
     -localedit = Change the editing process to the following:
                    If a voxel |s| value is >= c2, then replace
                    the voxel value with the average of the two
                    nearest non-spike (|s| < c2) values; the first
                    one previous and the first one after.
                    Note that the c1 cut value is not used here.
    
     -NEW       = Use the 'new' method for computing the fit, which
                  should be faster than the L1 method for long time
                  series (200+ time points); however, the results
                  are similar but NOT identical. [29 Nov 2013]
                  * You can also make the program use the 'new'
                    method by setting the environment variable
                      AFNI_3dDespike_NEW
                    to the value YES; as in
                      setenv AFNI_3dDespike_NEW YES  (csh)
                      export AFNI_3dDespike_NEW=YES  (bash)
                  * If this variable is set to YES, you can turn off
                    the '-NEW' processing by using the '-OLD' option.
              -->>* For time series more than 500 points long, the
                    '-OLD' algorithm is tremendously slow.  You should
                    use the '-NEW' algorith in such cases.
                 ** At some indeterminate point in the future, the '-NEW'
                    method will become the default!
              -->>* As of 29 Sep 2016, '-NEW' is the default if there
                    is more than 500 points in the time series dataset.
    
     -NEW25     = A slightly more aggressive despiking approach than
                  the '-NEW' method.
    
    --------
    Caveats:
    --------
    * Despiking may interfere with image registration, since head
       movement may produce 'spikes' at the edge of the brain, and
       this information would be used in the registration process.
       This possibility has not been explored or calibrated.
    
    * [LATER] Actually, it seems like the registration problem
       does NOT happen, and in fact, despiking seems to help!
    
    * Check your data visually before and after despiking and
       registration!
    
     =========================================================================
    * This binary version of 3dDespike is compiled using OpenMP, a semi-
       automatic parallelizer software toolkit, which splits the work across
       multiple CPUs/cores on the same shared memory computer.
    * OpenMP is NOT like MPI -- it does not work with CPUs connected only
       by a network (e.g., OpenMP doesn't work with 'cluster' setups).
    * For implementation and compilation details, please see
       https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html
    * The number of CPU threads used will default to the maximum number on
       your system. You can control this value by setting environment variable
       OMP_NUM_THREADS to some smaller value (including 1).
    * Un-setting OMP_NUM_THREADS resets OpenMP back to its default state of
       using all CPUs available.
       ++ However, on some systems, it seems to be necessary to set variable
          OMP_NUM_THREADS explicitly, or you only get one CPU.
       ++ On other systems with many CPUS, you probably want to limit the CPU
          count, since using more than (say) 16 threads is probably useless.
    * You must set OMP_NUM_THREADS in the shell BEFORE running the program,
       since OpenMP queries this variable BEFORE the program actually starts.
       ++ You can't usefully set this variable in your ~/.afnirc file or on the
          command line with the '-D' option.
    * How many threads are useful? That varies with the program, and how well
       it was coded. You'll have to experiment on your own systems!
    * The number of CPUs on this particular computer system is ...... 16.
    * The maximum number of CPUs that will be used is now set to .... 12.
     =========================================================================
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
