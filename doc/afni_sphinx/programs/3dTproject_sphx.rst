**********
3dTproject
**********

.. _3dTproject:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:  3dTproject [options]
    
    This program projects (detrends) out various 'nuisance' time series from each
    voxel in the input dataset.  Note that all the projections are done via linear
    regression, including the frequency-based options such as '-passband'.  In this
    way, you can bandpass time-censored data, and at the same time, remove other
    time series of no interest (e.g., physiological estimates, motion parameters).
    
    --------
    OPTIONS:
    --------
     -input dataset      = Specifies the input dataset.
     -prefix ppp         = Specifies the output dataset, as usual.
    
     -censor cname       = As in 3dDeconvolve.
     -CENSORTR clist     = As in 3dDeconvolve.
     -cenmode mode       = 'mode' specifies how censored time points are treated in
                           the output dataset:
                           ++ mode = ZERO ==> put zero values in their place
                                          ==> output datset is same length as input
                           ++ mode = KILL ==> remove those time points
                                          ==> output dataset is shorter than input
                           ++ mode = NTRP ==> censored values are replaced by interpolated
                                              neighboring (in time) non-censored values,
                                              BEFORE any projections, and then the
                                              analysis proceeds without actual removal
                                              of any time points -- this feature is to
                                              keep the Spanish Inquisition happy.
                           ** The default mode is KILL !!!
    
     -concat ccc.1D      = The catenation file, as in 3dDeconvolve, containing the
                           TR indexes of the start points for each contiguous run
                           within the input dataset (the first entry should be 0).
                           ++ Also as in 3dDeconvolve, if the input dataset is
                              automatically catenated from a collection of datasets,
                              then the run start indexes are determined directly,
                              and '-concat' is not needed (and will be ignored).
                           ++ Each run must have at least 9 time points AFTER
                              censoring, or the program will not work!
                           ++ The only use made of this input is in setting up
                              the bandpass/stopband regressors.
                           ++ '-ort' and '-dsort' regressors run through all time
                              points, as read in.  If you want separate projections
                              in each run, then you must either break these ort files
                              into appropriate components, OR you must run 3dTproject
                              for each run separately, using the appropriate pieces
                              from the ort files via the '{...}' selector for the
                              1D files and the '[...]' selector for the datasets.
     -noblock            = Also as in 3dDeconvolve, if you want the program to treat
                           an auto-catenated dataset as one long run, use this option.
                           ++ However, '-noblock' will not affect catenation if you use
                              the '-concat' option.
    
     -ort f.1D           = Remove each column in f.1D
                           ++ Multiple -ort options are allowed.
                           ++ Each column will have its mean removed.
     -polort pp          = Remove polynomials up to and including degree pp.
                           ++ Default value is 2.
                           ++ It makes no sense to use a value of pp greater than
                              2, if you are bandpassing out the lower frequencies!
                           ++ For catenated datasets, each run gets a separate set
                              set of pp+1 Legendre polynomial regressors.
                           ++ Use of -polort -1 is not advised (if data mean != 0),
                              even if -ort contains constant terms, as all means are
                              removed.
     -dsort fset         = Remove the 3D+time time series in dataset fset.
                           ++ That is, 'fset' contains a different nuisance time
                              series for each voxel (e.g., from AnatICOR).
                           ++ Multiple -dsort options are allowed.
    
     -passband fbot ftop = Remove all frequencies EXCEPT those in the range
      *OR* -bandpass       fbot..ftop.
                           ++ Only one -passband option is allowed.
     -stopband sbot stop = Remove all frequencies in the range sbot..stop.
                           ++ More than one -stopband option is allowed.
                           ++ For example, '-passband 0.01 0.10' is equivalent to
                              '-stopband 0 0.0099 -stopband 0.1001 9999'
     -dt dd              = Use time step dd for the frequency calculations,
     *OR* -TR              rather than the value stored in the dataset header.
    
     -mask mset          = Only operate on voxels nonzero in the mset dataset.
     *OR*                  ++ Use '-mask AUTO' to have the program generate the
     -automask                mask automatically (or use '-automask')
                           ++ Voxels outside the mask will be filled with zeros.
                           ++ If no masking option is given, then all voxels
                              will be processed.
    
     -blur fff           = Blur (inside the mask only) with a filter that has
                           width (FWHM) of fff millimeters.
                           ++ Spatial blurring (if done) is after the time
                              series filtering.
    
     -norm               = Normalize each output time series to have sum of
                           squares = 1. This is the LAST operation.
    
     -quiet              = Hide the super-fun and thrilling progress messages.
    
     -verb               = The program will save the fixed ort matrix and its
                           singular values into .1D files, for post-mortems.
                           It will also print out more progress messages, which
                           might help with figuring out what's happening when
                           problems occur.
    
    ------
    NOTES:
    ------
    * The output dataset is in floating point format.
    
    * Removal of the various undesired components is via linear regression.
       In particular, this method allows for bandpassing of censored time
       series.
    
    * If you like technical math jargon (and who doesn't?), this program
       performs orthogonal projection onto the null space of the set of 'ort'
       vectors assembled from the various options '-polort', '-ort',
       '-passband', '-stopband', and '-dsort'.
    
    * If A is a matrix whose column comprise the vectors to be projected
       out, define the projection matrix Q(A) by
        Q(A) = I - A psinv(A)
       where psinv(A) is the pseudo-inverse of A [e.g., inv(A'A)A' -- but
       the pseudo-inverse is actually calculated here via the SVD algorithm.]
    
    * If option '-dsort' is used, each voxel has a different matrix of
       regressors -- encode this extra set of regressors in matrix B
       (i.e., each column of B is a vector to be removed from its voxel's
       time series). Then the projection for the compound matrix [A B] is
          Q( Q(A)B ) Q(A) 
       that is, A is projected out of B, then the projector for that
       reduced B is formed, and applied to the projector for the
       voxel-independent A.  Since the number of columns in B is usually
       many fewer than the number of columns in A, this technique can
       be much faster than constructing the full Q([A B]) for each voxel.
       (Since Q(A) only need to be constructed once for all voxels.)
       A little fun linear algebra will show you that Q(Q(A)B)Q(A) = Q([A B]).
    
    * A similar regression could be done via the slower 3dTfitter program:
        3dTfitter -RHS inputdataset+orig   \
                  -LHS ort1.1D dsort2+orig \
                  -polort 2 -prefix NULL   \
                  -fitts Tfit
        3dcalc -a inputdataset+orig -b Tfit+orig -expr 'a-b' \
               -datum float -prefix Tresidual
      3dTproject should be MUCH more efficient, especially when using
      voxel-specific regressors (i.e., '-dsort'), and of course, it also
      offers internal generation of the bandpass/stopband regressors,
      as well as censoring, blurring, and L2-norming.
    
    * This version of the program is compiled using OpenMP for speed.
    
    * Authored by RWCox in a fit of excessive linear algebra [summer 2013].
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
