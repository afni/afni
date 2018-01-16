******
3dHist
******

.. _3dHist:

.. contents:: 
    :depth: 4 

.. code-block:: none

    3dHist computes histograms using functions for generating priors.
    If you are not sure you need this particular program, use 3dhistog instead.
    
    Example:
    3dHist      -input sigs+orig \n               
    
    Options:
       -input DSET: Dset providing values for histogram. Exact 0s are not counted
       -dind SB: Use sub-brick SB from the input rather than 0
       -mask MSET: Provide mask dataset to select subset of input.
       -mask_range BOT TOP: Specify the range of values to consider from MSET.
                            Default is anything non-zero
       -cmask CMASK: Provide cmask expression. Voxels where expression is 0
                     are excluded from computations. For example:
                -cmask '-a T1.div.r+orig -b T1.uni.r+orig -expr step(a/b-10)'
       -thishist HIST.niml.hist: Read this previously created histogram instead
                                 of forming one from DSET.
                                 Obviously, DSET, or -mask options are not needed
       -prefix PREF: Write histogram to niml file called PREF.niml.hist 
       -equalized PREF: Write a histogram equalized version of the input dataset
       Histogram Creation Parameters:
         By default, the program will select bin number, bin width, 
         and range automatically. You can also set the parameters manually with 
         the following options.
       -nbin K: Use K bins.
       -min MIN: Minimum intensity.
       -max MAX: Maximum intensity.
       -binwidth BW: Bin width
       -ignore_out: Do not count samples outside the user specified range.
       -rhist RHIST.niml.hist: Use previously created histogram to set range
                               and binwidth parameters.
    
       -showhist: Display histogram to stdout
                  You can also graph it with: 1dRplot HistOut.niml.hist
    
       Histogram Queries:
       -at VAL: Set the value at which you want histogram values
       -get 'PAR1,PAR2,PAR3..': Return the following PAR* properties at VAL
                                Choose from:
                                freq: Frequency (normalized count)
                                count: Count
                                bin: Continuous bin location estimate
                                cdf: Cumulative count
                                rcdf: Reverse cumulative count (from the top)
                                ncdf: The normalized version of cdf
                                nrcdf: The reverse version of ncdf
                                outl: 1.0-(2*smallest tail area)
                                   0 means VAL splits area in the middle
                                   1 means VAL is at either end of the histogram
                                ALL: All the above.
                       You can select multiple ones with something like:
                            -get 'freq, count, bin' 
    
                       You can also set one of the PAR* to 'upvol' to get 
                       the volume (liters) of voxels with values exceeding VAL
                       The use of upvol usually requires option -voxvol too.
      -voxvol VOL_MM3: A voxel's volume in mm^3. To be used with upvol if
                       no dataset is available or if you want to override
                       it.
      -val_at PAR PARVAL: Return the value (magnitude) where histogram property
                          PAR is equal to PARVAL
                          PAR can only be one of: cdf, rcdf, ncdf, nrcdf, upvol
                          For upvol, PARVAL is in Liters
      -quiet: Return a concise output to simplify parsing. For the moment, this
              option only affects output of option -val_at
    
      Examples:
           #A histogram a la 3dhistog:
           3dHist -input T1+orig.
           #Getting parameters from previously created histogram:
           3dHist -thishist HistOut.niml.hist -at 144.142700 
           #Or the reverse query:
           3dHist -thishist HistOut.niml.hist -val_at ncdf 0.132564
           #Compute histogram and find dataset threshold (approximate)
           #such that 1.5 liters of voxels remain above it.
           3dHist -prefix toy -input flair_axial.nii.gz -val_at upvol 1.5 
    
    
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
