.. contents:: 
    :depth: 4 

*****
1ddot
*****

.. code-block:: none

    Usage: 1ddot [options] 1Dfile 1Dfile ...
    * Prints out correlation matrix of the 1D files and
      their inverse correlation matrix.
    * Output appears on stdout.
    * Program 1dCorrelate does something similar-ish.
    
    Options:
     -one  =  Make 1st vector be all 1's.
     -dem  =  Remove mean from all vectors (conflicts with '-one')
     -cov  =  Compute with covariance matrix instead of correlation
     -inn  =  Computed with inner product matrix instead
     -rank =  Compute Spearman rank correlation instead
              (also implies '-terse')
     -terse=  Output only the correlation or covariance matrix
              and without any of the garnish. 
     -okzero= Do not quit if a vector is all zeros.
              The correlation matrix will have 0 where NaNs ought to go.
              Expect rubbish in the inverse matrices if all zero 
              vectors exist.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
