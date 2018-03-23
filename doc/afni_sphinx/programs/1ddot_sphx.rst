.. _ahelp_1ddot:

*****
1ddot
*****

.. contents:: 
    :depth: 4 

| 

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
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
