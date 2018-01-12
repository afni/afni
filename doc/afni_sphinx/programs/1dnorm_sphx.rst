.. contents:: 
    :depth: 4 

******
1dnorm
******

.. code-block:: none

    Usage: 1dnorm [options] infile outfile
    where infile is an AFNI *.1D file (ASCII list of numbers arranged
    in columns); outfile will be a similar file, with each column being
    L_2 normalized (sum of squares = 1).
    * If 'infile'  is '-', it will be read from stdin.
    * If 'outfile' is '-', it will be written to stdout.
    
    Options:
    --------
     -norm1  = Normalize so sum of absolute values is 1 (L_1 norm)
     -normx  = So that max absolute value is 1 (L_infinity norm)
    
     -demean = Subtract each column's mean before normalizing
     -demed  = Subtract each column's median before normalizing
                [-demean and -demed are mutually exclusive!]
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
