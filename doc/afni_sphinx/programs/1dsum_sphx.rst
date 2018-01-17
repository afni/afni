*****
1dsum
*****

.. _1dsum:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 1dsum [options] a.1D b.1D ...
    where each file a.1D, b.1D, etc. is an ASCII file of numbers arranged
    in rows and columns. The sum of each column is written to stdout.
    
    Options:
      -ignore nn = skip the first nn rows of each file
      -use    mm = use only mm rows from each file
      -mean      = compute the average instead of the sum
      -nocomment = the # comments from the header of the first
                   input file will be reproduced to the output;
                   if you do NOT want this to happen, use the
                   '-nocomment' option.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
