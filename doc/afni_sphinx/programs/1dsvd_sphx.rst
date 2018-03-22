*****
1dsvd
*****

.. _1dsvd:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dsvd [options] 1Dfile 1Dfile ...
    - Computes SVD of the matrix formed by the 1D file(s).
    - Output appears on stdout; to save it, use '>' redirection.
    
    OPTIONS:
     -one    = Make 1st vector be all 1's.
     -vmean  = Remove mean from each vector (can't be used with -one).
     -vnorm  = Make L2-norm of each vector = 1 before SVD.
               * The above 2 options mirror those in 3dpc.
     -cond   = Only print condition number (ratio of extremes)
     -sing   = Only print singular values
               * To compare the singular values from 1dsvd with those from
                 3dDeconvolve you must use the -vnorm option with 1dsvd.
                 For example, try
                   3dDeconvolve -nodata 200 1 -polort 5 -num_stimts 1 \
                                -stim_times 1 '1D: 30 130' 'BLOCK(50,1)' -singvals
                   1dsvd -sing -vnorm nodata.xmat.1D
     -sort   = Sort singular values (descending) [the default]
     -nosort = Don't bother to sort the singular values
     -asort  = Sort singular values (ascending)
     -1Dleft = Only output left eigenvectors, in a .1D format
               This might be useful for reducing the number of
               columns in a design matrix.  The singular values
               are printed at the top of each vector column,
               as a '#...' comment line.
     -nev n  = If -1Dleft is used, '-nev' specifies to output only
               the first 'n' eigenvectors, rather than all of them.
               * If you are a tricky person, such as Souheil, you can
                 put a '%' after the value, and then you are saying
                 keep eigenvectors until at least n% of the sum of
                 singular values is accounted for.  In this usage,
                 'n' must be a number less than 100; for example, to
                 reduce a matrix down to a smaller set of columns that
                 capture most of its column space, try something like
                   1dsvd -1Dleft -nev 99% Xorig.1D > X99.1D
    EXAMPLE:
     1dsvd -vmean -vnorm -1Dleft fred.1D'[1..6]' | 1dplot -stdin
    NOTES:
    * Call the input n X m matrix [A] (n rows, m columns).  The SVD
      is the factorization [A] = [U] [S] [V]' ('=transpose), where
      - [U] is an n x m matrix (whose columns are the 'Left vectors')
      - [S] is a diagonal m x m matrix (the 'singular values')
      - [V] is an m x m matrix (whose columns are the 'Right vectors')
    * The default output of the program is
      - An echo of the input [A]
      - The [U] matrix, each column headed by its singular value
      - The [V] matrix, each column headed by its singular value
        (please note that [V] is output, not [V]')
      - The pseudo-inverse of [A]
    * This program was written simply for some testing purposes,
      but is distributed with AFNI because it might be useful-ish.
    * Recall that you can transpose a .1D file on input by putting
      an escaped ' character after the filename.  For example,
        1dsvd fred.1D\'
      You can use this feature to get around the fact that there
      is no '-1Dright' option.  If you understand.
    * For more information on the SVD, you can start at
      http://en.wikipedia.org/wiki/Singular_value_decomposition
    * Author: Zhark the Algebraical (Linear).
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
