*****
2perm
*****

.. _2perm:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 2perm [-prefix PPP] [-comma] bot top [n1 n2]
    
    This program creates 2 random non-overlapping subsets of the set of
    integers from 'bot' to 'top' (inclusive).  The first subset is of
    length 'n1' and the second of length 'n2'.  If those values are not
    given, then equal size subsets of length (top-bot+1)/2 are used.
    
    This program is intended for use in various simulation and/or
    randomization scripts, or for amusement/hilarity.
    
    OPTIONS:
    ========
     -prefix PPP == Two output files are created, with names PPP_A and PPP_B,
                    where 'PPP' is the given prefix.  If no '-prefix' option
                    is given, then the string 'AFNIroolz' will be used.
                    ++ Each file is a single column of numbers.
                    ++ Note that the filenames do NOT end in '.1D'.
    
     -comma      == Write each file as a single row of comma-separated numbers.
    
    EXAMPLE:
    ========
    This illustration shows the purpose of 2perm -- for use in permutation
    and/or randomization tests of statistical significance and power.
    Given a dataset with 100 sub-bricks (indexed 0..99), split it into two
    random halves and do a 2-sample t-test between them.
    
      2perm -prefix Q50 0 99
      3dttest++ -setA dataset+orig"[1dcat Q50_A]" \
                -setB dataset+orig"[1dcat Q50_B]" \
                -no1sam -prefix Q50
      \rm -f Q50_?
    
    Alternatively:
    
      2perm -prefix Q50 -comma 0 99
      3dttest++ -setA dataset+orig"[`cat Q50_A`]" \
                -setB dataset+orig"[`cat Q50_B`]" \
                -no1sam -prefix Q50
      \rm -f Q50_?
    
    Note the combined use of the double quote " and backward quote `
    shell operators in this second approach.
    
    AUTHOR: (no one want to admit they wrote this trivial code).
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
