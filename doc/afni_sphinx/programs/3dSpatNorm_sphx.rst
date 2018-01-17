**********
3dSpatNorm
**********

.. _3dSpatNorm:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dSpatNorm [options] dataset
    
    Options:
      -prefix ppp = Write output dataset using 'ppp' for the prefix.
      -orig_space = Write output dataset using the same grid as dataset.
      -verb       = Write out progress reports
      -monkey : Monkey business
      -marmost: Marmoset head
      -rat: Rat head
      -human: Bone head (default)
      -bottom_cuts CUTFLAGS = Make approximate cuts at the bottom to shave
                              non brain areas. CUTFLAGS is a string of 
                              characters indicating which sides to cut.
                              An 'A' cuts along the anterior side
                              'P' for posterior, and 'R', 'L' for right, and
                              left, respectively.
                              To cut all four, use: -bottom_cuts APLR
                        Note: -bottom_cuts only works for Human heads.
    --------------------------------------------------------------------
    * This program is obsolete, and should not be used by most people. *
    --------------------------------------------------------------------
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
