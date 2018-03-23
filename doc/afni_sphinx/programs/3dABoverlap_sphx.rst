.. _ahelp_3dABoverlap:

***********
3dABoverlap
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dABoverlap [options] A B
    Output (to screen) is a count of various things about how
    the automasks of datasets A and B overlap or don't overlap.
    
    * Dataset B will be resampled to match dataset A, if necessary,
       which will be slow if A is high resolution.  In such a case,
       you should only use one sub-brick from dataset B.
      ++ The resampling of B is done before the automask is generated.
    * The values output are labeled thusly:
        #A         = number of voxels in the A mask
        #B         = number of voxels in the B mask
        #(A uni B) = number of voxels in the either or both masks (set union)
        #(A int B) = number of voxels present in BOTH masks (set intesection)
        #(A \ B)   = number of voxels in A mask that aren't in B mask
        #(B \ A)   = number of voxels in B mask that arent' in A mask
        %(A \ B)   = percentage of voxels from A mask that aren't in B mask
        %(B \ A)   = percentage of voxels from B mask that aren't in A mask
        Rx(B/A)    = radius of gyration of B mask / A mask, in x direction
        Ry(B/A)    = radius of gyration of B mask / A mask, in y direction
        Rz(B/A)    = radius of gyration of B mask / A mask, in z direction
    * If B is an EPI dataset sub-brick, and A is a skull stripped anatomical
       dataset, then %(B \ A) might be useful for assessing if the EPI
       brick B is grossly misaligned with respect to the anatomical brick A.
    * The radius of gyration ratios might be useful for determining if one
       dataset is grossly larger or smaller than the other.
    
    OPTIONS
    -------
     -no_automask = consider input datasets as masks
                    (automask does not work on mask datasets)
     -quiet = be as quiet as possible (without being entirely mute)
     -verb  = print out some progress reports (to stderr)
    
    NOTES
    -----
     * If an input dataset is comprised of bytes and contains only one
       sub-brick, then this program assumes it is already an automask-
       generated dataset and the automask operation will be skipped.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
