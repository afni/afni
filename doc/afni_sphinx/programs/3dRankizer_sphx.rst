**********
3dRankizer
**********

.. _ahelp_3dRankizer:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dRankizer [options] dataset
    Output = Rank of each voxel as sorted into increasing value.
             - Ties get the average rank.
             - Not the same as 3dRank!
             - Only sub-brick #0 is processed at this time!
             - Ranks start at 1 and increase:
                 Input  = 0   3   4   4   7   9
                 Output = 1   2   3.5 3.5 5   6
    Options:
      -brank bbb   Set the 'base' rank to 'bbb' instead of 1.
                     (You could also do this with 3dcalc.)
      -mask mset   Means to use the dataset 'mset' as a mask:
                     Only voxels with nonzero values in 'mset'
                     will be used from 'dataset'.  Voxels outside
                     the mask will get rank 0.
      -prefix ppp  Write results into float-format dataset 'ppp'
                     Output is in float format to allow for
                     non-integer ranks resulting from ties.
      -percentize : Divide rank by the number of voxels in the dataset x 100.0 
      -percentize_mask : Divide rank by the number of voxels in the mask x 100.0 
    
    Author: RW Cox  [[a quick hack for his own purposes]]
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
