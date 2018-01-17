*************
3dMultiThresh
*************

.. _3dMultiThresh:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Program to apply a multi-threshold (mthresh) dataset
    to an input dataset.
    
    Usage:
      3dMultiThresh OPTIONS
    
    OPTIONS (in any order)
    ----------------------
    
     -mthresh mmm    = Multi-threshold dataset from 3dXClustSim
                       (usually via running '3dttest++ -ETAC').
     -input   ddd    = Dataset to threshold.
     -1tindex iii    = Index (sub-brick) on which to threshold.
     -signed  +/-    = If the .mthresh.nii file from 3dXClustSim
                       was created using 1-sided thresholding,
                       this option tells which sign to choose when
                       doing voxel-wise thresholding: + or -.
                      ++ If the .mthresh.nii file was created using
                         2-sided thresholding, this option is ignored.
     -pos            = Same as '-signed +'
     -neg            = Same as '-signed -'
     -prefix  ppp    = prefix for output dataset
                      ++ Can be 'NULL' to get no output dataset
     -maskonly       = Instead of outputing a thresholded version
                       of the input dataset, just output a 0/1 mask
                       dataset of voxels that survive the process.
     -allmask qqq    = Write out a multi-volume dataset with prefix 'qqq'
                       where each volume is the binary mask of voxels that
                       pass ONE of the tests. This dataset can be used
                       to see which tests mattered where in the brain.
                      ++ To be more clear, there will be one sub-brick for
                         each p-value threshold coded in the '-mthresh'
                         dataset (e.g., p=0.0100 and p=0.0001).
                      ++ In each sub-brick, the value will be between
                         0 and 7, and is the sum of these:
                            1 == hpow=0 was declared 'active'
                            2 == hpow=1 was declared 'active'
                            4 == hpow=2 was declared 'active'
                         Of course, an hpow value will only be tested
                         if it is so encoded in the '-mthresh' dataset.
     -nozero         = This option prevents the output of a
                       dataset if it would be all zero
     -quiet          = Turn off progress report messages
    
    The number of surviving voxels will be written to stdout.
    It can be captured in a csh script by a command such as
       set nhits = `3dMultiThresh OPTIONS`
    
    Meant to be used in conjunction with program 3dXClustSim,
    which is in turn meant to be used with program 3dttest++ -- RWCox
