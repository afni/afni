.. _ahelp_@2dwarper.Allin:

***************
@2dwarper.Allin
***************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

     script to do 2D registration on each slice of a 3D+time
     dataset, and glue the results back together at the end
    
     This script is structured to operate only on an AFNI
     +orig.HEAD dataset.  The one input on the command line
     is the prefix for the dataset.
    
     Modified 07 Dec 2010 by RWC to use 3dAllineate instead
     of 3dWarpDrive, with nonlinear slice-wise warping.
    
     Set prefix of input 3D+time dataset here.
     In this example with 'wilma' as the command line
     argument, the output dataset will be 'wilma_reg+orig'.
     The output registration parameters files will
     be 'wilma_param_ssss.1D', where 'ssss' is the slice number.
    
     usage: @2dwarper.Allin [options] INPUT_PREFIX
    
        example: @2dwarper.Allin epi_run1
        example: @2dwarper.Allin -mask my_mask epi_run1
    
     options:
        -mask   MSET    : provide the prefix of an existing mask dataset
        -prefix PREFIX  : provide the prefix for output datasets
