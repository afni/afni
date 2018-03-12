**********
3dLocalACF
**********

.. _3dLocalACF:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: 3dLocalACF [options] inputdataset
    
    Options:
    --------
     -prefix   ppp
     -input    inputdataset
     -nbhd     nnn
     -mask     maskdataset
     -automask
    
    Notes:
    ------
    * This program estimates the spatial AutoCorrelation Function (ACF)
      locally in a neighborhood around each voxel, unlike '3FWHMx -acf',
      which produces an average over the whole volume.
    
    * The input dataset must be a time series dataset, and must have
      been detrended, despiked, etc. already.  The 'errts' output from
      afni_proc.py is recommended!
    
    * A brain mask is highly recommended as well.
    
    * I typically use 'SPHERE(25)' for the neighborhood.  YMMV.
    
    * This program is very slow.
       This copy of it uses multiple threads (OpenMP), so it is
       somewhat tolerable to use.
    
    ***** This program is experimental *****
    
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
