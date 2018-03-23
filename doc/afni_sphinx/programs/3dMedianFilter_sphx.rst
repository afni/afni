.. _ahelp_3dMedianFilter:

**************
3dMedianFilter
**************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dMedianFilter [options] dataset
    Computes the median in a spherical nbhd around each point in the
    input to produce the output.
    
    Options:
      -irad x    = Radius in voxels of spherical regions
      -iter n    = Iterate 'n' times [default=1]
      -verb      = Be verbose during run
      -prefix pp = Use 'pp' for prefix of output dataset
      -automask  = Create a mask (a la 3dAutomask)
    
    Output dataset is always stored in float format.  If the input
    dataset has more than 1 sub-brick, only sub-brick #0 is processed.
    
    -- Feb 2005 - RWCox
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
