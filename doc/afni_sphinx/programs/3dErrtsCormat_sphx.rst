*************
3dErrtsCormat
*************

.. _ahelp_3dErrtsCormat:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dErrtsCormat [options] dset
    
    Computes the correlation (not covariance) matrix corresponding
    to the residual (or error) time series in 'dset', which will
    usually be the '-errts' output from 3dDeconvolve.  The output
    is a 1D file of the Toeplitz entries (to stdout).
    
    Options:
      -concat rname  = as in 3dDeconvolve
      -input  dset   = alternate way of telling what dataset to read
      -mask   mset   = mask dataset
      -maxlag mm     = set maximum lag
      -polort pp     = set polort level (default=0)
    
    -- RWCox -- June 2008 -- for my own pleasant purposes
    -- Also see program 3dLocalCormat to do this on each voxel,
       and optionally estimate the ARMA(1,1) model parameters.
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
