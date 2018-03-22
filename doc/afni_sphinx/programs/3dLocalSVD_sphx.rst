**********
3dLocalSVD
**********

.. _ahelp_3dLocalSVD:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dLocalSVD [options] inputdataset
    * You may want to use 3dDetrend before running this program,
       or at least use the '-polort' option.
    * This program is highly experimental.  And slowish.
    * Computes the SVD of the time series from a neighborhood of each
       voxel.  An inricate way of 'smoothing' 3D+time datasets,
       in some sense, maybe.
    * For most purposes, program 3dLocalPV does the same thing, but faster.
       The only reason to use 3dLocalSVD is if you are using -vproj
       with the projection dimension ndim > 2.
    
    Options:
     -mask mset           = restrict operations to this mask
     -automask            = create a mask from time series dataset
     -prefix ppp          = save SVD vector result into this new dataset
     -input inputdataset  = input time series dataset
     -nbhd nnn            = e.g., 'SPHERE(5)' 'TOHD(7)' etc.
     -polort p [+]        = detrending ['+' means to add trend back]
     -vnorm               = normalize data vectors
                             [strongly recommended]
     -vproj [ndim]        = project central data time series onto local SVD subspace
                             of dimension 'ndim'
                             [default: just output principal singular vector]
                             [for 'smoothing' purposes, '-vnorm -vproj 2' is a good idea]
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
