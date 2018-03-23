.. _ahelp_3dAFNItoMINC:

************
3dAFNItoMINC
************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAFNItoMINC [options] dataset
    Reads in an AFNI dataset, and writes it out as a MINC file.
    
    OPTIONS:
     -prefix ppp  = Write result into file ppp.mnc;
                      default prefix is same as AFNI dataset's.
     -floatize    = Write MINC file in float format.
     -swap        = Swap bytes when passing data to rawtominc
    
    NOTES:
    * Multi-brick datasets are written as 4D (x,y,z,t) MINC
       files.
    * If the dataset has complex-valued sub-bricks, then this
       program won't write the MINC file.
    * If any of the sub-bricks have floating point scale
       factors attached, then the output will be in float
       format (regardless of the presence of -floatize).
    * This program uses the MNI program 'rawtominc' to create
       the MINC file; rawtominc must be in your path.  If you
       don't have rawtominc, you must install the MINC tools
       software package from MNI.  (But if you don't have the
       MINC tools already, why do you want to convert to MINC
       format anyway?)
    * At this time, you can find the MINC tools at
         ftp://ftp.bic.mni.mcgill.ca/pub/minc/
       You need the latest version of minc-*.tar.gz and also
       of netcdf-*.tar.gz.
    
    -- RWCox - April 2002
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
