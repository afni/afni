***************
3dAFNItoANALYZE
***************

.. _3dAFNItoANALYZE:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Usage: 3dAFNItoANALYZE [-4D] [-orient code] aname dset
    Writes AFNI dataset 'dset' to 1 or more ANALYZE 7.5 format
    .hdr/.img file pairs (one pair for each sub-brick in the
    AFNI dataset).  The ANALYZE files will be named
      aname_0000.hdr aname_0000.img   for sub-brick #0
      aname_0001.hdr aname_0001.img   for sub-brick #1
    and so forth.  Each file pair will contain a single 3D array.
    
    * If the AFNI dataset does not include sub-brick scale
      factors, then the ANALYZE files will be written in the
      datum type of the AFNI dataset.
    * If the AFNI dataset does have sub-brick scale factors,
      then each sub-brick will be scaled to floating format
      and the ANALYZE files will be written as floats.
    * The .hdr and .img files are written in the native byte
      order of the computer on which this program is executed.
    
    Options
    -------
    -4D [30 Sep 2002]:
     If you use this option, then all the data will be written to
     one big ANALYZE file pair named aname.hdr/aname.img, rather
     than a series of 3D files.  Even if you only have 1 sub-brick,
     you may prefer this option, since the filenames won't have
     the '_0000' appended to 'aname'.
    
    -orient code [19 Mar 2003]:
     This option lets you flip the dataset to a different orientation
     when it is written to the ANALYZE files.  The orientation code is
     formed as follows:
       The code must be 3 letters, one each from the
       pairs {R,L} {A,P} {I,S}.  The first letter gives
       the orientation of the x-axis, the second the
       orientation of the y-axis, the third the z-axis:
          R = Right-to-Left          L = Left-to-Right
          A = Anterior-to-Posterior  P = Posterior-to-Anterior
          I = Inferior-to-Superior   S = Superior-to-Inferior
       For example, 'LPI' means
          -x = Left       +x = Right
          -y = Posterior  +y = Anterior
          -z = Inferior   +z = Superior
     * For display in SPM, 'LPI' or 'RPI' seem to work OK.
        Be careful with this: you don't want to confuse L and R
        in the SPM display!
     * If you DON'T use this option, the dataset will be written
        out in the orientation in which it is stored in AFNI
        (e.g., the output of '3dinfo dset' will tell you this.)
     * The dataset orientation is NOT stored in the .hdr file.
     * AFNI and ANALYZE data are stored in files with the x-axis
        varying most rapidly and the z-axis most slowly.
     * Note that if you read an ANALYZE dataset into AFNI for
        display, AFNI assumes the LPI orientation, unless you
        set environment variable AFNI_ANALYZE_ORIENT.
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
