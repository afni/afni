.. _ahelp_3dEmpty:

*******
3dEmpty
*******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dEmpty [options]
    Makes an 'empty' dataset .HEAD file.
    
    Options:
    =======
     -prefix p   = Prefix name for output file (default = 'Empty')
    
     -nxyz x y z = Set number of voxels to be 'x', 'y', and 'z'
                     along the 3 axes [defaults=64]
       *OR*
    
     -geometry m = Set the 3D geometry of the grid using a
                   string 'm' of the form
              'MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34):nx,ny,nz'
                   which defines the number of grid points, as well as
                   relationship between grid indexes (voxel centers)
                   and the 3D xyz coordinates.
                 * Sample 'MATRIX()' entries can be found by using
                   program 3dinfo on an existing datasets.
                 * Each .niml file used by 3dGroupInCorr has a
                   'geometry="MATRIX(...)" entry.
    
     -nt         = Number of time points [default=1]
    
    * Other dataset parameters can be changed with 3drefit.
    * The purpose of this program (combined with 3drefit) is to
      allow you to make up an AFNI header for an existing data file.
    * This program does NOT create data to fill up the dataset.
    * If you want to create a dataset of a given size with random
      values attached, a command like
        3dcalc -a jRandomDataset:32,32,16,10 -expr a -prefix Something
      would work. In this example, nx=ny=32 nz=16 nt=10.
      (Changing '-expr a' to '-expr 0' would fill the dataset with zeros.)
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
