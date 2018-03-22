******
from3d
******

.. _ahelp_from3d:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage:   from3d [options] -input fname -prefix rname
    Purpose: Extract 2D image files from a 3D AFNI dataset.
    Options:
    -v             Print out verbose information during the run.
    -nsize         Adjust size of 2D data file to be NxN, by padding
                     with zeros, where N is a power of 2.
    -raw           Write images in 'raw' format (just the data bytes)
                     N.B.: there will be no header information saying
                           what the image dimensions are - you'll have
                           to get that information from the x and y
                           axis information output by 3dinfo.
    -float         Write images as floats, no matter what they are in
                     the dataset itself.
    -zfirst num    Set 'num' = number of first z slice to be extracted.
                     (default = 1)
    -zlast num     Set 'num' = number of last z slice to be extracted.
                     (default = largest)
    -tfirst num    Set 'num' = number of first time slice to be extracted.
                     (default = 1)
    -tlast num     Set 'num' = number of last time slice to be extracted.
                     (default = largest)
    -input fname   Read 3D dataset from file 'fname'.
                     'fname' may include a sub-brick selector list.
    -prefix rname  Write 2D images using prefix 'rname'.
    
                   (-input and -prefix are non-optional options: they)
                   (must be present or the program will not execute. )
    
    N.B.: * Image data is extracted directly from the dataset bricks.
             If a brick has a floating point scaling factor, it will NOT
             be applied.
          * Images are extracted parallel to the xy-plane of the dataset
             orientation (which can be determined by program 3dinfo).
             This is the order in which the images were input to the
             dataset originally, via to3d.
          * If either of these conditions is unacceptable, you can also
