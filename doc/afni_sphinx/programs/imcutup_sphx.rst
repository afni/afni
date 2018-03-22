*******
imcutup
*******

.. _ahelp_imcutup:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: imcutup [options] nx ny fname1
    Breaks up larger images into smaller image files of size
    nx by ny pixels.  Intended as an aid to using image files
    which have been catenated to make one big 2D image.
    OPTIONS:
      -prefix ppp = Prefix the output files with string 'ppp'
      -xynum      = Number the output images in x-first, then y [default]
      -yxnum      = Number the output images in y-first, then x
      -x.ynum     = 2D numbering, x.y format
      -y.xnum     = 2D numbering, y.x format
    For example:
      imcutup -prefix Fred 64 64 3D:-1:0:256:128:1:zork.im
    will break up the big 256 by 128 image in file zork.im
    into 8 images, each 64 by 64.  The output filenames would be
      -xynum  => Fred.001 Fred.002 Fred.003 Fred.004
                 Fred.005 Fred.006 Fred.007 Fred.008
    
      -yxnum  => Fred.001 Fred.003 Fred.005 Fred.007
                 Fred.002 Fred.004 Fred.006 Fred.008
    
      -x.ynum => Fred.001.001 Fred.002.001 Fred.003.001 Fred.004.001
                 Fred.001.002 Fred.002.002 Fred.003.002 Fred.004.002
    
      -y.xnum => Fred.001.001 Fred.001.002 Fred.001.003 Fred.001.004
                 Fred.002.001 Fred.002.002 Fred.002.003 Fred.002.004
    
    You may want to look at the input image file with
      afni -im fname  [then open the Sagittal image window]
    before deciding on what to do with the image file.
    
    N.B.: the file specification 'fname' must result in a single
          input 2D image - multiple images can't be cut up in one
